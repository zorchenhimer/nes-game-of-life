.include "nes2header.inc"
nes2mapper 0
nes2prg 16 * 1024
nes2chr 1 * 8 * 1024
nes2chrram 0
nes2wram 0
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers

; Button Constants
BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "ZEROPAGE"
Sleeping: .res 1
Controller: .res 1
Controller_Old: .res 1
Controller_Pressed: .res 1

TmpA: .res 1
TmpB: .res 1
TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

TableSelect: .res 1

ptrTable: .res 2
ptrRow: .res 2

ptrCurrent: .res 2
ptrNext: .res 2
ptrCell: .res 2
ptrInspect: .res 2

Neighbors: .res 1

CoordX: .res 1
CoordY: .res 1
CurrentAlive: .res 1

; X/Y coords of cells around the
; cell being inspected
;
; 012
; 3.4
; 567
NeighborsX: .res 8
NeighborsY: .res 8

Tick: .res 1
UpdateReady: .res 1

SwapReady:  .res 1
BufferAddr: .res 2
TileBuffer: .res 32

.segment "OAM"
.segment "BSS"

SmTableA: .res 120
SmTableB: .res 120

AttrBuffer: .res 64

.segment "CHR0"
.incbin "images/main.chr"

.segment "PAGE0"

Palettes:
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00

    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00

CellMasks:
    .repeat 8, i
    .byte 1 << (7-i)
    .endrepeat

CellMasksInvert:
    .repeat 8, i
    .byte (1 << (7-i)) ^ $FF
    .endrepeat

PpuRows:
    .repeat 32, i
    .word $2000+(i*32)
    .endrepeat

ReadControllers:
    lda Controller
    sta Controller_Old

    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldx #$08
@player1:
    lda $4016
    lsr A           ; Bit0 -> Carry
    rol Controller  ; Bit0 <- Carry
    dex
    bne @player1

    lda Controller_Old  ; 0001
    eor #$FF            ; 1110
    and Controller      ; 0000
    sta Controller_Pressed ; 0000
    rts

WaitForNMI:
    lda #0
    sta Sleeping
:   bit Sleeping
    bpl :-
    rts

IRQ:
    rti

NMI:
    pha
    txa
    pha
    tya
    pha

    lda #$FF
    sta Sleeping

    lda BufferAddr+1
    bne :+
    jmp @noBuffer
:
    sta $2006
    lda BufferAddr+0
    sta $2006

    .repeat 32, i
    lda TileBuffer+i
    sta $2007
    .endrepeat

@noBuffer:

    lda #0
    sta BufferAddr+0
    sta BufferAddr+1

    lda SwapReady
    beq :+
    lda #0
    sta SwapReady
    lda TableSelect
    eor #$FF
    sta TableSelect
:

    lda TableSelect
    bne @tableB
    lda #$88
    sta $2000
    jmp @selectDone
@tableB:
    lda #$89
    sta $2000
@selectDone:

    lda #0
    sta $2005
    sta $2005

    pla
    tay
    pla
    tax
    pla
    rti

RESET:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

    ldx #$40
    stx $4017   ; Disable APU frame IRQ

    ldx #$FF
    txs         ; Setup new stack

    inx         ; Now X = 0

    stx $2000   ; disable NMI
    stx $2001   ; disable rendering
    stx $4010   ; disable DMC IRQs

:   ; First wait for VBlank to make sure PPU is ready.
    bit $2002   ; test this bit with ACC
    bpl :- ; Branch on result plus

:   ; Clear RAM
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x

    inx
    bne :-  ; loop if != 0

:   ; Second wait for vblank.  PPU is ready after this
    bit $2002
    bpl :-

    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    .repeat 8*4, i
    lda Palettes+i
    sta $2007
    .endrepeat

; clear attribtutes
    lda #$23
    sta $2006
    lda #$C0
    sta $2006
    lda #0
    ldx #64
:
    sta $2006
    dex
    bne :-

    lda #$27
    sta $2006
    lda #$C0
    sta $2006
    lda #0
    ldx #64
:
    sta $2006
    dex
    bne :-

; initial state
; ..X
; X.X
; .XX
    lda TableSelect
    beq :+
    lda #.lobyte(SmTableA)
    sta ptrCurrent+0
    lda #.hibyte(SmTableA)
    sta ptrCurrent+1

    lda #.lobyte(SmTableB)
    sta ptrNext+0
    lda #.hibyte(SmTableB)
    sta ptrNext+1
    jmp :++
:
    lda #.lobyte(SmTableA)
    sta ptrNext+0
    lda #.hibyte(SmTableA)
    sta ptrNext+1

    lda #.lobyte(SmTableB)
    sta ptrCurrent+0
    lda #.hibyte(SmTableB)
    sta ptrCurrent+1
:

    lda #1
    sta CurrentAlive

    ldx #2
    ldy #0
    jsr SetCell

    ldx #2
    ldy #1
    jsr SetCell

    ldx #2
    ldy #2
    jsr SetCell

    ldx #1
    ldy #2
    jsr SetCell

    ldx #0
    ldy #1
    jsr SetCell

    lda #$88
    sta $2000

    lda #$0A
    sta $2001

    lda #0
    sta CoordX
    sta CoordY

ResetFrame:
    jsr SmUpdate
    jsr SmBuffer
    jsr WaitForNMI
    jmp ResetFrame

SmUpdate:
    lda TableSelect
    beq :+
    lda #.lobyte(SmTableB)
    sta ptrCurrent+0
    lda #.hibyte(SmTableB)
    sta ptrCurrent+1

    lda #.lobyte(SmTableA)
    sta ptrNext+0
    lda #.hibyte(SmTableA)
    sta ptrNext+1
    jmp :++
:
    lda #.lobyte(SmTableB)
    sta ptrNext+0
    lda #.hibyte(SmTableB)
    sta ptrNext+1

    lda #.lobyte(SmTableA)
    sta ptrCurrent+0
    lda #.hibyte(SmTableA)
    sta ptrCurrent+1
:


@chunkLoop:
    lda #0
    sta CurrentAlive

    ldx CoordX
    ldy CoordY
    jsr GetCell ; returns non-zero if cell is alive.
    beq :+
    lda #1
    sta CurrentAlive
:

    ldx CoordY
    beq @wrapAbove
    dex
    stx NeighborsY+0
    stx NeighborsY+1
    stx NeighborsY+2
    jmp @aboveDone
@wrapAbove:
    lda #29
    sta NeighborsY+0
    sta NeighborsY+1
    sta NeighborsY+2
@aboveDone:

    ; middle row
    lda CoordY
    sta NeighborsY+3
    sta NeighborsY+4

    ldx CoordY
    cpx #29
    beq @wrapBelow
    inx
    stx NeighborsY+5
    stx NeighborsY+6
    stx NeighborsY+7
    jmp @belowDone
@wrapBelow:
    ldx #0
    stx NeighborsY+5
    stx NeighborsY+6
    stx NeighborsY+7
@belowDone:

    ldx CoordX
    beq @wrapLeft
    dex
    stx NeighborsX+0
    stx NeighborsX+3
    stx NeighborsX+5
    jmp @leftDone
@wrapLeft:
    ldx #31
    stx NeighborsX+0
    stx NeighborsX+3
    stx NeighborsX+5
@leftDone:

    ; middle column
    lda CoordX
    sta NeighborsX+1
    sta NeighborsX+6

    ldx CoordX
    cpx #31
    beq @wrapRight
    inx
    stx NeighborsX+2
    stx NeighborsX+4
    stx NeighborsX+7
    jmp @rightDone
@wrapRight:
    ldx #0
    stx NeighborsX+2
    stx NeighborsX+4
    stx NeighborsX+7
@rightDone:

    ldx #0
    stx Neighbors
    stx TmpZ
@neighborLoop:
    ldx TmpZ
    ldy NeighborsY, x
    sty TmpY

    ldx TmpZ
    ldy NeighborsX, x
    tya
    tax
    ldy TmpY

    jsr GetCell
    beq :+
    inc Neighbors
:
    inc TmpZ
    lda TmpZ
    cmp #8
    bne @neighborLoop

    ; cell modifications
    lda CurrentAlive
    beq @dead

    lda Neighbors
    cmp #2
    bcs :+
    ; kill
    lda #0
    sta CurrentAlive
    ldy CoordY
    ldx CoordX
    jsr SetCell
    jmp @cellDone
:

    cmp #4
    bcc :+
    ; kill
    lda #0
    sta CurrentAlive
    ldy CoordY
    ldx CoordX
    jsr SetCell
    jmp @cellDone
:

    ; no change
    ldy CoordY
    ldx CoordX
    jsr SetCell

@dead:
    lda Neighbors
    cmp #3
    bne :+
    ; revive
    lda #1
    sta CurrentAlive
    ldy CoordY
    ldx CoordX
    jsr SetCell
    jmp @cellDone
:

    ; no change
    ldy CoordY
    ldx CoordX
    jsr SetCell

@cellDone:
    inc CoordX
    lda CoordX
    cmp #32
    bne @noX
    lda #0
    sta CoordX
    inc CoordY
    lda CoordY
    cmp #30
    bne :+
    lda #0
    sta CoordY
:   jmp @exitLoop
@noX:
    jmp @chunkLoop

@exitLoop:
    rts

; X&Y coordinates in X&Y registers
GetCell:
    ;lda CoordY ; CoordY*4
    tya
    asl a
    asl a
    tay ; start of row offset from beginning of table

    ;lda CoordX ; CoordX/8
    txa
    lsr a
    lsr a
    lsr a
    sta TmpX
    tya
    clc
    adc TmpX ; Offset of byte for current cell
    tay

    ;lda CoorX
    txa
    and #$07 ; %0000_0111
    tax

    lda (ptrCurrent), y
    and CellMasks, x
    rts

; X&Y coordinates in X&Y registers
SetCell:
    ;lda CoordY ; CoordY*4
    tya
    asl a
    asl a
    tay ; start of row offset from beginning of table

    ;lda CoordX ; CoordX/8
    txa
    lsr a
    lsr a
    lsr a
    sta TmpX
    tya
    clc
    adc TmpX ; Offset of byte for current cell
    tay

    ;lda CoorX
    txa
    and #$07 ; %0000_0111
    tax


    lda CurrentAlive
    beq @dead

    lda CellMasks, x
    ora (ptrNext), y
    jmp @set

@dead:
    lda CellMasksInvert, x
    and (ptrNext), y

@set:
    sta (ptrNext), y
    rts

TileON  = $01
TileOFF = $00
SmBuffer:
    ; loop over prev row
    ldy CoordY
    dey
    bpl :+
    ldy #29
:

    lda #0
    sta TmpA
    sty TmpB

    tya
    asl a
    tay

    lda PpuRows+0, y
    sta BufferAddr+0
    lda PpuRows+1, y
    sta BufferAddr+1

    lda TableSelect
    bne :+
    clc
    lda BufferAddr+1
    adc #$04
    sta BufferAddr+1

    lda BufferAddr+0
    adc #0
    sta BufferAddr+0
:

    lda TableSelect
    beq :+
    lda #.lobyte(SmTableA)
    sta ptrCurrent+0
    lda #.hibyte(SmTableA)
    sta ptrCurrent+1
    jmp :++
:
    lda #.lobyte(SmTableB)
    sta ptrCurrent+0
    lda #.hibyte(SmTableB)
    sta ptrCurrent+1
:

@loop:
    ldx TmpA
    ldy TmpB
    jsr GetCell
    beq @off
    lda #TileON
    jmp @set
@off:
    lda #TileOFF

@set:
    ldx TmpA
    sta TileBuffer, x
    inx
    stx TmpA
    cpx #32
    bne @loop

    lda CoordY
    bne :+
    lda #1
    sta SwapReady
:
    rts
