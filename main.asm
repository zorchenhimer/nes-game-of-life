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

TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

TableSelect: .res 1
AttrBuffer: .res 64

ptrTable: .res 2
ptrRow: .res 2

ptrCurrent: .res 2
ptrNext: .res 2
ptrCell: .res 2
ptrInspect: .res 2

Neighbors: .res 1

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

.segment "OAM"
.segment "BSS"

TableA: .res 240
TableB: .res 240

.segment "CHR0"
.incbin "main.chr"

.segment "PAGE0"

Palettes:
    .byte $0F, $0F, $20, $00
    .byte $0F, $20, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00

    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00
    .byte $0F, $10, $20, $00

RowOffsetsA:
    .repeat 15, i
    .word (i*16)+TableA
    .endrepeat

RowOffsetsB:
    .repeat 15, i
    .word (i*16)+TableB
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

    lda UpdateReady
    bne :+
    jmp @nope
:

    lda #$23
    sta $2006
    lda #$C0
    sta $2006
    .repeat 64, i
    lda AttrBuffer+i
    sta $2007
    .endrepeat
@nope:
    lda #0
    sta UpdateReady

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

    ; draw initial cell grid thing
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    lda #15
    sta TmpX

@outer:
    lda #0
    ldy #1

    ldx #16
:
    sta $2007
    sty $2007
    dex
    bne :-

    lda #2
    ldy #3

    ldx #16
:
    sta $2007
    sty $2007
    dex
    bne :-

    dec TmpX
    bne @outer

    lda #0
    ldx #64
:
    sta $2006
    dex
    bne :-

;    ldx #0
;    lda #$FF
;    ldy #16*2
;:
;    sta TableA+1, x
;    inx
;    inx
;    dey
;    bne :-

; ..X
; X.X
; .XX

    lda #$FF
    ;           Row
    sta TableA+(16*0)+2
    sta TableA+(16*1)+2
    sta TableA+(16*2)+2
    sta TableA+(16*2)+1
    sta TableA+(16*1)+0

    ;lda #1
    ;sta TableA+49
    ;sta TableA+50
    ;sta TableA+51

    lda #$88
    sta $2000

    lda #$0A
    sta $2001

    ;lda #15
    ;sta Tick

ResetFrame:
    ;dec Tick
    ;bne :+
    jsr UpdateTable
;:
    jsr BufferTable
    jsr WaitForNMI
    jmp ResetFrame

UpdateTable:

    ; select the correct table
    lda TableSelect
    beq :+
    lda #.lobyte(RowOffsetsB)
    sta ptrCurrent+0
    lda #.hibyte(RowOffsetsB)
    sta ptrCurrent+1

    lda #.lobyte(RowOffsetsA)
    sta ptrNext+0
    lda #.hibyte(RowOffsetsA)
    sta ptrNext+1
    jmp :++
:
    lda #.lobyte(RowOffsetsB)
    sta ptrNext+0
    lda #.hibyte(RowOffsetsB)
    sta ptrNext+1

    lda #.lobyte(RowOffsetsA)
    sta ptrCurrent+0
    lda #.hibyte(RowOffsetsA)
    sta ptrCurrent+1
:

    lda TableSelect
    eor #$FF
    sta TableSelect

    ldy #0
    sty TmpY    ; current row
    sty TmpX    ; current column
@cell:
    ; get current cell's addr
    lda TmpY
    asl a
    tay
    lda (ptrCurrent), y
    sta ptrCell+0
    iny
    lda (ptrCurrent), y
    sta ptrCell+1

    clc
    lda ptrCell+0
    adc TmpX
    sta ptrCell+0

    lda ptrCell+1
    adc #0
    sta ptrCell+1

    ldx TmpY
    beq @wrapAbove
    dex
    stx NeighborsY+0
    stx NeighborsY+1
    stx NeighborsY+2
    jmp @aboveDone
@wrapAbove:
    lda #14
    sta NeighborsY+0
    sta NeighborsY+1
    sta NeighborsY+2
@aboveDone:

    ; middle row
    lda TmpY
    sta NeighborsY+3
    sta NeighborsY+4

    ldx TmpY
    cpx #14
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

    ldx TmpX
    beq @wrapLeft
    dex
    stx NeighborsX+0
    stx NeighborsX+3
    stx NeighborsX+5
    jmp @leftDone
@wrapLeft:
    ldx #15
    stx NeighborsX+0
    stx NeighborsX+3
    stx NeighborsX+5
@leftDone:

    ; middle column
    lda TmpX
    sta NeighborsX+1
    sta NeighborsX+6

    ldx TmpX
    cpx #15
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
@loop:
    jsr InspectCell
    inx
    cpx #8
    bne @loop

    lda TmpY
    asl a
    tay
    clc
    lda (ptrNext), y
    adc TmpX
    sta ptrInspect+0
    iny
    lda (ptrNext), y
    adc #0
    sta ptrInspect+1

    ; cell modifications
    ldy #0
    lda (ptrCell), y
    beq @dead

    lda Neighbors
    cmp #2
    bcs :+
    ; kill it
    lda #0
    sta (ptrInspect), y
    jmp @cellDone
:

    cmp #4
    bcc :+
    ; kill it
    lda #0
    sta (ptrInspect), y
    jmp @cellDone
:

    lda #$FF
    sta (ptrInspect), y
    jmp @cellDone

@dead:
    lda Neighbors
    cmp #3
    bne :+
    ; revive it
    lda #$FF
    sta (ptrInspect), y
    jmp @cellDone
:
    lda #0
    sta (ptrInspect), y

@cellDone:
    inc TmpX
    lda TmpX
    cmp #16
    bne @nextCell

    lda #0
    sta TmpX
    inc TmpY
    lda TmpY
    cmp #15
    bne @nextCell
    rts

@nextCell:
    jmp @cell

; Inspect a cell and increment Neghbors
; if something is found
InspectCell:
    lda NeighborsY, x
    asl a
    tay
    clc
    lda (ptrCurrent), y
    adc NeighborsX, x
    sta ptrInspect+0
    iny
    lda (ptrCurrent), y
    adc #0
    sta ptrInspect+1

    ldy #0
    lda (ptrInspect), y
    beq :+
    inc Neighbors
:
    rts

ATTR_TopLeft  = %0000_0011
ATTR_TopRight = %0000_1100
ATTR_BotLeft  = %0011_0000
ATTR_BotRight = %1100_0000

; Read a full table and fill the attribute buffer
BufferTable:
    lda #0
    ldx #.sizeof(AttrBuffer)-1
:
    sta AttrBuffer, x
    dex
    bpl :-

    ; select the correct table
    lda TableSelect
    beq :+
    lda #.lobyte(RowOffsetsB)
    sta ptrTable+0
    lda #.hibyte(RowOffsetsB)
    sta ptrTable+1
    jmp :++
:
    lda #.lobyte(RowOffsetsA)
    sta ptrTable+0
    lda #.hibyte(RowOffsetsA)
    sta ptrTable+1
:

    ldy #0
    ldx #0
@newRow:
    sty TmpY
    ; get a pointer to the row
    lda (ptrTable), y
    sta ptrRow+0
    iny
    lda (ptrTable), y
    sta ptrRow+1

    ldy #0
    lda #8
    sta TmpZ
@row:
    lda #0
    sta TmpX

    tya
    pha
    ; top left
    lda (ptrRow), y
    beq :+
    lda #ATTR_TopLeft
    sta TmpX
:

    ; top right
    iny
    lda (ptrRow), y
    beq :+
    lda #ATTR_TopRight
    ora TmpX
    sta TmpX
:

    ; bottom left
    tya
    clc
    adc #15
    tay
    lda (ptrRow), y
    beq :+
    lda #ATTR_BotLeft
    ora TmpX
    sta TmpX
:

    ; bottom right
    iny
    lda (ptrRow), y
    beq :+
    lda #ATTR_BotRight
    ora TmpX
    sta TmpX
:
    pla
    tay
    iny
    iny

    lda TmpX
    sta AttrBuffer, x
    inx
    dec TmpZ
    bne @row

    ldy TmpY
    iny
    iny
    iny
    iny
    cpy #32
    bne @newRow

    lda #1
    sta UpdateReady

    rts
