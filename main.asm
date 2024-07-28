;
; TODO:
;   precalculate these for each cell:
;   NeighborCells - byte offset for cell
;   NeighborMasks - mask for cell

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
CurrentMask: .res 1

; X/Y coords of cells around the
; cell being inspected
;
; 012
; 3.4
; 567
NeighborsX: .res 8
NeighborsY: .res 8

Neighbor_ByteIdx: .res 8
Neighbors_Mask:   .res 8
Neighbor_Bytes:   .res 8

Tick: .res 1
UpdateReady: .res 1

SwapReady:   .res 1
BufferReady: .res 1
BufferAddr:  .res 2
TileBuffer:  .res 32

RngSeed: .res 2
PpuControl: .res 1
PpuMask: .res 1
MenuSelect: .res 1

.segment "OAM"
SpriteZero: .res 4

SpriteSeedUp:   .res 4
SpriteSeedDown: .res 4

SpriteSeed_0: .res 4
SpriteSeed_1: .res 4
SpriteSeed_2: .res 4
SpriteSeed_3: .res 4
SpriteSeed_Done: .res 4

SpriteSel: .res 4
.segment "BSS"

SmTableA: .res 120
SmTableB: .res 120

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
    .repeat 30, i
    .word $2000+(i*32)
    .endrepeat

; offset table
CellRows:
    .repeat 30, i
    .byte i*4
    .assert (i*4) < 256, error, "CellRows value overflow"
    .endrepeat

CellCols:
    .repeat 32, i
        .byte i/8
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

WaitForNMI_Menu:
    lda #0
    sta Sleeping
:
    jsr prng
    bit Sleeping
    bpl :-
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

    lda PpuMask
    and #$10
    beq @noSprites
    lda #$00
    sta $2003
    lda #$02
    sta $4014
@noSprites:

    lda PpuMask
    and #$08
    bne :+
    jmp @noBuffer
:
    lda BufferReady
    bne :+
    jmp @noBuffer
:
    lda BufferAddr+1
    sta $2006
    lda BufferAddr+0
    sta $2006

    .repeat 32, i
    lda TileBuffer+i
    sta $2007
    .endrepeat

    lda #0
    sta BufferAddr+0
    sta BufferAddr+1
    sta BufferReady

@noBuffer:

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
    lda PpuControl
    and #$FE
    sta PpuControl
    sta $2000
    jmp @selectDone
@tableB:
    lda #$89
    lda PpuControl
    ora #$01
    sta PpuControl
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

;    ldx #18
;    ldy #16
;    jsr SetCell
;
;    ldx #18
;    ldy #17
;    jsr SetCell
;
;    ldx #18
;    ldy #18
;    jsr SetCell
;
;    ldx #17
;    ldy #18
;    jsr SetCell
;
;    ldx #16
;    ldy #17
;    jsr SetCell

MenuStart = $216B
MenuCursor = $0F
MenuSize = 4

MenuInit:
    lda #$00
    sta PpuMask
    sta $2001

    lda #'Z'
    sta RngSeed+1
    lda #'o'
    sta RngSeed+0

    jsr ClearScreen
    jsr ClearSprites

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #.lobyte(MenuStart)
    sta ptrCurrent+0
    lda #.hibyte(MenuStart)
    sta ptrCurrent+1

    ldx #$FF
@loopOuter:
    lda ptrCurrent+1
    sta $2006
    lda ptrCurrent+0
    sta $2006

    inx
    lda MenuItems, x
    beq @menuDone

    sta $2007
@loopInner:
    inx
    lda MenuItems, x
    beq @menuNext
    sta $2007
    jmp @loopInner

@menuNext:
    clc
    lda ptrCurrent+0
    adc #64
    sta ptrCurrent+0

    lda ptrCurrent+1
    adc #0
    sta ptrCurrent+1
    jmp @loopOuter

@menuDone:

    lda #MenuCursor
    sta SpriteZero+1

    lda #72
    sta SpriteZero+3
    lda MenuCursorLocs+0
    sta SpriteZero+0

    lda #0
    sta $2005
    sta $2005
    sta MenuSelect

    lda #$00
    sta TableSelect

    lda #$80
    sta PpuControl
    sta $2000

    lda #$1A
    sta PpuMask
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

MenuFrame:
    jsr ReadControllers

    lda Controller_Pressed
    and #BUTTON_A | BUTTON_START
    beq @noStart
    lda MenuSelect
    asl a
    tax
    lda MenuInits+0, x
    sta ptrCurrent+0
    lda MenuInits+1, x
    sta ptrCurrent+1
    jmp (ptrCurrent)
@noStart:

    lda Controller_Pressed
    and #BUTTON_SELECT | BUTTON_DOWN
    beq @noDown
    inc MenuSelect
    lda MenuSelect
    cmp #MenuSize
    bne :+
    lda #0
    sta MenuSelect
:
    jmp @menuDone
@noDown:

    lda Controller_Pressed
    and #BUTTON_UP
    beq @menuDone
    dec MenuSelect
    bpl @menuDone
    lda #MenuSize-1
    sta MenuSelect

@menuDone:
    ldx MenuSelect
    lda MenuCursorLocs, x
    sta SpriteZero+0

    jsr WaitForNMI_Menu
    jmp MenuFrame

MenuItems:
    .asciiz "Random Seed"
    .asciiz "Enter Seed"
    .asciiz "Edit Board"
    .asciiz "Clear Board"
    .byte $00

MenuInits:
    .word SimInitRando
    .word EnterSeed
    .word EditInit
    .word ClearBoard

MenuCursorLocs:
    .repeat MenuSize, i
    .byte 87+(16*i)
    .endrepeat

; Tiles
SeedUp   = $81
SeedDown = $80
SeedStart = $21CC

ClearBoard:
    ldx #120-1
    lda #0
:
    sta SmTableA, x
    dex
    sta SmTableA, x
    dex
    bpl :-

    jsr WaitForNMI
    jmp MenuFrame
    rts

EnterSeed:
    jsr WaitForNMI

    lda #$00
    sta PpuMask
    sta $2001

    jsr ClearScreen

    lda #$34
    sta RngSeed+0
    lda #$12
    sta RngSeed+1

    lda #' '
    sta SpriteZero+1

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #'>'
    sta SpriteSeed_Done+1

    lda #SeedUp
    sta SpriteSeedUp+1
    lda #SeedDown
    sta SpriteSeedDown+1

    lda #95
    sta SpriteSeedUp+3
    sta SpriteSeedDown+3

    lda #102
    sta SpriteSeedUp+0
    lda #120
    sta SpriteSeedDown+0

    lda #111
    sta SpriteSeed_0+0
    sta SpriteSeed_1+0
    sta SpriteSeed_2+0
    sta SpriteSeed_3+0
    sta SpriteSeed_Done+0

    lda SeedPositions+0
    sta SpriteSeed_0+3
    lda SeedPositions+1
    sta SpriteSeed_1+3
    lda SeedPositions+2
    sta SpriteSeed_2+3
    lda SeedPositions+3
    sta SpriteSeed_3+3
    lda SeedPositions+4
    sta SpriteSeed_Done+3

    lda #0
    sta MenuSelect

    jsr WaitForNMI

    lda #$80
    sta PpuControl
    sta $2000

    lda #$1A
    sta PpuMask
    sta $2001

SeedFrame:
    jsr ReadControllers

    lda Controller_Pressed
    and #BUTTON_UP
    beq @noUp
    lda MenuSelect
    cmp #0
    bne :+
    lda RngSeed+1
    lsr a
    lsr a
    lsr a
    lsr a
    clc
    adc #1
    asl a
    asl a
    asl a
    asl a
    tax
    lda RngSeed+1
    and #$0F
    sta RngSeed+1
    txa
    ora RngSeed+1
    sta RngSeed+1
    jmp @noUp
:
    cmp #1
    bne :+
    lda RngSeed+1
    and #$0F
    clc
    adc #1
    and #$0F
    tax
    lda RngSeed+1
    and #$F0
    sta RngSeed+1
    txa
    ora RngSeed+1
    sta RngSeed+1
    jmp @noUp
:
    cmp #2
    bne :+
    lda RngSeed+0
    lsr a
    lsr a
    lsr a
    lsr a
    clc
    adc #1
    asl a
    asl a
    asl a
    asl a
    tax
    lda RngSeed+0
    and #$0F
    sta RngSeed+0
    txa
    ora RngSeed+0
    sta RngSeed+0
    jmp @noUp
:
    cmp #3
    bne @noUp
    lda RngSeed+0
    and #$0F
    clc
    adc #1
    and #$0F
    tax
    lda RngSeed+0
    and #$F0
    sta RngSeed+0
    txa
    ora RngSeed+0
    sta RngSeed+0
@noUp:

    lda Controller_Pressed
    and #BUTTON_DOWN
    beq @noDown
    lda MenuSelect
    cmp #0
    bne :+
    lda RngSeed+1
    lsr a
    lsr a
    lsr a
    lsr a
    sec
    sbc #1
    asl a
    asl a
    asl a
    asl a
    tax
    lda RngSeed+1
    and #$0F
    sta RngSeed+1
    txa
    ora RngSeed+1
    sta RngSeed+1
    jmp @noDown
:
    cmp #1
    bne :+
    lda RngSeed+1
    and #$0F
    sec
    sbc #1
    and #$0F
    tax
    lda RngSeed+1
    and #$F0
    sta RngSeed+1
    txa
    ora RngSeed+1
    sta RngSeed+1
    jmp @noDown
:
    cmp #2
    bne :+
    lda RngSeed+0
    lsr a
    lsr a
    lsr a
    lsr a
    sec
    sbc #1
    asl a
    asl a
    asl a
    asl a
    tax
    lda RngSeed+0
    and #$0F
    sta RngSeed+0
    txa
    ora RngSeed+0
    sta RngSeed+0
    jmp @noDown
:
    cmp #3
    bne @noDown
    lda RngSeed+0
    and #$0F
    sec
    sbc #1
    and #$0F
    tax
    lda RngSeed+0
    and #$F0
    sta RngSeed+0
    txa
    ora RngSeed+0
    sta RngSeed+0
@noDown:

    lda Controller_Pressed
    and #BUTTON_LEFT | BUTTON_SELECT
    beq :+
    dec MenuSelect
    bpl :+
    lda #4
    sta MenuSelect
:

    lda Controller_Pressed
    and #BUTTON_RIGHT
    beq :+
    inc MenuSelect
    lda MenuSelect
    cmp #5
    bne :+
    lda #0
    sta MenuSelect
:

    lda Controller_Pressed
    and #BUTTON_A
    beq :+
    lda MenuSelect
    cmp #4
    bne :+
    jsr WaitForNMI
    jmp SimInitRando
:

    lda Controller_Pressed
    and #BUTTON_START
    beq :+
    jsr WaitForNMI
    jmp SimInitRando
:

    lda Controller_Pressed
    and #BUTTON_B
    beq :+
    jsr WaitForNMI
    jmp MenuInit
:

    lda RngSeed+1
    lsr a
    lsr a
    lsr a
    lsr a
    ora #$F0
    sta SpriteSeed_0+1

    lda RngSeed+1
    ora #$F0
    sta SpriteSeed_1+1

    lda RngSeed+0
    lsr a
    lsr a
    lsr a
    lsr a
    ora #$F0
    sta SpriteSeed_2+1

    lda RngSeed+0
    ora #$F0
    sta SpriteSeed_3+1

    ldx MenuSelect
    lda SeedPositions, x
    sta SpriteSeedUp+3
    sta SpriteSeedDown+3

    jsr WaitForNMI
    jmp SeedFrame

SeedPositions:
    .byte 96+(16*0)
    .byte 96+(16*1)
    .byte 96+(16*2)
    .byte 96+(16*3)
    .byte 96+(16*4)

SeedVals:
    .word RngSeed+0
    .word RngSeed+0
    .word RngSeed+1
    .word RngSeed+1
    .word $0000

EditInit:
    lda #$00
    sta PpuMask
    sta $2001

    jsr ClearScreen
    jsr ClearSprites
    jsr DrawInitTable

    lda #$82
    sta SpriteSel+1

    lda #10
    sta CoordX
    sta CoordY

    jsr WaitForNMI

    lda #.lobyte(SmTableA)
    sta ptrCurrent+0
    lda #.hibyte(SmTableA)
    sta ptrCurrent+1

    lda #.lobyte(SmTableA)
    sta ptrNext+0
    lda #.hibyte(SmTableA)
    sta ptrNext+1

    lda #$80
    sta PpuControl
    sta $2000

    lda #$1E
    sta PpuMask
    sta $2001

EditFrame:
    jsr ReadControllers

    lda Controller_Pressed
    and #BUTTON_LEFT
    beq :+
    dec CoordX
    lda CoordX
    bpl :+
    lda #31
    sta CoordX
:

    lda Controller_Pressed
    and #BUTTON_RIGHT
    beq :+
    inc CoordX
    lda CoordX
    cmp #32
    bne :+
    lda #0
    sta CoordX
:

    lda Controller_Pressed
    and #BUTTON_UP
    beq :+
    dec CoordY
    lda CoordY
    bpl :+
    lda #29
    sta CoordY
:

    lda Controller_Pressed
    and #BUTTON_DOWN
    beq :+
    inc CoordY
    lda CoordY
    cmp #30
    bne :+
    lda #0
    sta CoordY
:

    lda Controller_Pressed
    and #BUTTON_A
    beq @noA
    ldy CoordY
    ldx CoordX
    jsr ToggleCell
@noA:

    lda Controller_Pressed
    and #BUTTON_LEFT|BUTTON_RIGHT|BUTTON_UP|BUTTON_DOWN
    beq :+
    lda Controller
    and #BUTTON_A
    beq :+
    ldy CoordY
    ldx CoordX
    jsr ToggleCell

:

    lda Controller_Pressed
    and #BUTTON_B
    beq :+
    jsr WaitForNMI
    jmp MenuInit
:

    lda Controller_Pressed
    and #BUTTON_START
    beq :+
    jsr WaitForNMI
    jmp SimInit
:

    ldx CoordX
    lda SelCols, x
    sta SpriteSel+3

    ldx CoordY
    lda SelRows, x
    sta SpriteSel+0

    jsr WaitForNMI

    lda CoordY
    asl a
    tax
    lda PpuRows+0, x
    clc
    adc CoordX
    sta TmpA

    lda PpuRows+1, x
    adc #0
    bit $2002
    sta $2006
    lda TmpA
    sta $2006

    ldy CoordY
    ldx CoordX
    jsr GetCell
    beq :+
    lda #1
    sta $2007
    jmp :++
:
    lda #0
    sta $2007
:

    lda #0
    sta $2005
    sta $2005

    jmp EditFrame

SelRows:
    .byte 0
    .repeat 29, i
    .byte (((i+1)*8)-1) & $FF
    .endrepeat

SelCols:
    .repeat 32, i
    .byte i*8
    .endrepeat

SimInitRando:
    jsr RandoField

SimInit:
    lda #$00
    sta PpuMask
    sta $2001

    jsr ClearSprites
    jsr DrawInitTable

    lda #0
    sta CoordY

    lda #$00
    sta TableSelect
    sta BufferAddr+0
    sta BufferAddr+1
    sta SwapReady

    lda #$80
    sta PpuControl
    sta $2000

    lda #$0A
    sta PpuMask
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

SimFrame:
    jsr ReadControllers
    lda Controller_Pressed
    and #BUTTON_B
    beq :+
    jsr CopyBtoA
    jsr WaitForNMI
    jmp MenuInit
:

    jsr SmUpdate_Redo
    ;jsr SmUpdate
    jsr SmBuffer
    jsr WaitForNMI
    jmp SimFrame

CopyBtoA:
    lda TableSelect
    bne :+
    rts
:

    ldx #120-1
:
    lda SmTableB, x
    sta SmTableA, x
    dex
    lda SmTableB, x
    sta SmTableA, x
    dex
    bpl :-
    rts

SmUpdate_Redo:
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

@cellLoop:
    lda #0
    sta CurrentAlive

    ldx CoordX
    ldy CoordY
    jsr GetCell ; returns non-zero if cell is alive.
    beq :+
    lda #1
    sta CurrentAlive
:

    ;ldx CoordY
    ;lda Mult32_Lo, x
    ;sta ptrCurrent+0

    ;lda Mult32_Hi, x
    ;sta ptrCurrent+1

    ;ldy CoordX
    ;lda (ptrCurrent), y

    ; ptrInspect = NeighborBytes + Y*32*8 + X*8

    lda CoordX
    asl a
    asl a
    asl a
    clc
    adc #.lobyte(NeighborBytes)
    sta ptrInspect+0

    lda CoordY
    adc #.hibyte(NeighborBytes)
    sta ptrInspect+1

    lda CoordX
    and #$07
    tax
    lda CellMasks, x
    sta CurrentMask

    ldy #0
:
    lda (ptrInspect), y
    sta Neighbor_ByteIdx, y
    iny
    cpy #8
    bne :-

    ldx #0
:
    ldy Neighbor_ByteIdx, x
    lda (ptrCurrent), y
    sta Neighbor_Bytes, x
    inx
    cpx #8
    bne :-

    ldy #0
    sty Neighbors
    sty TmpY
;:
    ;ldy TmpY
    ;lda (ptrInspect), y
    ;sta Neighbors_ByteIdx, y

    ;ldy #1 ; (X, Y-1)
    lda Neighbor_Bytes+1
    and CurrentMask
    beq :+
    inc Neighbors
:

    ;ldy #6 ; (X, Y+1)
    lda Neighbor_Bytes+6
    and CurrentMask
    beq :+
    inc Neighbors
:

    lda CurrentMask
    asl a
    lda CurrentMask
    rol a
    sta TmpZ

    ;ldy #0 ; (X-1, Y-1)
    lda Neighbor_Bytes+0
    and TmpZ
    beq :+
    inc Neighbors
:

    ;ldy #3 ; (X-1, Y)
    lda Neighbor_Bytes+3
    and TmpZ
    beq :+
    inc Neighbors
:

    ;ldy #5 ; (X-1, Y+1)
    lda Neighbor_Bytes+5
    and TmpZ
    beq :+
    inc Neighbors
:

    lda CurrentMask
    lsr a
    lda CurrentMask
    ror a
    sta TmpZ

    ;ldy #2 ; (X+1, Y-1)
    lda Neighbor_Bytes+2
    and TmpZ
    beq :+
    inc Neighbors
:

    ;ldy #4 ; (X+1, Y)
    lda Neighbor_Bytes+4
    and TmpZ
    beq :+
    inc Neighbors
:

    ;ldy #7 ; (X+1, Y)
    lda Neighbor_Bytes+7
    and TmpZ
    beq :+
    inc Neighbors
:

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
    beq :+
    jmp @cellLoop
:
    lda #0
    sta CoordX

    inc CoordY
    lda CoordY
    cmp #30
    bne @notDone
    lda #0
    sta CoordY

@notDone:
    rts

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

    clc
    lda CellRows, y
    adc CellCols, x ; Offset of byte for current cell
    tay

    txa
    and #$07 ; %0000_0111
    tax

    lda (ptrCurrent), y
    and CellMasks, x
    ;jsr GetCell
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
    clc
    lda CellRows, y
    adc CellCols, x ; Offset of byte for current cell
    tay

    txa
    and #$07 ; %0000_0111
    tax

    lda (ptrCurrent), y
    and CellMasks, x
    rts

; X&Y coordinates in X&Y registers
SetCell:
    clc
    lda CellRows, y
    adc CellCols, x ; Offset of byte for current cell
    tay

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

ToggleCell:
    clc
    lda CellRows, y
    adc CellCols, x ; Offset of byte for current cell
    tay

    txa
    and #$07 ; %0000_0111
    tax

    lda (ptrCurrent), y
    and CellMasksInvert, x
    sta TmpX

    lda (ptrCurrent), y
    eor #$FF
    and CellMasks, x
    ora TmpX
    sta (ptrCurrent), y
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
    sty TmpB

    lda #0
    sta TmpA

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

@outerloop:
    ldx TmpA
    ldy TmpB

    ;jsr GetCell
    clc
    lda CellRows, y
    adc CellCols, x ; Offset of byte for current cell
    tay

    txa
    and #$07 ; %0000_0111
    tax

    lda (ptrCurrent), y
    sta TmpZ
    ;and CellMasks, x

    ldy #8
@innerLoop:
    asl TmpZ
    bcc @off
    lda #TileON
    jmp @set
@off:
    lda #TileOFF

@set:
    ldx TmpA
    sta TileBuffer, x
    inx
    stx TmpA
    dey
    bne @innerLoop

    cpx #32
    bne @outerloop

    lda CoordY
    bne :+
    lda #1
    sta SwapReady
:

    lda #$FF
    sta BufferReady
    rts

prng:
    ldy #8  ; iteration count (generates 8 bits)
    lda RngSeed
    bne @one
    ;lda seed_ram

@one:
    asl a    ; shift the register
    rol RngSeed+1
    bcc @two
    ; Apply XOR feedback whenever a 1 bit is shifted out
    eor #$2D
@two:

    dey
    bne @one    ; generate another bit

    sta RngSeed
    cmp #0  ; reload flags
    rts

RandoField:
    ldx #0
@byte:

    lda #8
    sta TmpA
@inner:
    jsr prng
    lda RngSeed+1
    cmp #256-40
    ror SmTableA, x

    dec TmpA
    bne @inner

    inx
    cpx #120
    bne @byte
    rts

ClearSprites:
    lda #' '
    sta SpriteSeed_0+1
    sta SpriteSeed_1+1
    sta SpriteSeed_2+1
    sta SpriteSeed_3+1
    sta SpriteSeed_Done+1
    sta SpriteSeedUp+1
    sta SpriteSeedDown+1
    sta SpriteZero+1
    sta SpriteSel+1
    rts

DrawInitTable:
    lda #$FF
    sta TableSelect

    lda #0
    sta CoordX
    lda #1
    sta CoordY

    lda #$20
    sta $2006
    lda #$00
    sta $2006

@initloop:
    jsr SmBuffer
    ldx #0
:
    lda TileBuffer, x
    sta $2007
    inx
    cpx #32
    bne :-

    inc CoordY
    lda CoordY
    cmp #30
    bne @initloop
    lda #$00
    sta TableSelect
    rts

ClearScreen:
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    ldy #0
    lda #0
@loop:
    sta $2007
    sta $2007
    sta $2007
    sta $2007
    iny
    bne @loop
    rts

    .include "neighbors.inc"
