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

MenuStart = $218B
MenuCursor = $0F
MenuSize = 3

MenuInit:
    lda #$00
    sta PpuMask
    sta $2001

    lda #'Z'
    sta RngSeed+1
    lda #'o'
    sta RngSeed+0

    lda #' '
    sta SpriteSeed_0+1
    sta SpriteSeed_1+1
    sta SpriteSeed_2+1
    sta SpriteSeed_3+1
    sta SpriteSeed_Done+1
    sta SpriteSeedUp+1
    sta SpriteSeedDown+1

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
    lda #96
    sta SpriteZero+0

    lda #0
    sta $2005
    sta $2005
    sta MenuSelect

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
    cmp #3
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
    lda #2
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
    .byte $00

MenuInits:
    .word SimInit
    .word EnterSeed
    .word EditInit

MenuCursorLocs:
    .byte 95+(16*0)
    .byte 95+(16*1)
    .byte 95+(16*2)

; Tiles
SeedUp   = $81
SeedDown = $80
SeedStart = $21CC

EnterSeed:
    jsr WaitForNMI

    lda #$00
    sta PpuMask
    sta $2001

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
    jmp SimInit
:

    lda Controller_Pressed
    and #BUTTON_START
    beq :+
    jsr WaitForNMI
    jmp SimInit
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
    brk
    jmp EditInit

SimInit:
    jsr RandoField
    lda #$00
    sta PpuMask
    sta $2001

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

    ;lda BufferAddr+1
    ;sta $2006
    ;lda BufferAddr+0
    ;sta $2006

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
    jsr SmUpdate_Redo
    ;jsr SmUpdate
    jsr SmBuffer
    jsr WaitForNMI
    jmp SimFrame

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

NeighborBytes:
.byte  119,  116,  116,    3,    0,    7,    4,    4
.byte  116,  116,  116,    0,    0,    4,    4,    4
.byte  116,  116,  116,    0,    0,    4,    4,    4
.byte  116,  116,  116,    0,    0,    4,    4,    4
.byte  116,  116,  116,    0,    0,    4,    4,    4
.byte  116,  116,  116,    0,    0,    4,    4,    4
.byte  116,  116,  116,    0,    0,    4,    4,    4
.byte  116,  116,  117,    0,    1,    4,    4,    5
.byte  116,  117,  117,    0,    1,    4,    5,    5
.byte  117,  117,  117,    1,    1,    5,    5,    5
.byte  117,  117,  117,    1,    1,    5,    5,    5
.byte  117,  117,  117,    1,    1,    5,    5,    5
.byte  117,  117,  117,    1,    1,    5,    5,    5
.byte  117,  117,  117,    1,    1,    5,    5,    5
.byte  117,  117,  117,    1,    1,    5,    5,    5
.byte  117,  117,  118,    1,    2,    5,    5,    6
.byte  117,  118,  118,    1,    2,    5,    6,    6
.byte  118,  118,  118,    2,    2,    6,    6,    6
.byte  118,  118,  118,    2,    2,    6,    6,    6
.byte  118,  118,  118,    2,    2,    6,    6,    6
.byte  118,  118,  118,    2,    2,    6,    6,    6
.byte  118,  118,  118,    2,    2,    6,    6,    6
.byte  118,  118,  118,    2,    2,    6,    6,    6
.byte  118,  118,  119,    2,    3,    6,    6,    7
.byte  118,  119,  119,    2,    3,    6,    7,    7
.byte  119,  119,  119,    3,    3,    7,    7,    7
.byte  119,  119,  119,    3,    3,    7,    7,    7
.byte  119,  119,  119,    3,    3,    7,    7,    7
.byte  119,  119,  119,    3,    3,    7,    7,    7
.byte  119,  119,  119,    3,    3,    7,    7,    7
.byte  119,  119,  119,    3,    3,    7,    7,    7
.byte  119,  119,    0,    3,    4,    7,    7,    8
.byte    3,    0,    0,    7,    4,   11,    8,    8
.byte    0,    0,    0,    4,    4,    8,    8,    8
.byte    0,    0,    0,    4,    4,    8,    8,    8
.byte    0,    0,    0,    4,    4,    8,    8,    8
.byte    0,    0,    0,    4,    4,    8,    8,    8
.byte    0,    0,    0,    4,    4,    8,    8,    8
.byte    0,    0,    0,    4,    4,    8,    8,    8
.byte    0,    0,    1,    4,    5,    8,    8,    9
.byte    0,    1,    1,    4,    5,    8,    9,    9
.byte    1,    1,    1,    5,    5,    9,    9,    9
.byte    1,    1,    1,    5,    5,    9,    9,    9
.byte    1,    1,    1,    5,    5,    9,    9,    9
.byte    1,    1,    1,    5,    5,    9,    9,    9
.byte    1,    1,    1,    5,    5,    9,    9,    9
.byte    1,    1,    1,    5,    5,    9,    9,    9
.byte    1,    1,    2,    5,    6,    9,    9,   10
.byte    1,    2,    2,    5,    6,    9,   10,   10
.byte    2,    2,    2,    6,    6,   10,   10,   10
.byte    2,    2,    2,    6,    6,   10,   10,   10
.byte    2,    2,    2,    6,    6,   10,   10,   10
.byte    2,    2,    2,    6,    6,   10,   10,   10
.byte    2,    2,    2,    6,    6,   10,   10,   10
.byte    2,    2,    2,    6,    6,   10,   10,   10
.byte    2,    2,    3,    6,    7,   10,   10,   11
.byte    2,    3,    3,    6,    7,   10,   11,   11
.byte    3,    3,    3,    7,    7,   11,   11,   11
.byte    3,    3,    3,    7,    7,   11,   11,   11
.byte    3,    3,    3,    7,    7,   11,   11,   11
.byte    3,    3,    3,    7,    7,   11,   11,   11
.byte    3,    3,    3,    7,    7,   11,   11,   11
.byte    3,    3,    3,    7,    7,   11,   11,   11
.byte    3,    3,    4,    7,    8,   11,   11,   12
.byte    7,    4,    4,   11,    8,   15,   12,   12
.byte    4,    4,    4,    8,    8,   12,   12,   12
.byte    4,    4,    4,    8,    8,   12,   12,   12
.byte    4,    4,    4,    8,    8,   12,   12,   12
.byte    4,    4,    4,    8,    8,   12,   12,   12
.byte    4,    4,    4,    8,    8,   12,   12,   12
.byte    4,    4,    4,    8,    8,   12,   12,   12
.byte    4,    4,    5,    8,    9,   12,   12,   13
.byte    4,    5,    5,    8,    9,   12,   13,   13
.byte    5,    5,    5,    9,    9,   13,   13,   13
.byte    5,    5,    5,    9,    9,   13,   13,   13
.byte    5,    5,    5,    9,    9,   13,   13,   13
.byte    5,    5,    5,    9,    9,   13,   13,   13
.byte    5,    5,    5,    9,    9,   13,   13,   13
.byte    5,    5,    5,    9,    9,   13,   13,   13
.byte    5,    5,    6,    9,   10,   13,   13,   14
.byte    5,    6,    6,    9,   10,   13,   14,   14
.byte    6,    6,    6,   10,   10,   14,   14,   14
.byte    6,    6,    6,   10,   10,   14,   14,   14
.byte    6,    6,    6,   10,   10,   14,   14,   14
.byte    6,    6,    6,   10,   10,   14,   14,   14
.byte    6,    6,    6,   10,   10,   14,   14,   14
.byte    6,    6,    6,   10,   10,   14,   14,   14
.byte    6,    6,    7,   10,   11,   14,   14,   15
.byte    6,    7,    7,   10,   11,   14,   15,   15
.byte    7,    7,    7,   11,   11,   15,   15,   15
.byte    7,    7,    7,   11,   11,   15,   15,   15
.byte    7,    7,    7,   11,   11,   15,   15,   15
.byte    7,    7,    7,   11,   11,   15,   15,   15
.byte    7,    7,    7,   11,   11,   15,   15,   15
.byte    7,    7,    7,   11,   11,   15,   15,   15
.byte    7,    7,    8,   11,   12,   15,   15,   16
.byte   11,    8,    8,   15,   12,   19,   16,   16
.byte    8,    8,    8,   12,   12,   16,   16,   16
.byte    8,    8,    8,   12,   12,   16,   16,   16
.byte    8,    8,    8,   12,   12,   16,   16,   16
.byte    8,    8,    8,   12,   12,   16,   16,   16
.byte    8,    8,    8,   12,   12,   16,   16,   16
.byte    8,    8,    8,   12,   12,   16,   16,   16
.byte    8,    8,    9,   12,   13,   16,   16,   17
.byte    8,    9,    9,   12,   13,   16,   17,   17
.byte    9,    9,    9,   13,   13,   17,   17,   17
.byte    9,    9,    9,   13,   13,   17,   17,   17
.byte    9,    9,    9,   13,   13,   17,   17,   17
.byte    9,    9,    9,   13,   13,   17,   17,   17
.byte    9,    9,    9,   13,   13,   17,   17,   17
.byte    9,    9,    9,   13,   13,   17,   17,   17
.byte    9,    9,   10,   13,   14,   17,   17,   18
.byte    9,   10,   10,   13,   14,   17,   18,   18
.byte   10,   10,   10,   14,   14,   18,   18,   18
.byte   10,   10,   10,   14,   14,   18,   18,   18
.byte   10,   10,   10,   14,   14,   18,   18,   18
.byte   10,   10,   10,   14,   14,   18,   18,   18
.byte   10,   10,   10,   14,   14,   18,   18,   18
.byte   10,   10,   10,   14,   14,   18,   18,   18
.byte   10,   10,   11,   14,   15,   18,   18,   19
.byte   10,   11,   11,   14,   15,   18,   19,   19
.byte   11,   11,   11,   15,   15,   19,   19,   19
.byte   11,   11,   11,   15,   15,   19,   19,   19
.byte   11,   11,   11,   15,   15,   19,   19,   19
.byte   11,   11,   11,   15,   15,   19,   19,   19
.byte   11,   11,   11,   15,   15,   19,   19,   19
.byte   11,   11,   11,   15,   15,   19,   19,   19
.byte   11,   11,   12,   15,   16,   19,   19,   20
.byte   15,   12,   12,   19,   16,   23,   20,   20
.byte   12,   12,   12,   16,   16,   20,   20,   20
.byte   12,   12,   12,   16,   16,   20,   20,   20
.byte   12,   12,   12,   16,   16,   20,   20,   20
.byte   12,   12,   12,   16,   16,   20,   20,   20
.byte   12,   12,   12,   16,   16,   20,   20,   20
.byte   12,   12,   12,   16,   16,   20,   20,   20
.byte   12,   12,   13,   16,   17,   20,   20,   21
.byte   12,   13,   13,   16,   17,   20,   21,   21
.byte   13,   13,   13,   17,   17,   21,   21,   21
.byte   13,   13,   13,   17,   17,   21,   21,   21
.byte   13,   13,   13,   17,   17,   21,   21,   21
.byte   13,   13,   13,   17,   17,   21,   21,   21
.byte   13,   13,   13,   17,   17,   21,   21,   21
.byte   13,   13,   13,   17,   17,   21,   21,   21
.byte   13,   13,   14,   17,   18,   21,   21,   22
.byte   13,   14,   14,   17,   18,   21,   22,   22
.byte   14,   14,   14,   18,   18,   22,   22,   22
.byte   14,   14,   14,   18,   18,   22,   22,   22
.byte   14,   14,   14,   18,   18,   22,   22,   22
.byte   14,   14,   14,   18,   18,   22,   22,   22
.byte   14,   14,   14,   18,   18,   22,   22,   22
.byte   14,   14,   14,   18,   18,   22,   22,   22
.byte   14,   14,   15,   18,   19,   22,   22,   23
.byte   14,   15,   15,   18,   19,   22,   23,   23
.byte   15,   15,   15,   19,   19,   23,   23,   23
.byte   15,   15,   15,   19,   19,   23,   23,   23
.byte   15,   15,   15,   19,   19,   23,   23,   23
.byte   15,   15,   15,   19,   19,   23,   23,   23
.byte   15,   15,   15,   19,   19,   23,   23,   23
.byte   15,   15,   15,   19,   19,   23,   23,   23
.byte   15,   15,   16,   19,   20,   23,   23,   24
.byte   19,   16,   16,   23,   20,   27,   24,   24
.byte   16,   16,   16,   20,   20,   24,   24,   24
.byte   16,   16,   16,   20,   20,   24,   24,   24
.byte   16,   16,   16,   20,   20,   24,   24,   24
.byte   16,   16,   16,   20,   20,   24,   24,   24
.byte   16,   16,   16,   20,   20,   24,   24,   24
.byte   16,   16,   16,   20,   20,   24,   24,   24
.byte   16,   16,   17,   20,   21,   24,   24,   25
.byte   16,   17,   17,   20,   21,   24,   25,   25
.byte   17,   17,   17,   21,   21,   25,   25,   25
.byte   17,   17,   17,   21,   21,   25,   25,   25
.byte   17,   17,   17,   21,   21,   25,   25,   25
.byte   17,   17,   17,   21,   21,   25,   25,   25
.byte   17,   17,   17,   21,   21,   25,   25,   25
.byte   17,   17,   17,   21,   21,   25,   25,   25
.byte   17,   17,   18,   21,   22,   25,   25,   26
.byte   17,   18,   18,   21,   22,   25,   26,   26
.byte   18,   18,   18,   22,   22,   26,   26,   26
.byte   18,   18,   18,   22,   22,   26,   26,   26
.byte   18,   18,   18,   22,   22,   26,   26,   26
.byte   18,   18,   18,   22,   22,   26,   26,   26
.byte   18,   18,   18,   22,   22,   26,   26,   26
.byte   18,   18,   18,   22,   22,   26,   26,   26
.byte   18,   18,   19,   22,   23,   26,   26,   27
.byte   18,   19,   19,   22,   23,   26,   27,   27
.byte   19,   19,   19,   23,   23,   27,   27,   27
.byte   19,   19,   19,   23,   23,   27,   27,   27
.byte   19,   19,   19,   23,   23,   27,   27,   27
.byte   19,   19,   19,   23,   23,   27,   27,   27
.byte   19,   19,   19,   23,   23,   27,   27,   27
.byte   19,   19,   19,   23,   23,   27,   27,   27
.byte   19,   19,   20,   23,   24,   27,   27,   28
.byte   23,   20,   20,   27,   24,   31,   28,   28
.byte   20,   20,   20,   24,   24,   28,   28,   28
.byte   20,   20,   20,   24,   24,   28,   28,   28
.byte   20,   20,   20,   24,   24,   28,   28,   28
.byte   20,   20,   20,   24,   24,   28,   28,   28
.byte   20,   20,   20,   24,   24,   28,   28,   28
.byte   20,   20,   20,   24,   24,   28,   28,   28
.byte   20,   20,   21,   24,   25,   28,   28,   29
.byte   20,   21,   21,   24,   25,   28,   29,   29
.byte   21,   21,   21,   25,   25,   29,   29,   29
.byte   21,   21,   21,   25,   25,   29,   29,   29
.byte   21,   21,   21,   25,   25,   29,   29,   29
.byte   21,   21,   21,   25,   25,   29,   29,   29
.byte   21,   21,   21,   25,   25,   29,   29,   29
.byte   21,   21,   21,   25,   25,   29,   29,   29
.byte   21,   21,   22,   25,   26,   29,   29,   30
.byte   21,   22,   22,   25,   26,   29,   30,   30
.byte   22,   22,   22,   26,   26,   30,   30,   30
.byte   22,   22,   22,   26,   26,   30,   30,   30
.byte   22,   22,   22,   26,   26,   30,   30,   30
.byte   22,   22,   22,   26,   26,   30,   30,   30
.byte   22,   22,   22,   26,   26,   30,   30,   30
.byte   22,   22,   22,   26,   26,   30,   30,   30
.byte   22,   22,   23,   26,   27,   30,   30,   31
.byte   22,   23,   23,   26,   27,   30,   31,   31
.byte   23,   23,   23,   27,   27,   31,   31,   31
.byte   23,   23,   23,   27,   27,   31,   31,   31
.byte   23,   23,   23,   27,   27,   31,   31,   31
.byte   23,   23,   23,   27,   27,   31,   31,   31
.byte   23,   23,   23,   27,   27,   31,   31,   31
.byte   23,   23,   23,   27,   27,   31,   31,   31
.byte   23,   23,   24,   27,   28,   31,   31,   32
.byte   27,   24,   24,   31,   28,   35,   32,   32
.byte   24,   24,   24,   28,   28,   32,   32,   32
.byte   24,   24,   24,   28,   28,   32,   32,   32
.byte   24,   24,   24,   28,   28,   32,   32,   32
.byte   24,   24,   24,   28,   28,   32,   32,   32
.byte   24,   24,   24,   28,   28,   32,   32,   32
.byte   24,   24,   24,   28,   28,   32,   32,   32
.byte   24,   24,   25,   28,   29,   32,   32,   33
.byte   24,   25,   25,   28,   29,   32,   33,   33
.byte   25,   25,   25,   29,   29,   33,   33,   33
.byte   25,   25,   25,   29,   29,   33,   33,   33
.byte   25,   25,   25,   29,   29,   33,   33,   33
.byte   25,   25,   25,   29,   29,   33,   33,   33
.byte   25,   25,   25,   29,   29,   33,   33,   33
.byte   25,   25,   25,   29,   29,   33,   33,   33
.byte   25,   25,   26,   29,   30,   33,   33,   34
.byte   25,   26,   26,   29,   30,   33,   34,   34
.byte   26,   26,   26,   30,   30,   34,   34,   34
.byte   26,   26,   26,   30,   30,   34,   34,   34
.byte   26,   26,   26,   30,   30,   34,   34,   34
.byte   26,   26,   26,   30,   30,   34,   34,   34
.byte   26,   26,   26,   30,   30,   34,   34,   34
.byte   26,   26,   26,   30,   30,   34,   34,   34
.byte   26,   26,   27,   30,   31,   34,   34,   35
.byte   26,   27,   27,   30,   31,   34,   35,   35
.byte   27,   27,   27,   31,   31,   35,   35,   35
.byte   27,   27,   27,   31,   31,   35,   35,   35
.byte   27,   27,   27,   31,   31,   35,   35,   35
.byte   27,   27,   27,   31,   31,   35,   35,   35
.byte   27,   27,   27,   31,   31,   35,   35,   35
.byte   27,   27,   27,   31,   31,   35,   35,   35
.byte   27,   27,   28,   31,   32,   35,   35,   36
.byte   31,   28,   28,   35,   32,   39,   36,   36
.byte   28,   28,   28,   32,   32,   36,   36,   36
.byte   28,   28,   28,   32,   32,   36,   36,   36
.byte   28,   28,   28,   32,   32,   36,   36,   36
.byte   28,   28,   28,   32,   32,   36,   36,   36
.byte   28,   28,   28,   32,   32,   36,   36,   36
.byte   28,   28,   28,   32,   32,   36,   36,   36
.byte   28,   28,   29,   32,   33,   36,   36,   37
.byte   28,   29,   29,   32,   33,   36,   37,   37
.byte   29,   29,   29,   33,   33,   37,   37,   37
.byte   29,   29,   29,   33,   33,   37,   37,   37
.byte   29,   29,   29,   33,   33,   37,   37,   37
.byte   29,   29,   29,   33,   33,   37,   37,   37
.byte   29,   29,   29,   33,   33,   37,   37,   37
.byte   29,   29,   29,   33,   33,   37,   37,   37
.byte   29,   29,   30,   33,   34,   37,   37,   38
.byte   29,   30,   30,   33,   34,   37,   38,   38
.byte   30,   30,   30,   34,   34,   38,   38,   38
.byte   30,   30,   30,   34,   34,   38,   38,   38
.byte   30,   30,   30,   34,   34,   38,   38,   38
.byte   30,   30,   30,   34,   34,   38,   38,   38
.byte   30,   30,   30,   34,   34,   38,   38,   38
.byte   30,   30,   30,   34,   34,   38,   38,   38
.byte   30,   30,   31,   34,   35,   38,   38,   39
.byte   30,   31,   31,   34,   35,   38,   39,   39
.byte   31,   31,   31,   35,   35,   39,   39,   39
.byte   31,   31,   31,   35,   35,   39,   39,   39
.byte   31,   31,   31,   35,   35,   39,   39,   39
.byte   31,   31,   31,   35,   35,   39,   39,   39
.byte   31,   31,   31,   35,   35,   39,   39,   39
.byte   31,   31,   31,   35,   35,   39,   39,   39
.byte   31,   31,   32,   35,   36,   39,   39,   40
.byte   35,   32,   32,   39,   36,   43,   40,   40
.byte   32,   32,   32,   36,   36,   40,   40,   40
.byte   32,   32,   32,   36,   36,   40,   40,   40
.byte   32,   32,   32,   36,   36,   40,   40,   40
.byte   32,   32,   32,   36,   36,   40,   40,   40
.byte   32,   32,   32,   36,   36,   40,   40,   40
.byte   32,   32,   32,   36,   36,   40,   40,   40
.byte   32,   32,   33,   36,   37,   40,   40,   41
.byte   32,   33,   33,   36,   37,   40,   41,   41
.byte   33,   33,   33,   37,   37,   41,   41,   41
.byte   33,   33,   33,   37,   37,   41,   41,   41
.byte   33,   33,   33,   37,   37,   41,   41,   41
.byte   33,   33,   33,   37,   37,   41,   41,   41
.byte   33,   33,   33,   37,   37,   41,   41,   41
.byte   33,   33,   33,   37,   37,   41,   41,   41
.byte   33,   33,   34,   37,   38,   41,   41,   42
.byte   33,   34,   34,   37,   38,   41,   42,   42
.byte   34,   34,   34,   38,   38,   42,   42,   42
.byte   34,   34,   34,   38,   38,   42,   42,   42
.byte   34,   34,   34,   38,   38,   42,   42,   42
.byte   34,   34,   34,   38,   38,   42,   42,   42
.byte   34,   34,   34,   38,   38,   42,   42,   42
.byte   34,   34,   34,   38,   38,   42,   42,   42
.byte   34,   34,   35,   38,   39,   42,   42,   43
.byte   34,   35,   35,   38,   39,   42,   43,   43
.byte   35,   35,   35,   39,   39,   43,   43,   43
.byte   35,   35,   35,   39,   39,   43,   43,   43
.byte   35,   35,   35,   39,   39,   43,   43,   43
.byte   35,   35,   35,   39,   39,   43,   43,   43
.byte   35,   35,   35,   39,   39,   43,   43,   43
.byte   35,   35,   35,   39,   39,   43,   43,   43
.byte   35,   35,   36,   39,   40,   43,   43,   44
.byte   39,   36,   36,   43,   40,   47,   44,   44
.byte   36,   36,   36,   40,   40,   44,   44,   44
.byte   36,   36,   36,   40,   40,   44,   44,   44
.byte   36,   36,   36,   40,   40,   44,   44,   44
.byte   36,   36,   36,   40,   40,   44,   44,   44
.byte   36,   36,   36,   40,   40,   44,   44,   44
.byte   36,   36,   36,   40,   40,   44,   44,   44
.byte   36,   36,   37,   40,   41,   44,   44,   45
.byte   36,   37,   37,   40,   41,   44,   45,   45
.byte   37,   37,   37,   41,   41,   45,   45,   45
.byte   37,   37,   37,   41,   41,   45,   45,   45
.byte   37,   37,   37,   41,   41,   45,   45,   45
.byte   37,   37,   37,   41,   41,   45,   45,   45
.byte   37,   37,   37,   41,   41,   45,   45,   45
.byte   37,   37,   37,   41,   41,   45,   45,   45
.byte   37,   37,   38,   41,   42,   45,   45,   46
.byte   37,   38,   38,   41,   42,   45,   46,   46
.byte   38,   38,   38,   42,   42,   46,   46,   46
.byte   38,   38,   38,   42,   42,   46,   46,   46
.byte   38,   38,   38,   42,   42,   46,   46,   46
.byte   38,   38,   38,   42,   42,   46,   46,   46
.byte   38,   38,   38,   42,   42,   46,   46,   46
.byte   38,   38,   38,   42,   42,   46,   46,   46
.byte   38,   38,   39,   42,   43,   46,   46,   47
.byte   38,   39,   39,   42,   43,   46,   47,   47
.byte   39,   39,   39,   43,   43,   47,   47,   47
.byte   39,   39,   39,   43,   43,   47,   47,   47
.byte   39,   39,   39,   43,   43,   47,   47,   47
.byte   39,   39,   39,   43,   43,   47,   47,   47
.byte   39,   39,   39,   43,   43,   47,   47,   47
.byte   39,   39,   39,   43,   43,   47,   47,   47
.byte   39,   39,   40,   43,   44,   47,   47,   48
.byte   43,   40,   40,   47,   44,   51,   48,   48
.byte   40,   40,   40,   44,   44,   48,   48,   48
.byte   40,   40,   40,   44,   44,   48,   48,   48
.byte   40,   40,   40,   44,   44,   48,   48,   48
.byte   40,   40,   40,   44,   44,   48,   48,   48
.byte   40,   40,   40,   44,   44,   48,   48,   48
.byte   40,   40,   40,   44,   44,   48,   48,   48
.byte   40,   40,   41,   44,   45,   48,   48,   49
.byte   40,   41,   41,   44,   45,   48,   49,   49
.byte   41,   41,   41,   45,   45,   49,   49,   49
.byte   41,   41,   41,   45,   45,   49,   49,   49
.byte   41,   41,   41,   45,   45,   49,   49,   49
.byte   41,   41,   41,   45,   45,   49,   49,   49
.byte   41,   41,   41,   45,   45,   49,   49,   49
.byte   41,   41,   41,   45,   45,   49,   49,   49
.byte   41,   41,   42,   45,   46,   49,   49,   50
.byte   41,   42,   42,   45,   46,   49,   50,   50
.byte   42,   42,   42,   46,   46,   50,   50,   50
.byte   42,   42,   42,   46,   46,   50,   50,   50
.byte   42,   42,   42,   46,   46,   50,   50,   50
.byte   42,   42,   42,   46,   46,   50,   50,   50
.byte   42,   42,   42,   46,   46,   50,   50,   50
.byte   42,   42,   42,   46,   46,   50,   50,   50
.byte   42,   42,   43,   46,   47,   50,   50,   51
.byte   42,   43,   43,   46,   47,   50,   51,   51
.byte   43,   43,   43,   47,   47,   51,   51,   51
.byte   43,   43,   43,   47,   47,   51,   51,   51
.byte   43,   43,   43,   47,   47,   51,   51,   51
.byte   43,   43,   43,   47,   47,   51,   51,   51
.byte   43,   43,   43,   47,   47,   51,   51,   51
.byte   43,   43,   43,   47,   47,   51,   51,   51
.byte   43,   43,   44,   47,   48,   51,   51,   52
.byte   47,   44,   44,   51,   48,   55,   52,   52
.byte   44,   44,   44,   48,   48,   52,   52,   52
.byte   44,   44,   44,   48,   48,   52,   52,   52
.byte   44,   44,   44,   48,   48,   52,   52,   52
.byte   44,   44,   44,   48,   48,   52,   52,   52
.byte   44,   44,   44,   48,   48,   52,   52,   52
.byte   44,   44,   44,   48,   48,   52,   52,   52
.byte   44,   44,   45,   48,   49,   52,   52,   53
.byte   44,   45,   45,   48,   49,   52,   53,   53
.byte   45,   45,   45,   49,   49,   53,   53,   53
.byte   45,   45,   45,   49,   49,   53,   53,   53
.byte   45,   45,   45,   49,   49,   53,   53,   53
.byte   45,   45,   45,   49,   49,   53,   53,   53
.byte   45,   45,   45,   49,   49,   53,   53,   53
.byte   45,   45,   45,   49,   49,   53,   53,   53
.byte   45,   45,   46,   49,   50,   53,   53,   54
.byte   45,   46,   46,   49,   50,   53,   54,   54
.byte   46,   46,   46,   50,   50,   54,   54,   54
.byte   46,   46,   46,   50,   50,   54,   54,   54
.byte   46,   46,   46,   50,   50,   54,   54,   54
.byte   46,   46,   46,   50,   50,   54,   54,   54
.byte   46,   46,   46,   50,   50,   54,   54,   54
.byte   46,   46,   46,   50,   50,   54,   54,   54
.byte   46,   46,   47,   50,   51,   54,   54,   55
.byte   46,   47,   47,   50,   51,   54,   55,   55
.byte   47,   47,   47,   51,   51,   55,   55,   55
.byte   47,   47,   47,   51,   51,   55,   55,   55
.byte   47,   47,   47,   51,   51,   55,   55,   55
.byte   47,   47,   47,   51,   51,   55,   55,   55
.byte   47,   47,   47,   51,   51,   55,   55,   55
.byte   47,   47,   47,   51,   51,   55,   55,   55
.byte   47,   47,   48,   51,   52,   55,   55,   56
.byte   51,   48,   48,   55,   52,   59,   56,   56
.byte   48,   48,   48,   52,   52,   56,   56,   56
.byte   48,   48,   48,   52,   52,   56,   56,   56
.byte   48,   48,   48,   52,   52,   56,   56,   56
.byte   48,   48,   48,   52,   52,   56,   56,   56
.byte   48,   48,   48,   52,   52,   56,   56,   56
.byte   48,   48,   48,   52,   52,   56,   56,   56
.byte   48,   48,   49,   52,   53,   56,   56,   57
.byte   48,   49,   49,   52,   53,   56,   57,   57
.byte   49,   49,   49,   53,   53,   57,   57,   57
.byte   49,   49,   49,   53,   53,   57,   57,   57
.byte   49,   49,   49,   53,   53,   57,   57,   57
.byte   49,   49,   49,   53,   53,   57,   57,   57
.byte   49,   49,   49,   53,   53,   57,   57,   57
.byte   49,   49,   49,   53,   53,   57,   57,   57
.byte   49,   49,   50,   53,   54,   57,   57,   58
.byte   49,   50,   50,   53,   54,   57,   58,   58
.byte   50,   50,   50,   54,   54,   58,   58,   58
.byte   50,   50,   50,   54,   54,   58,   58,   58
.byte   50,   50,   50,   54,   54,   58,   58,   58
.byte   50,   50,   50,   54,   54,   58,   58,   58
.byte   50,   50,   50,   54,   54,   58,   58,   58
.byte   50,   50,   50,   54,   54,   58,   58,   58
.byte   50,   50,   51,   54,   55,   58,   58,   59
.byte   50,   51,   51,   54,   55,   58,   59,   59
.byte   51,   51,   51,   55,   55,   59,   59,   59
.byte   51,   51,   51,   55,   55,   59,   59,   59
.byte   51,   51,   51,   55,   55,   59,   59,   59
.byte   51,   51,   51,   55,   55,   59,   59,   59
.byte   51,   51,   51,   55,   55,   59,   59,   59
.byte   51,   51,   51,   55,   55,   59,   59,   59
.byte   51,   51,   52,   55,   56,   59,   59,   60
.byte   55,   52,   52,   59,   56,   63,   60,   60
.byte   52,   52,   52,   56,   56,   60,   60,   60
.byte   52,   52,   52,   56,   56,   60,   60,   60
.byte   52,   52,   52,   56,   56,   60,   60,   60
.byte   52,   52,   52,   56,   56,   60,   60,   60
.byte   52,   52,   52,   56,   56,   60,   60,   60
.byte   52,   52,   52,   56,   56,   60,   60,   60
.byte   52,   52,   53,   56,   57,   60,   60,   61
.byte   52,   53,   53,   56,   57,   60,   61,   61
.byte   53,   53,   53,   57,   57,   61,   61,   61
.byte   53,   53,   53,   57,   57,   61,   61,   61
.byte   53,   53,   53,   57,   57,   61,   61,   61
.byte   53,   53,   53,   57,   57,   61,   61,   61
.byte   53,   53,   53,   57,   57,   61,   61,   61
.byte   53,   53,   53,   57,   57,   61,   61,   61
.byte   53,   53,   54,   57,   58,   61,   61,   62
.byte   53,   54,   54,   57,   58,   61,   62,   62
.byte   54,   54,   54,   58,   58,   62,   62,   62
.byte   54,   54,   54,   58,   58,   62,   62,   62
.byte   54,   54,   54,   58,   58,   62,   62,   62
.byte   54,   54,   54,   58,   58,   62,   62,   62
.byte   54,   54,   54,   58,   58,   62,   62,   62
.byte   54,   54,   54,   58,   58,   62,   62,   62
.byte   54,   54,   55,   58,   59,   62,   62,   63
.byte   54,   55,   55,   58,   59,   62,   63,   63
.byte   55,   55,   55,   59,   59,   63,   63,   63
.byte   55,   55,   55,   59,   59,   63,   63,   63
.byte   55,   55,   55,   59,   59,   63,   63,   63
.byte   55,   55,   55,   59,   59,   63,   63,   63
.byte   55,   55,   55,   59,   59,   63,   63,   63
.byte   55,   55,   55,   59,   59,   63,   63,   63
.byte   55,   55,   56,   59,   60,   63,   63,   64
.byte   59,   56,   56,   63,   60,   67,   64,   64
.byte   56,   56,   56,   60,   60,   64,   64,   64
.byte   56,   56,   56,   60,   60,   64,   64,   64
.byte   56,   56,   56,   60,   60,   64,   64,   64
.byte   56,   56,   56,   60,   60,   64,   64,   64
.byte   56,   56,   56,   60,   60,   64,   64,   64
.byte   56,   56,   56,   60,   60,   64,   64,   64
.byte   56,   56,   57,   60,   61,   64,   64,   65
.byte   56,   57,   57,   60,   61,   64,   65,   65
.byte   57,   57,   57,   61,   61,   65,   65,   65
.byte   57,   57,   57,   61,   61,   65,   65,   65
.byte   57,   57,   57,   61,   61,   65,   65,   65
.byte   57,   57,   57,   61,   61,   65,   65,   65
.byte   57,   57,   57,   61,   61,   65,   65,   65
.byte   57,   57,   57,   61,   61,   65,   65,   65
.byte   57,   57,   58,   61,   62,   65,   65,   66
.byte   57,   58,   58,   61,   62,   65,   66,   66
.byte   58,   58,   58,   62,   62,   66,   66,   66
.byte   58,   58,   58,   62,   62,   66,   66,   66
.byte   58,   58,   58,   62,   62,   66,   66,   66
.byte   58,   58,   58,   62,   62,   66,   66,   66
.byte   58,   58,   58,   62,   62,   66,   66,   66
.byte   58,   58,   58,   62,   62,   66,   66,   66
.byte   58,   58,   59,   62,   63,   66,   66,   67
.byte   58,   59,   59,   62,   63,   66,   67,   67
.byte   59,   59,   59,   63,   63,   67,   67,   67
.byte   59,   59,   59,   63,   63,   67,   67,   67
.byte   59,   59,   59,   63,   63,   67,   67,   67
.byte   59,   59,   59,   63,   63,   67,   67,   67
.byte   59,   59,   59,   63,   63,   67,   67,   67
.byte   59,   59,   59,   63,   63,   67,   67,   67
.byte   59,   59,   60,   63,   64,   67,   67,   68
.byte   63,   60,   60,   67,   64,   71,   68,   68
.byte   60,   60,   60,   64,   64,   68,   68,   68
.byte   60,   60,   60,   64,   64,   68,   68,   68
.byte   60,   60,   60,   64,   64,   68,   68,   68
.byte   60,   60,   60,   64,   64,   68,   68,   68
.byte   60,   60,   60,   64,   64,   68,   68,   68
.byte   60,   60,   60,   64,   64,   68,   68,   68
.byte   60,   60,   61,   64,   65,   68,   68,   69
.byte   60,   61,   61,   64,   65,   68,   69,   69
.byte   61,   61,   61,   65,   65,   69,   69,   69
.byte   61,   61,   61,   65,   65,   69,   69,   69
.byte   61,   61,   61,   65,   65,   69,   69,   69
.byte   61,   61,   61,   65,   65,   69,   69,   69
.byte   61,   61,   61,   65,   65,   69,   69,   69
.byte   61,   61,   61,   65,   65,   69,   69,   69
.byte   61,   61,   62,   65,   66,   69,   69,   70
.byte   61,   62,   62,   65,   66,   69,   70,   70
.byte   62,   62,   62,   66,   66,   70,   70,   70
.byte   62,   62,   62,   66,   66,   70,   70,   70
.byte   62,   62,   62,   66,   66,   70,   70,   70
.byte   62,   62,   62,   66,   66,   70,   70,   70
.byte   62,   62,   62,   66,   66,   70,   70,   70
.byte   62,   62,   62,   66,   66,   70,   70,   70
.byte   62,   62,   63,   66,   67,   70,   70,   71
.byte   62,   63,   63,   66,   67,   70,   71,   71
.byte   63,   63,   63,   67,   67,   71,   71,   71
.byte   63,   63,   63,   67,   67,   71,   71,   71
.byte   63,   63,   63,   67,   67,   71,   71,   71
.byte   63,   63,   63,   67,   67,   71,   71,   71
.byte   63,   63,   63,   67,   67,   71,   71,   71
.byte   63,   63,   63,   67,   67,   71,   71,   71
.byte   63,   63,   64,   67,   68,   71,   71,   72
.byte   67,   64,   64,   71,   68,   75,   72,   72
.byte   64,   64,   64,   68,   68,   72,   72,   72
.byte   64,   64,   64,   68,   68,   72,   72,   72
.byte   64,   64,   64,   68,   68,   72,   72,   72
.byte   64,   64,   64,   68,   68,   72,   72,   72
.byte   64,   64,   64,   68,   68,   72,   72,   72
.byte   64,   64,   64,   68,   68,   72,   72,   72
.byte   64,   64,   65,   68,   69,   72,   72,   73
.byte   64,   65,   65,   68,   69,   72,   73,   73
.byte   65,   65,   65,   69,   69,   73,   73,   73
.byte   65,   65,   65,   69,   69,   73,   73,   73
.byte   65,   65,   65,   69,   69,   73,   73,   73
.byte   65,   65,   65,   69,   69,   73,   73,   73
.byte   65,   65,   65,   69,   69,   73,   73,   73
.byte   65,   65,   65,   69,   69,   73,   73,   73
.byte   65,   65,   66,   69,   70,   73,   73,   74
.byte   65,   66,   66,   69,   70,   73,   74,   74
.byte   66,   66,   66,   70,   70,   74,   74,   74
.byte   66,   66,   66,   70,   70,   74,   74,   74
.byte   66,   66,   66,   70,   70,   74,   74,   74
.byte   66,   66,   66,   70,   70,   74,   74,   74
.byte   66,   66,   66,   70,   70,   74,   74,   74
.byte   66,   66,   66,   70,   70,   74,   74,   74
.byte   66,   66,   67,   70,   71,   74,   74,   75
.byte   66,   67,   67,   70,   71,   74,   75,   75
.byte   67,   67,   67,   71,   71,   75,   75,   75
.byte   67,   67,   67,   71,   71,   75,   75,   75
.byte   67,   67,   67,   71,   71,   75,   75,   75
.byte   67,   67,   67,   71,   71,   75,   75,   75
.byte   67,   67,   67,   71,   71,   75,   75,   75
.byte   67,   67,   67,   71,   71,   75,   75,   75
.byte   67,   67,   68,   71,   72,   75,   75,   76
.byte   71,   68,   68,   75,   72,   79,   76,   76
.byte   68,   68,   68,   72,   72,   76,   76,   76
.byte   68,   68,   68,   72,   72,   76,   76,   76
.byte   68,   68,   68,   72,   72,   76,   76,   76
.byte   68,   68,   68,   72,   72,   76,   76,   76
.byte   68,   68,   68,   72,   72,   76,   76,   76
.byte   68,   68,   68,   72,   72,   76,   76,   76
.byte   68,   68,   69,   72,   73,   76,   76,   77
.byte   68,   69,   69,   72,   73,   76,   77,   77
.byte   69,   69,   69,   73,   73,   77,   77,   77
.byte   69,   69,   69,   73,   73,   77,   77,   77
.byte   69,   69,   69,   73,   73,   77,   77,   77
.byte   69,   69,   69,   73,   73,   77,   77,   77
.byte   69,   69,   69,   73,   73,   77,   77,   77
.byte   69,   69,   69,   73,   73,   77,   77,   77
.byte   69,   69,   70,   73,   74,   77,   77,   78
.byte   69,   70,   70,   73,   74,   77,   78,   78
.byte   70,   70,   70,   74,   74,   78,   78,   78
.byte   70,   70,   70,   74,   74,   78,   78,   78
.byte   70,   70,   70,   74,   74,   78,   78,   78
.byte   70,   70,   70,   74,   74,   78,   78,   78
.byte   70,   70,   70,   74,   74,   78,   78,   78
.byte   70,   70,   70,   74,   74,   78,   78,   78
.byte   70,   70,   71,   74,   75,   78,   78,   79
.byte   70,   71,   71,   74,   75,   78,   79,   79
.byte   71,   71,   71,   75,   75,   79,   79,   79
.byte   71,   71,   71,   75,   75,   79,   79,   79
.byte   71,   71,   71,   75,   75,   79,   79,   79
.byte   71,   71,   71,   75,   75,   79,   79,   79
.byte   71,   71,   71,   75,   75,   79,   79,   79
.byte   71,   71,   71,   75,   75,   79,   79,   79
.byte   71,   71,   72,   75,   76,   79,   79,   80
.byte   75,   72,   72,   79,   76,   83,   80,   80
.byte   72,   72,   72,   76,   76,   80,   80,   80
.byte   72,   72,   72,   76,   76,   80,   80,   80
.byte   72,   72,   72,   76,   76,   80,   80,   80
.byte   72,   72,   72,   76,   76,   80,   80,   80
.byte   72,   72,   72,   76,   76,   80,   80,   80
.byte   72,   72,   72,   76,   76,   80,   80,   80
.byte   72,   72,   73,   76,   77,   80,   80,   81
.byte   72,   73,   73,   76,   77,   80,   81,   81
.byte   73,   73,   73,   77,   77,   81,   81,   81
.byte   73,   73,   73,   77,   77,   81,   81,   81
.byte   73,   73,   73,   77,   77,   81,   81,   81
.byte   73,   73,   73,   77,   77,   81,   81,   81
.byte   73,   73,   73,   77,   77,   81,   81,   81
.byte   73,   73,   73,   77,   77,   81,   81,   81
.byte   73,   73,   74,   77,   78,   81,   81,   82
.byte   73,   74,   74,   77,   78,   81,   82,   82
.byte   74,   74,   74,   78,   78,   82,   82,   82
.byte   74,   74,   74,   78,   78,   82,   82,   82
.byte   74,   74,   74,   78,   78,   82,   82,   82
.byte   74,   74,   74,   78,   78,   82,   82,   82
.byte   74,   74,   74,   78,   78,   82,   82,   82
.byte   74,   74,   74,   78,   78,   82,   82,   82
.byte   74,   74,   75,   78,   79,   82,   82,   83
.byte   74,   75,   75,   78,   79,   82,   83,   83
.byte   75,   75,   75,   79,   79,   83,   83,   83
.byte   75,   75,   75,   79,   79,   83,   83,   83
.byte   75,   75,   75,   79,   79,   83,   83,   83
.byte   75,   75,   75,   79,   79,   83,   83,   83
.byte   75,   75,   75,   79,   79,   83,   83,   83
.byte   75,   75,   75,   79,   79,   83,   83,   83
.byte   75,   75,   76,   79,   80,   83,   83,   84
.byte   79,   76,   76,   83,   80,   87,   84,   84
.byte   76,   76,   76,   80,   80,   84,   84,   84
.byte   76,   76,   76,   80,   80,   84,   84,   84
.byte   76,   76,   76,   80,   80,   84,   84,   84
.byte   76,   76,   76,   80,   80,   84,   84,   84
.byte   76,   76,   76,   80,   80,   84,   84,   84
.byte   76,   76,   76,   80,   80,   84,   84,   84
.byte   76,   76,   77,   80,   81,   84,   84,   85
.byte   76,   77,   77,   80,   81,   84,   85,   85
.byte   77,   77,   77,   81,   81,   85,   85,   85
.byte   77,   77,   77,   81,   81,   85,   85,   85
.byte   77,   77,   77,   81,   81,   85,   85,   85
.byte   77,   77,   77,   81,   81,   85,   85,   85
.byte   77,   77,   77,   81,   81,   85,   85,   85
.byte   77,   77,   77,   81,   81,   85,   85,   85
.byte   77,   77,   78,   81,   82,   85,   85,   86
.byte   77,   78,   78,   81,   82,   85,   86,   86
.byte   78,   78,   78,   82,   82,   86,   86,   86
.byte   78,   78,   78,   82,   82,   86,   86,   86
.byte   78,   78,   78,   82,   82,   86,   86,   86
.byte   78,   78,   78,   82,   82,   86,   86,   86
.byte   78,   78,   78,   82,   82,   86,   86,   86
.byte   78,   78,   78,   82,   82,   86,   86,   86
.byte   78,   78,   79,   82,   83,   86,   86,   87
.byte   78,   79,   79,   82,   83,   86,   87,   87
.byte   79,   79,   79,   83,   83,   87,   87,   87
.byte   79,   79,   79,   83,   83,   87,   87,   87
.byte   79,   79,   79,   83,   83,   87,   87,   87
.byte   79,   79,   79,   83,   83,   87,   87,   87
.byte   79,   79,   79,   83,   83,   87,   87,   87
.byte   79,   79,   79,   83,   83,   87,   87,   87
.byte   79,   79,   80,   83,   84,   87,   87,   88
.byte   83,   80,   80,   87,   84,   91,   88,   88
.byte   80,   80,   80,   84,   84,   88,   88,   88
.byte   80,   80,   80,   84,   84,   88,   88,   88
.byte   80,   80,   80,   84,   84,   88,   88,   88
.byte   80,   80,   80,   84,   84,   88,   88,   88
.byte   80,   80,   80,   84,   84,   88,   88,   88
.byte   80,   80,   80,   84,   84,   88,   88,   88
.byte   80,   80,   81,   84,   85,   88,   88,   89
.byte   80,   81,   81,   84,   85,   88,   89,   89
.byte   81,   81,   81,   85,   85,   89,   89,   89
.byte   81,   81,   81,   85,   85,   89,   89,   89
.byte   81,   81,   81,   85,   85,   89,   89,   89
.byte   81,   81,   81,   85,   85,   89,   89,   89
.byte   81,   81,   81,   85,   85,   89,   89,   89
.byte   81,   81,   81,   85,   85,   89,   89,   89
.byte   81,   81,   82,   85,   86,   89,   89,   90
.byte   81,   82,   82,   85,   86,   89,   90,   90
.byte   82,   82,   82,   86,   86,   90,   90,   90
.byte   82,   82,   82,   86,   86,   90,   90,   90
.byte   82,   82,   82,   86,   86,   90,   90,   90
.byte   82,   82,   82,   86,   86,   90,   90,   90
.byte   82,   82,   82,   86,   86,   90,   90,   90
.byte   82,   82,   82,   86,   86,   90,   90,   90
.byte   82,   82,   83,   86,   87,   90,   90,   91
.byte   82,   83,   83,   86,   87,   90,   91,   91
.byte   83,   83,   83,   87,   87,   91,   91,   91
.byte   83,   83,   83,   87,   87,   91,   91,   91
.byte   83,   83,   83,   87,   87,   91,   91,   91
.byte   83,   83,   83,   87,   87,   91,   91,   91
.byte   83,   83,   83,   87,   87,   91,   91,   91
.byte   83,   83,   83,   87,   87,   91,   91,   91
.byte   83,   83,   84,   87,   88,   91,   91,   92
.byte   87,   84,   84,   91,   88,   95,   92,   92
.byte   84,   84,   84,   88,   88,   92,   92,   92
.byte   84,   84,   84,   88,   88,   92,   92,   92
.byte   84,   84,   84,   88,   88,   92,   92,   92
.byte   84,   84,   84,   88,   88,   92,   92,   92
.byte   84,   84,   84,   88,   88,   92,   92,   92
.byte   84,   84,   84,   88,   88,   92,   92,   92
.byte   84,   84,   85,   88,   89,   92,   92,   93
.byte   84,   85,   85,   88,   89,   92,   93,   93
.byte   85,   85,   85,   89,   89,   93,   93,   93
.byte   85,   85,   85,   89,   89,   93,   93,   93
.byte   85,   85,   85,   89,   89,   93,   93,   93
.byte   85,   85,   85,   89,   89,   93,   93,   93
.byte   85,   85,   85,   89,   89,   93,   93,   93
.byte   85,   85,   85,   89,   89,   93,   93,   93
.byte   85,   85,   86,   89,   90,   93,   93,   94
.byte   85,   86,   86,   89,   90,   93,   94,   94
.byte   86,   86,   86,   90,   90,   94,   94,   94
.byte   86,   86,   86,   90,   90,   94,   94,   94
.byte   86,   86,   86,   90,   90,   94,   94,   94
.byte   86,   86,   86,   90,   90,   94,   94,   94
.byte   86,   86,   86,   90,   90,   94,   94,   94
.byte   86,   86,   86,   90,   90,   94,   94,   94
.byte   86,   86,   87,   90,   91,   94,   94,   95
.byte   86,   87,   87,   90,   91,   94,   95,   95
.byte   87,   87,   87,   91,   91,   95,   95,   95
.byte   87,   87,   87,   91,   91,   95,   95,   95
.byte   87,   87,   87,   91,   91,   95,   95,   95
.byte   87,   87,   87,   91,   91,   95,   95,   95
.byte   87,   87,   87,   91,   91,   95,   95,   95
.byte   87,   87,   87,   91,   91,   95,   95,   95
.byte   87,   87,   88,   91,   92,   95,   95,   96
.byte   91,   88,   88,   95,   92,   99,   96,   96
.byte   88,   88,   88,   92,   92,   96,   96,   96
.byte   88,   88,   88,   92,   92,   96,   96,   96
.byte   88,   88,   88,   92,   92,   96,   96,   96
.byte   88,   88,   88,   92,   92,   96,   96,   96
.byte   88,   88,   88,   92,   92,   96,   96,   96
.byte   88,   88,   88,   92,   92,   96,   96,   96
.byte   88,   88,   89,   92,   93,   96,   96,   97
.byte   88,   89,   89,   92,   93,   96,   97,   97
.byte   89,   89,   89,   93,   93,   97,   97,   97
.byte   89,   89,   89,   93,   93,   97,   97,   97
.byte   89,   89,   89,   93,   93,   97,   97,   97
.byte   89,   89,   89,   93,   93,   97,   97,   97
.byte   89,   89,   89,   93,   93,   97,   97,   97
.byte   89,   89,   89,   93,   93,   97,   97,   97
.byte   89,   89,   90,   93,   94,   97,   97,   98
.byte   89,   90,   90,   93,   94,   97,   98,   98
.byte   90,   90,   90,   94,   94,   98,   98,   98
.byte   90,   90,   90,   94,   94,   98,   98,   98
.byte   90,   90,   90,   94,   94,   98,   98,   98
.byte   90,   90,   90,   94,   94,   98,   98,   98
.byte   90,   90,   90,   94,   94,   98,   98,   98
.byte   90,   90,   90,   94,   94,   98,   98,   98
.byte   90,   90,   91,   94,   95,   98,   98,   99
.byte   90,   91,   91,   94,   95,   98,   99,   99
.byte   91,   91,   91,   95,   95,   99,   99,   99
.byte   91,   91,   91,   95,   95,   99,   99,   99
.byte   91,   91,   91,   95,   95,   99,   99,   99
.byte   91,   91,   91,   95,   95,   99,   99,   99
.byte   91,   91,   91,   95,   95,   99,   99,   99
.byte   91,   91,   91,   95,   95,   99,   99,   99
.byte   91,   91,   92,   95,   96,   99,   99,  100
.byte   95,   92,   92,   99,   96,  103,  100,  100
.byte   92,   92,   92,   96,   96,  100,  100,  100
.byte   92,   92,   92,   96,   96,  100,  100,  100
.byte   92,   92,   92,   96,   96,  100,  100,  100
.byte   92,   92,   92,   96,   96,  100,  100,  100
.byte   92,   92,   92,   96,   96,  100,  100,  100
.byte   92,   92,   92,   96,   96,  100,  100,  100
.byte   92,   92,   93,   96,   97,  100,  100,  101
.byte   92,   93,   93,   96,   97,  100,  101,  101
.byte   93,   93,   93,   97,   97,  101,  101,  101
.byte   93,   93,   93,   97,   97,  101,  101,  101
.byte   93,   93,   93,   97,   97,  101,  101,  101
.byte   93,   93,   93,   97,   97,  101,  101,  101
.byte   93,   93,   93,   97,   97,  101,  101,  101
.byte   93,   93,   93,   97,   97,  101,  101,  101
.byte   93,   93,   94,   97,   98,  101,  101,  102
.byte   93,   94,   94,   97,   98,  101,  102,  102
.byte   94,   94,   94,   98,   98,  102,  102,  102
.byte   94,   94,   94,   98,   98,  102,  102,  102
.byte   94,   94,   94,   98,   98,  102,  102,  102
.byte   94,   94,   94,   98,   98,  102,  102,  102
.byte   94,   94,   94,   98,   98,  102,  102,  102
.byte   94,   94,   94,   98,   98,  102,  102,  102
.byte   94,   94,   95,   98,   99,  102,  102,  103
.byte   94,   95,   95,   98,   99,  102,  103,  103
.byte   95,   95,   95,   99,   99,  103,  103,  103
.byte   95,   95,   95,   99,   99,  103,  103,  103
.byte   95,   95,   95,   99,   99,  103,  103,  103
.byte   95,   95,   95,   99,   99,  103,  103,  103
.byte   95,   95,   95,   99,   99,  103,  103,  103
.byte   95,   95,   95,   99,   99,  103,  103,  103
.byte   95,   95,   96,   99,  100,  103,  103,  104
.byte   99,   96,   96,  103,  100,  107,  104,  104
.byte   96,   96,   96,  100,  100,  104,  104,  104
.byte   96,   96,   96,  100,  100,  104,  104,  104
.byte   96,   96,   96,  100,  100,  104,  104,  104
.byte   96,   96,   96,  100,  100,  104,  104,  104
.byte   96,   96,   96,  100,  100,  104,  104,  104
.byte   96,   96,   96,  100,  100,  104,  104,  104
.byte   96,   96,   97,  100,  101,  104,  104,  105
.byte   96,   97,   97,  100,  101,  104,  105,  105
.byte   97,   97,   97,  101,  101,  105,  105,  105
.byte   97,   97,   97,  101,  101,  105,  105,  105
.byte   97,   97,   97,  101,  101,  105,  105,  105
.byte   97,   97,   97,  101,  101,  105,  105,  105
.byte   97,   97,   97,  101,  101,  105,  105,  105
.byte   97,   97,   97,  101,  101,  105,  105,  105
.byte   97,   97,   98,  101,  102,  105,  105,  106
.byte   97,   98,   98,  101,  102,  105,  106,  106
.byte   98,   98,   98,  102,  102,  106,  106,  106
.byte   98,   98,   98,  102,  102,  106,  106,  106
.byte   98,   98,   98,  102,  102,  106,  106,  106
.byte   98,   98,   98,  102,  102,  106,  106,  106
.byte   98,   98,   98,  102,  102,  106,  106,  106
.byte   98,   98,   98,  102,  102,  106,  106,  106
.byte   98,   98,   99,  102,  103,  106,  106,  107
.byte   98,   99,   99,  102,  103,  106,  107,  107
.byte   99,   99,   99,  103,  103,  107,  107,  107
.byte   99,   99,   99,  103,  103,  107,  107,  107
.byte   99,   99,   99,  103,  103,  107,  107,  107
.byte   99,   99,   99,  103,  103,  107,  107,  107
.byte   99,   99,   99,  103,  103,  107,  107,  107
.byte   99,   99,   99,  103,  103,  107,  107,  107
.byte   99,   99,  100,  103,  104,  107,  107,  108
.byte  103,  100,  100,  107,  104,  111,  108,  108
.byte  100,  100,  100,  104,  104,  108,  108,  108
.byte  100,  100,  100,  104,  104,  108,  108,  108
.byte  100,  100,  100,  104,  104,  108,  108,  108
.byte  100,  100,  100,  104,  104,  108,  108,  108
.byte  100,  100,  100,  104,  104,  108,  108,  108
.byte  100,  100,  100,  104,  104,  108,  108,  108
.byte  100,  100,  101,  104,  105,  108,  108,  109
.byte  100,  101,  101,  104,  105,  108,  109,  109
.byte  101,  101,  101,  105,  105,  109,  109,  109
.byte  101,  101,  101,  105,  105,  109,  109,  109
.byte  101,  101,  101,  105,  105,  109,  109,  109
.byte  101,  101,  101,  105,  105,  109,  109,  109
.byte  101,  101,  101,  105,  105,  109,  109,  109
.byte  101,  101,  101,  105,  105,  109,  109,  109
.byte  101,  101,  102,  105,  106,  109,  109,  110
.byte  101,  102,  102,  105,  106,  109,  110,  110
.byte  102,  102,  102,  106,  106,  110,  110,  110
.byte  102,  102,  102,  106,  106,  110,  110,  110
.byte  102,  102,  102,  106,  106,  110,  110,  110
.byte  102,  102,  102,  106,  106,  110,  110,  110
.byte  102,  102,  102,  106,  106,  110,  110,  110
.byte  102,  102,  102,  106,  106,  110,  110,  110
.byte  102,  102,  103,  106,  107,  110,  110,  111
.byte  102,  103,  103,  106,  107,  110,  111,  111
.byte  103,  103,  103,  107,  107,  111,  111,  111
.byte  103,  103,  103,  107,  107,  111,  111,  111
.byte  103,  103,  103,  107,  107,  111,  111,  111
.byte  103,  103,  103,  107,  107,  111,  111,  111
.byte  103,  103,  103,  107,  107,  111,  111,  111
.byte  103,  103,  103,  107,  107,  111,  111,  111
.byte  103,  103,  104,  107,  108,  111,  111,  112
.byte  107,  104,  104,  111,  108,  115,  112,  112
.byte  104,  104,  104,  108,  108,  112,  112,  112
.byte  104,  104,  104,  108,  108,  112,  112,  112
.byte  104,  104,  104,  108,  108,  112,  112,  112
.byte  104,  104,  104,  108,  108,  112,  112,  112
.byte  104,  104,  104,  108,  108,  112,  112,  112
.byte  104,  104,  104,  108,  108,  112,  112,  112
.byte  104,  104,  105,  108,  109,  112,  112,  113
.byte  104,  105,  105,  108,  109,  112,  113,  113
.byte  105,  105,  105,  109,  109,  113,  113,  113
.byte  105,  105,  105,  109,  109,  113,  113,  113
.byte  105,  105,  105,  109,  109,  113,  113,  113
.byte  105,  105,  105,  109,  109,  113,  113,  113
.byte  105,  105,  105,  109,  109,  113,  113,  113
.byte  105,  105,  105,  109,  109,  113,  113,  113
.byte  105,  105,  106,  109,  110,  113,  113,  114
.byte  105,  106,  106,  109,  110,  113,  114,  114
.byte  106,  106,  106,  110,  110,  114,  114,  114
.byte  106,  106,  106,  110,  110,  114,  114,  114
.byte  106,  106,  106,  110,  110,  114,  114,  114
.byte  106,  106,  106,  110,  110,  114,  114,  114
.byte  106,  106,  106,  110,  110,  114,  114,  114
.byte  106,  106,  106,  110,  110,  114,  114,  114
.byte  106,  106,  107,  110,  111,  114,  114,  115
.byte  106,  107,  107,  110,  111,  114,  115,  115
.byte  107,  107,  107,  111,  111,  115,  115,  115
.byte  107,  107,  107,  111,  111,  115,  115,  115
.byte  107,  107,  107,  111,  111,  115,  115,  115
.byte  107,  107,  107,  111,  111,  115,  115,  115
.byte  107,  107,  107,  111,  111,  115,  115,  115
.byte  107,  107,  107,  111,  111,  115,  115,  115
.byte  107,  107,  108,  111,  112,  115,  115,  116
.byte  111,  108,  108,  115,  112,  119,  116,  116
.byte  108,  108,  108,  112,  112,  116,  116,  116
.byte  108,  108,  108,  112,  112,  116,  116,  116
.byte  108,  108,  108,  112,  112,  116,  116,  116
.byte  108,  108,  108,  112,  112,  116,  116,  116
.byte  108,  108,  108,  112,  112,  116,  116,  116
.byte  108,  108,  108,  112,  112,  116,  116,  116
.byte  108,  108,  109,  112,  113,  116,  116,  117
.byte  108,  109,  109,  112,  113,  116,  117,  117
.byte  109,  109,  109,  113,  113,  117,  117,  117
.byte  109,  109,  109,  113,  113,  117,  117,  117
.byte  109,  109,  109,  113,  113,  117,  117,  117
.byte  109,  109,  109,  113,  113,  117,  117,  117
.byte  109,  109,  109,  113,  113,  117,  117,  117
.byte  109,  109,  109,  113,  113,  117,  117,  117
.byte  109,  109,  110,  113,  114,  117,  117,  118
.byte  109,  110,  110,  113,  114,  117,  118,  118
.byte  110,  110,  110,  114,  114,  118,  118,  118
.byte  110,  110,  110,  114,  114,  118,  118,  118
.byte  110,  110,  110,  114,  114,  118,  118,  118
.byte  110,  110,  110,  114,  114,  118,  118,  118
.byte  110,  110,  110,  114,  114,  118,  118,  118
.byte  110,  110,  110,  114,  114,  118,  118,  118
.byte  110,  110,  111,  114,  115,  118,  118,  119
.byte  110,  111,  111,  114,  115,  118,  119,  119
.byte  111,  111,  111,  115,  115,  119,  119,  119
.byte  111,  111,  111,  115,  115,  119,  119,  119
.byte  111,  111,  111,  115,  115,  119,  119,  119
.byte  111,  111,  111,  115,  115,  119,  119,  119
.byte  111,  111,  111,  115,  115,  119,  119,  119
.byte  111,  111,  111,  115,  115,  119,  119,  119
.byte  111,  111,  112,  115,  116,  119,  119,    0
.byte  115,  112,  112,  119,  116,    3,    0,    0
.byte  112,  112,  112,  116,  116,    0,    0,    0
.byte  112,  112,  112,  116,  116,    0,    0,    0
.byte  112,  112,  112,  116,  116,    0,    0,    0
.byte  112,  112,  112,  116,  116,    0,    0,    0
.byte  112,  112,  112,  116,  116,    0,    0,    0
.byte  112,  112,  112,  116,  116,    0,    0,    0
.byte  112,  112,  113,  116,  117,    0,    0,    1
.byte  112,  113,  113,  116,  117,    0,    1,    1
.byte  113,  113,  113,  117,  117,    1,    1,    1
.byte  113,  113,  113,  117,  117,    1,    1,    1
.byte  113,  113,  113,  117,  117,    1,    1,    1
.byte  113,  113,  113,  117,  117,    1,    1,    1
.byte  113,  113,  113,  117,  117,    1,    1,    1
.byte  113,  113,  113,  117,  117,    1,    1,    1
.byte  113,  113,  114,  117,  118,    1,    1,    2
.byte  113,  114,  114,  117,  118,    1,    2,    2
.byte  114,  114,  114,  118,  118,    2,    2,    2
.byte  114,  114,  114,  118,  118,    2,    2,    2
.byte  114,  114,  114,  118,  118,    2,    2,    2
.byte  114,  114,  114,  118,  118,    2,    2,    2
.byte  114,  114,  114,  118,  118,    2,    2,    2
.byte  114,  114,  114,  118,  118,    2,    2,    2
.byte  114,  114,  115,  118,  119,    2,    2,    3
.byte  114,  115,  115,  118,  119,    2,    3,    3
.byte  115,  115,  115,  119,  119,    3,    3,    3
.byte  115,  115,  115,  119,  119,    3,    3,    3
.byte  115,  115,  115,  119,  119,    3,    3,    3
.byte  115,  115,  115,  119,  119,    3,    3,    3
.byte  115,  115,  115,  119,  119,    3,    3,    3
.byte  115,  115,  115,  119,  119,    3,    3,    3
.byte  115,  115,  116,  119,    0,    3,    3,    4
