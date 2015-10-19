INCLUDE "constants.asm"
INCLUDE "wram.asm"

; rst vectors go unused
SECTION "rst00",HOME[0]
    ret

SECTION "rst08",HOME[8]
    ret

SECTION "rst10",HOME[$10]
    ret

SECTION "rst18",HOME[$18]
    ret

SECTION "rst20",HOME[$20]
    ret

SECTION "rst30",HOME[$30]
    ret

SECTION "rst38",HOME[$38]
    ret

SECTION "vblank",HOME[$40]
	jp VBlankHandler
SECTION "lcdc",HOME[$48]
    ;reti
    jp HBlankHandler
SECTION "timer",HOME[$50]
	reti
SECTION "serial",HOME[$58]
	reti
SECTION "joypad",HOME[$60]
	reti

SECTION "bank0",HOME[$61]

SECTION "romheader",HOME[$100]
    nop
    jp Start150

Section "start",HOME[$150]

Start150:
    jp Start

VBlankHandler:
    push af
    push bc
    push de
    push hl
    call FastVblank
    call $FF80
    call ReadJoypadRegister
    ld hl, H_TIMER
    inc [hl]
    call GetRNG
    lda [rSCY], [H_SCY]
    lda [rSCX], [H_SCX]
    lda [H_PASTVBLANK], 1
    pop hl
    pop de
    pop bc
    pop af
    reti
    
HBlankHandler:
    reti

FastVblank:
    ld a, [H_VBLANKPART]
    and a
    ret z
    dec a
    jr z, .bottom
    ld [wSPtmp], sp
    ld sp, W_TILEMAP
    ld hl, $9800
    ld b, 5
    jr .picked
.bottom
    ld [wSPtmp], sp
    ld sp, W_TILEMAP + 20 * $a
    ld hl, $9800+$140
    ld b, 4
.picked
.loop
rept 2
    popdetohli 10
    ld de, 12
    add hl, de
endr
    dec b
    jr nz, .loop
        
    ld hl, H_VBLANKPART
    dec [hl]
    
    ld a, [wSPtmp]
    ld l, a
    ld a, [wSPtmp+1]
    ld h, a
    ld sp, hl
    ret
    
INCLUDE "common.asm"
INCLUDE "vwf.asm"

Start:
    di
    
    ; palettes
    ld a, %11100100
    ld [rBGP], a
    ld a, %11010000
    ld [rOBP0], a
    
    xor a
    ld [H_SCX], a
    ld [H_SCY], a
    
    ld a, %11000011
    ld [rLCDC], a
    ld a, %00001000
    ld [rSTAT], a
    
    ei
    
    call DisableLCD
    
    ; seed the RNG
    ld hl, $C000
    ld l, [hl]
    ld a, [hl]
    push af
    
    ; fill the memory with zeroes
    ld hl, $C000
.loop
    ld a, 0
    ld [hli], a
    ld a, h
    cp $e0
    jr nz, .loop
    
    pop af
    ; set up the stack pointer
    ld sp, $dffe
    push af

    ld hl, $ff80
.loop2
    ld a, 0
    ld [hli], a
    ld a, h
    cp $00
    jr nz, .loop2
    
    pop af
    ld [H_RNG1], a
    
    call WriteDMACodeToHRAM
    
    ; set up ingame graphics
    copy $8f00, BoxChars, $0100
        
    call EnableLCD
    
    xor a
    ld [$ffff], a
    ld a, %00000011
    ld [$ffff], a
    ; TODO clear interrupt latches
    ei
    halt
    
    ;call WaitForKey
    
    call ClearTilemap
    call DisableLCD
    call EnableLCD
    jp InitGame

NewGameMenu:
    db 1, 4, 4, 8 ; y, x, h, w
    db $30 ; tile gfx index
    db 0 ; options are inlined
    db 3 ; number of options
    db "Tutorial?@"
    db "Start?@"
    db "Options?@"
    db 1 ; callbacks
    dw Tutorial
    dw StartGame
    dw Options

InitGame:
    call ClearTilemap
    printstatic $00, $0f, 0, 6, "Flora GB!@"
    printstatic $10, $20, $11, 0, "Very early test build. By Sanky.@"
    xor a
    ld [H_SCX], a
    ld [H_SCY], a
    refreshscreen
.here
    halt
    domenu NewGameMenu
    jr .here

Tutorial:
    printstatic $50, $10, $8, 0, "Tutorial not implemented...@"
    refreshscreen
    ret

StartGame:
    frame2
    call ClearTilemap
    refreshscreen
    frame2
    
    call DisableLCD
    
    copy $9010, Tileset
    copy $9800, Tilemap
    
    call EnableLCD
    
.loop
    frame
    calljoy UP, .up
    calljoy DOWN, .down
    calljoy LEFT, .left
    calljoy RIGHT, .right
    jrjoynew START, .return
    jr .loop
.up
    ld hl, H_SCY
    dec [hl]
    ret
.down
    ld hl, H_SCY
    inc [hl]
    ret
.left
    ld hl, H_SCX
    dec [hl]
    ret
.right
    ld hl, H_SCX
    inc [hl]
    ret
    
.return
    call ClearOAM
    pop hl ; don't want to ret
    jp InitGame


Options:
    printstatic $50, $10, $8, 0, "Options not implemented...@"
    refreshscreen
    ret

Tileset:
    INCBIN "gfx/tileset.2bpp"
TilesetEnd

Tilemap:
    db 1, 2, 2, 1, 3, 3, 3, 3, 1, 1, 2, 3, 1, 2, 3, 2, 2, 2, 3, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 3, 3, 3, 2, 1, 2, 2, 1, 2, 3, 3, 3, 3, 3, 1, 2, 3, 2, 1, 1, 2, 1, 1, 1, 2, 3, 3, 1, 2, 2, 2, 1, 3, 1, 2, 1, 3, 3, 3, 2, 3, 3, 3, 1, 1, 3, 2, 1, 2, 1, 3, 1, 2, 2, 2, 2, 1, 2, 1, 1, 1, 2, 1, 2, 3, 1, 4, 5, 2, 1, 1, 3, 1, 1, 2, 1, 3, 1, 1, 1, 1, 3, 3, 3, 3, 3, 4, 5, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3, 20
    db 21, 2, 2, 1, 1, 3, 1, 1, 1, 1, 2, 2, 3, 3, 3, 1, 2, 1, 1, 20, 21, 2, 2, 3, 3, 2, 1, 3, 1, 3, 1, 1, 2, 1, 2, 1, 1, 4, 5, 2, 2, 1, 2, 3, 4, 5, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 3, 3, 2, 3, 3, 2, 2, 1, 1, 1, 1, 1, 20, 21, 1, 1, 3, 3, 1, 20, 21, 2, 2, 1, 4, 5, 1, 1, 2, 2, 2, 2, 1, 3, 2, 3, 2, 1, 2, 2, 1, 1, 2, 1, 3, 2, 2, 3, 3, 1, 2, 2, 1, 2, 1, 2, 20, 21, 2, 2, 2, 2, 1, 4, 5, 3, 2, 3, 1, 2, 2, 2, 1, 2, 2, 1, 3, 3, 3, 3, 2, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 20
    db 21, 3, 2, 3, 2, 1, 1, 1, 2, 2, 1, 2, 3, 3, 2, 2, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 3, 1, 2, 3, 2, 2, 1, 2, 2, 2, 2, 1, 3, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 3, 3, 1, 1, 3, 1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 1, 2, 3, 3, 2, 1, 2, 2, 1, 2, 1, 4, 5, 2, 2, 2, 3, 1, 2, 1, 1, 3, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 2, 20, 21, 2, 2, 3, 1, 1, 1, 1
    db 1, 1, 4, 5, 2, 2, 2, 2, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 3, 3, 3, 3, 1, 1, 2, 2, 2, 2, 20, 21, 1, 2, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2, 1, 3, 3, 4, 5, 3, 3, 3, 3, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, 3, 3, 3, 1, 2, 1, 2, 1, 1, 2, 2, 3, 3, 3, 2, 20, 21, 2, 1, 2, 2, 2, 3, 1, 2, 2, 2, 1, 2, 2, 1, 4, 5, 1, 3, 3, 3, 3, 2, 1, 2, 1, 3, 3, 1, 1, 2
    db 1, 1, 2, 3, 1, 2, 1, 1, 3, 2, 4, 5, 1, 1, 2, 1, 20, 21, 1, 1, 2, 2, 3, 1, 1, 2, 1, 4, 5, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 2, 3, 20, 21, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 2, 2, 2, 1, 20, 21, 3, 3, 3, 2, 1, 2, 2, 1, 4, 5, 2, 1, 1, 1, 1, 1, 2, 2, 4, 5, 4, 5, 4, 5, 2, 3, 1, 2, 2, 1, 1, 1, 2, 1, 3, 4, 5, 3, 1, 2, 20, 21, 1, 2, 2, 2, 1, 1, 1, 1, 20, 21, 4, 5, 4, 5, 2, 3, 1, 2, 2, 2, 2, 2, 1, 2, 2, 20, 21, 3, 3, 3, 1, 2, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 4, 5, 4, 5, 1, 3, 4, 5, 2, 1, 2, 2, 2, 2, 2, 1, 2, 1, 2, 1, 3, 1, 1, 2, 2, 2, 3, 2, 1, 4, 5, 2, 4, 5, 20, 21, 1, 3, 20, 21, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 3, 4, 5, 3, 1, 2, 2
    db 2, 20, 21, 1, 4, 5, 4, 5, 1, 3, 3, 1, 1, 1, 2, 2, 3, 2, 3, 2, 2, 1, 1, 2, 1, 1, 20, 21, 1, 2, 2, 1, 1, 2, 1, 1, 20, 21, 20, 21, 2, 2, 3, 1, 1, 1, 2, 1, 1, 3, 1, 3, 2, 1, 2, 2, 2, 2, 3, 2, 2, 1, 1, 1, 1, 2, 1, 2, 1, 4, 5, 1, 2, 2, 3, 2, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1, 3, 1, 2, 3, 3, 2, 2, 4
    db 5, 1, 2, 20, 21, 3, 2, 2, 3, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 4, 5, 1, 1, 2, 3, 2, 1, 20, 21, 1, 1, 1, 1, 1, 2, 2, 3, 1, 2, 2, 1, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 20, 21, 2, 1, 3, 1, 1, 2, 1, 2, 3, 2, 1, 2, 3, 4, 5, 3, 3, 2, 1, 1, 2, 1, 4, 5, 2, 2, 2, 1, 1, 1, 3, 3, 1, 1, 1, 3, 1, 2, 2, 1, 1, 4, 5, 2, 2, 20, 21
    db 2, 3, 3, 4, 5, 3, 3, 20, 21, 3, 3, 3, 3, 3, 3, 3, 1, 4, 5, 1, 2, 2, 2, 3, 1, 1, 20, 21, 1, 3, 1, 2, 2, 2, 1, 20, 21, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 20, 21, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 3, 1, 2, 2, 2, 2, 2, 2, 1, 2, 3, 1, 1, 2, 1, 3, 1, 2, 2, 1, 2, 2, 1, 2, 1
TilemapEnd
