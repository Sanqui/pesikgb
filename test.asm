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
    call VCopyRow
    call VCopyCol
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

VCopyRow:
    ld a, [wCopyRowAmount]
    and a
    ret z
    ld b, a
    lda e, [wCopyRowDest]
    lda d, [wCopyRowDest+1]
    ld hl, wCopyRowData
.copyloop
    ld a, [hli]
    ld [de], a
    inc de
    ld a, e
    and %00011111
    jr nz, .decb
    ld a, e
    sub %00100000
    ld e, a
    jr nc, .decb
    dec d
.decb
    dec b
    jr nz, .copyloop
    lda [wCopyRowAmount], b
    ret

VCopyCol:
    ld a, [wCopyColAmount]
    and a
    ret z
    ld b, a
    lda e, [wCopyColDest]
    lda d, [wCopyColDest+1]
    ld hl, wCopyColData
.copyloop
    ld a, [hli]
    ld [de], a
    ld a, e
    add 32
    ld e, a
    jr nc, .decb
    inc d
    ld a, d
    cp $9c
    jr nz, .decb
    dec d
    dec d
    dec d
    dec d
.decb
    dec b
    jr nz, .copyloop
    lda [wCopyColAmount], b
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
    
    ld a, %11000111
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
    db 3, 4, 2, 9 ; y, x, h, w
    db $30 ; tile gfx index
    db 0 ; options are inlined
    db 1 ; number of options
    db "Start@"
    db 1 ; callbacks
    dw StartGame

InitGame:
    call ClearTilemap
    printstatic $00, $0f, 1, 5, "RPG engine test@"
    printstatic $10, $20, $11, 0, "Sanky 2015@"
    xor a
    ld [H_SCX], a
    ld [H_SCY], a
    refreshscreen
.here
    halt
    domenu NewGameMenu
    jr .here

StartGame:
    frame2
    call ClearTilemap
    refreshscreen
    frame2
    
    call DisableLCD
    
    copy $9010, Tileset
    copy $8000, Melodingo
    ld b, 0
    ld c, 0
.loadtilemaploop
    push bc
    call ScrollVert
    call VCopyRow
    pop bc
    ld a, c
    add 8
    ld c, a
    cp 160-8
    jr c, .loadtilemaploop
    ;copy $9800, Tilemap
    
    ld hl, wMapObject0
    lda [hli], 120+100
    lda [hli], 0
    lda [hli], 80+100
    lda [hli], 0
    
    call EnableLCD
    
.loop
    frame
    xor a
    ld [wTmpSpriteWidth], a
    ld [wTmpSpriteHeight], a
    calljoy UP, .up
    calljoy DOWN, .down
    calljoy LEFT, .left
    calljoy RIGHT, .right
    ;call ScrollVert
    call CameraTowardsSprite
    call UpdateSprites
    lda [H_SCY], [wCameraY]
    lda [H_SCX], [wCameraX]
    jpjoynew START, .return
    jr .loop
    
.up
    dec16 wMapObject0
    call IsSpriteAtWall
    ret z
    inc16 wMapObject0
    ret
.down
    inc16 wMapObject0
    call IsSpriteAtWall
    ret z
    dec16 wMapObject0
    ret
.left
    dec16 wMapObject0+2
    call IsSpriteAtWall
    ret z
    inc16 wMapObject0+2
    ret
.right
    inc16 wMapObject0+2
    call IsSpriteAtWall
    ret z
    dec16 wMapObject0+2
    ret
    
.return
    call ClearOAM
    pop hl ; don't want to ret
    jp InitGame

MoveCameraUp:
    dec16 wCameraY
    ld a, [wCameraY+1]
    cp $ff
    jr nz, .notneg
    xor a
    ld [wCameraY], a
    ld [wCameraY+1],a
.notneg
    ld a, [wCameraY]
    and %00000111
    call z, ScrollUp
    ret
MoveCameraDown:
    inc16 wCameraY
    ld a, [wCameraY]
    and %00000111
    call z, ScrollDown
    ret
MoveCameraLeft:
    dec16 wCameraX
    ld a, [wCameraX+1]
    cp $ff
    jr nz, .notnegx
    xor a
    ld [wCameraX], a
    ld [wCameraX+1],a
.notnegx
    ld a, [wCameraX]
    and %00000111
    call z, ScrollLeft
    ret
MoveCameraRight:
    inc16 wCameraX
    ld a, [wCameraX]
    and %00000111
    call z, ScrollRight
    ret
    

ScrollDown:
    ld bc, 144
    jr ScrollVert
ScrollUp:
    ld bc, -1

ScrollVert:
    lda l, [wCameraY]
    lda h, [wCameraY+1]
    add hl, bc
    ld a, l
    and %11111000
    ld l, a
    
    ; *4
    sla l
    rl h
    sla l
    rl h
    
    push hl
    ld a, h
    and %00000011
    ld h, a
    ld a, [wCameraX]
    srl a
    srl a
    srl a
    dec a
    and %00011111
    ld c, a
    ld b, 0
    add hl, bc
    ld bc, $9800
    add hl, bc
    push hl
    pop de
    pop hl
    sla l
    rl h
    ld bc, Tilemap
    add hl, bc
    lda c, [wCameraX]
    lda b, [wCameraX+1]
    sra b
    rr c
    sra b
    rr c
    sra b
    rr c
    dec bc
    add hl, bc
    
    lda [wCopyRowDest], e
    lda [wCopyRowDest+1], d
    ld de, wCopyRowData
    
    ld bc, $16
    call CopyData
    
    lda [wCopyRowAmount], $16
    ret


ScrollLeft:
    ld bc, -1
    jr ScrollHor
ScrollRight:
    ld bc, 160
    ld b, a

ScrollHor:
    lda l, [wCameraY]
    lda h, [wCameraY+1]
    ld a, l
    and %11111000
    ld l, a
    
    ; *4
    sla l
    rl h
    sla l
    rl h
    
    push bc ; off
    push hl
    ld a, h
    and %00000011
    ld h, a
    ld a, [wCameraX]
    add c
    srl a
    srl a
    srl a
    and %00011111
    ld c, a
    ld b, 0
    add hl, bc
    ld bc, $9800
    add hl, bc
    push hl
    pop de
    pop hl
    sla l
    rl h
    ld bc, Tilemap
    add hl, bc
    pop bc
    push hl
    lda l, [wCameraX]
    lda h, [wCameraX+1]
    add hl, bc
    push hl
    pop bc
    pop hl
    
    sra b
    rr c
    sra b
    rr c
    sra b
    rr c
    add hl, bc
    
    lda [wCopyColDest], e
    lda [wCopyColDest+1], d
    ld de, wCopyColData
    
    ld b, $13
.copyloop
    ld a, [hl]
    ld [de], a
    inc de
    push bc
    ld bc, 64
    add hl, bc
    pop bc
    dec b
    jr nz, .copyloop
    
    lda [wCopyColAmount], $13
    ret

SubCameraYAtDE:
    ld a, [wCameraY]
    xor $ff
    ld c, a
    ld a, [wCameraY+1]
    xor $ff
    ld b, a
    lda l, [de]
    inc de
    lda h, [de]
    inc de
    add hl, bc
    ld bc, 16
    add hl, bc
    ret

SubCameraXAtDE:
    ld a, [wCameraX]
    xor $ff
    ld c, a
    ld a, [wCameraX+1]
    xor $ff
    ld b, a
    lda l, [de]
    inc de
    lda h, [de]
    inc de
    add hl, bc
    ld bc, 16
    add hl, bc
    ret

UpdateSprites:
    fillmemory W_OAM, 0, 4*$28
    
    
    ld de, wMapObject0
    call SubCameraYAtDE
    ld a, h
    and a
    jr nz, .skip2
    ld a, l
    cp 160
    jr nc, .skip2
    ld [wTmpSpriteY], a
    
    call SubCameraXAtDE
    ld a, h
    and a
    jr nz, .skip
    ld a, l
    cp 144+32
    jr nc, .skip
    sub 8
    ld [wTmpSpriteX], a
    
    ld hl, W_OAM
    lda [hli], [wTmpSpriteY]
    lda [hli], [wTmpSpriteX]
    xor a
    ld [hli], a
    ld [hli], a
    
    lda [hli], [wTmpSpriteY]
    ld a, [wTmpSpriteX]
    add 8
    ld [hli], a
    ld a, 2
    ld [hli], a
    xor a
    ld [hli], a
    jr .end
    
.skip2
    inc de
    inc de
.skip
.end
    ret

CameraTowardsSprite:
    ld de, wMapObject0
    call SubCameraYAtDE
    push de
    ld bc, -160/2 - 8
    add hl, bc
    bit 7, h
    jr z, .notup
    call MoveCameraUp
    jr .next
.notup
    ld a, h
    or l
    call nz, MoveCameraDown
.next
    pop de
    
    call SubCameraXAtDE
    ld bc, -144/2 - 16
    add hl, bc
    bit 7, h
    jr z, .notleft
    call MoveCameraLeft
    jr .end
.notleft
    ld a, h
    or l
    call nz, MoveCameraRight
.end
    ret

GetTileAt:
    ld a, [wCurY]
    and %11111000
    ld l, a
    lda h, [wCurY+1]
    sla l
    rl h
    sla l
    rl h
    sla l
    rl h
    
    lda c, [wCurX]
    lda b, [wCurX+1]
    sra b
    rr c
    sra b
    rr c
    sra b
    rr c
    add hl, bc
    ld bc, Tilemap
    add hl, bc
    ld a, [hl] ; tile
    ret

CopyCollisionMapOfTile:
    ld hl, TileCollisions
    add l
    ld l, a
    jr nc, .nc
    inc h
.nc
    ld l, [hl]
    ld h, 0
    sla l
    rr h
    sla l
    rr h
    sla l
    rr h
    ld bc, TileCollisionMasks
    add hl, bc
rept 8
    ld a, [hli]
    ld [de], a
    inc de
endr
    ret

FillCollisionMap:
    call GetTileAt
    ld de, wCollisionTestMap
    call CopyThreeCollisionTiles
    ld bc, 64 - 2
    add hl, bc
    ld a, [hli]
CopyThreeCollisionTiles:
    push hl
    call CopyCollisionMapOfTile
    pop hl
    ld a, [hli]
    push hl
    call CopyCollisionMapOfTile
    pop hl
    ld a, [hl]
    push hl
    call CopyCollisionMapOfTile
    pop hl
    ret

IsSpriteAtWall:
    lda [wCurY], [wMapObject0]
    lda [wCurY+1], [wMapObject0+1]
    lda [wCurX], [wMapObject0+2]
    lda [wCurX+1], [wMapObject0+3]
    call FillCollisionMap
    ; overlay player collision mask
    ;ld hl, PlayerCollisionMask
    ld hl, wCollisionTestMap
    ld a, [wMapObject0]
    and %00000111
    ld c, a
    ld b, 0
    add hl, bc
    
    ld a, [wMapObject0+2]
    and %00000111
.loop
    rlc [hl]
    dec a
    jr nz, .loop
    bit 0, [hl]
    
    ret

Tileset:
    INCBIN "gfx/tileset.2bpp"
TilesetEnd

Tilemap:
    INCBIN "maps/test.bin"
TilemapEnd

Melodingo:
    INCBIN "gfx/melodingo_small.interleave.2bpp"
MelodingoEnd

PlayerCollisionMask:
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %01111110
    db %11111111
    db %11111111
    db %01111110

TileCollisions:
    db 0, 0, 0, 2, 4, 5, 4, 5, 1, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 6, 7, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0

TileCollisionMasks:
; 0 empty
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
; 1 full
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
; 2 round
    db %00011000
    db %00111100
    db %01111110
    db %11111111
    db %11111111
    db %01111110
    db %00111100
    db %00011000
; 3 none yet
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
    db %00000000
; 4 bottom right
    db %00011111
    db %00111111
    db %01111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
; 4 bottom left
    db %11111000
    db %11111100
    db %11111110
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
; 4 top right
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %01111111
    db %00111111
    db %00011111
; 4 top left
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111110
    db %11111100
    db %11111000
