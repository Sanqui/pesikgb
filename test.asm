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
    lda [rWY], [H_WY]
    lda [rWX], [H_WX]
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
    
    ld a, %11100111
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
    ld a, 160
    ld [H_WX], a
    ld a, 144
    ld [H_WY], a
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
    copy $8b00, MenuIcons
    copy $8700, SelectorGfx
    
    copy $9c00, MenuTilemap0
    copy $9c20, MenuTilemap1
    copy $9c40, MenuTilemap2
    copy $9c60, MenuTilemap3
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
    lda [hli], 0
    lda [hli], 120+100+8
    lda [hli], 0
    lda [hli], 0
    lda [hli], 80+100+16
    lda [hli], 0
    
    call EnableLCD
    
.loop
    frame
    xor a
    ld [wTmpSpriteWidth], a
    ld [wTmpSpriteHeight], a
    call MovePlayer
    call CameraTowardsSprite
    call UpdateSprites
    call DoOWMenu
    lda [H_SCY], [wCameraY]
    lda [H_SCX], [wCameraX]
    ;jpjoynew START, .return
    jr .loop

MovePlayer:
    ld a, [wMenuOpen]
    and a
    ret nz
    ld a, [H_JOY]
    swap a
    and %00001111
    ld [wMapObject0+6], a
    jp MoveObject

MoveObject:
    ld a, [wMapObject0+6]
    and a
    jr nz, .move
    ld a, [wMapObject0+7]
    sub 2
    jr nc, .not0
    xor a
.not0
    ld [wMapObject0+7], a
    ret
.move
    call MoveObjectTryDirection
    ld a, [wMoved]
    and a
    ;jr .moved
    jr nz, .moved
    ld a, [wMapObject0+6]
    ld c, a
    ld b, 0
    ld hl, NextDirections
    add hl, bc
    add hl, bc
    ld a, [hli]
    push hl
    call MoveObjectTryDirection
    pop hl
    ld a, [wMoved]
    and a
    jr nz, .moved
    
    ld a, [hli]
    call MoveObjectTryDirection
    ld a, [wMoved]
    and a
    jr nz, .moved
    
    ld a, [wMapObject0+7]
    sub 1
    ret c
    ld [wMapObject0+7], a
    ret
    
.moved    
    ld hl, wMapObject0+7
    ld a, [hl]
    inc a
    ld [hl], a
    cp 32
    ret c
    dec [hl]
    ret
 
MoveObjectTryDirection:
    push af
    xor a
    lda [wMoved], a
    pop af
    
    ld hl, Directions
    ld c, a
    ld b, 0
    add hl, bc
    add hl, bc
    lda [wMoveVert], [hli]
    ld b, a
    lda [wMoveHor], [hl]
    and b
    ld hl, MovementSteps
    jr z, .notdiag
    ld hl, MovementStepsDiag
.notdiag
    ld a, [wMapObject0+7]
    ld c, a
    ld b, 0
    add hl, bc
    add hl, bc
    lda c, [hli]
    ld b, [hl]
    
    ld de, wMapObject0
    
    push bc
    ld a, [wMoveVert]
    call .doDirection
    lda [wNewPositionBackup], [wNewPosition]
    lda [wNewPositionBackup+1], [wNewPosition+1]
    lda [wNewPositionBackup+2], [wNewPosition+2]
    pop bc
    
    ld a, [wMoveHor]
    call .doDirection
    
    lda [wTestObjectY  ], [wNewPositionBackup+1]
    lda [wTestObjectY+1], [wNewPositionBackup+2]
    lda [wTestObjectX  ], [wNewPosition+1]
    lda [wTestObjectX+1], [wNewPosition+2]
    
    call IsSpriteAtWall
    jr nz, .nowrite
    lda [wMoved], 1
    lda [wMapObject0], [wNewPositionBackup]
    lda [wMapObject0+1], [wNewPositionBackup+1]
    lda [wMapObject0+2], [wNewPositionBackup+2]
    lda [wMapObject0+3], [wNewPosition]
    lda [wMapObject0+4], [wNewPosition+1]
    lda [wMapObject0+5], [wNewPosition+2]
    
    ret
.nowrite
    xor a
    ld [wMoved], a
    ret
    
.doDirection
    and a
    jr nz, .dodirection
.skip
    lda [wNewPosition], [de]
    inc de
    lda [wNewPosition+1], [de]
    inc de
    lda [wNewPosition+2], [de]
    inc de
    xor a
    ret
.dodirection
    push af
    ld a, c
    or b
    jr nz, .do
    pop af
    jr .skip
.do
    pop af
    
    dec a
    jr z, .positive
.negative
    ld a, c
    cpl
    inc a
    ld c, a
    jr nz, .nzneg
    dec b
.nzneg
    ld a, b
    cpl
    
    ld b, a
    lda l, [de]
    inc de
    lda h, [de]
    inc de
    add hl, bc
    ld a, [de]
    jr c, .write
    dec a
    ;ld [de], a
    
    jr .write
    
.positive
    ld b, a
    lda l, [de]
    inc de
    lda h, [de]
    inc de
    add hl, bc
    ld a, [de]
    jr nc, .write
    inc a
    ;ld [de], a
.write
    inc de
    push de
    ld de, wNewPosition
    
    push af
    lda [de], l
    inc de
    lda [de], h
    inc de
    pop af
    ld [de], a
    
    ld a, 1
    and a
    pop de
    ret

Directions:
    ;   y   x
    db  0,  0 ; 0 0000   none
    db  0,  1 ; 1 0001   right 
    db  0, -1 ; 2 0010   left
    db  0,  0 ; 3 0011 X right/left
    db -1,  0 ; 4 0100   up
    db -1,  1 ; 5 0101   up/right
    db -1, -1 ; 6 0110   up/left
    db  0,  0 ; 7 0111 X up/left/right
    db  1,  0 ; 8 1000   down
    db  1,  1 ; 9 1001   down/right 
    db  1, -1 ; a 1010   down/left
    db  0,  0 ; b 1011 X down/left/right
    db  0,  0 ; c 1100 X down/up
    db  0,  0 ; d 1101 X down/up/right
    db  0,  0 ; e 1110 X down/up/left
    db  0,  0 ; f 1111 X down/up/left/right

NextDirections:
    db $0, $0 ; 0 0000   none
    db $5, $9 ; 1 0001   right 
    db $6, $a ; 2 0010   left
    db $0, $0 ; 3 0011 X right/left
    db $6, $5 ; 4 0100   up
    db $4, $1 ; 5 0101   up/right
    db $2, $4 ; 6 0110   up/left
    db $0, $0 ; 7 0111 X up/left/right
    db $a, $9 ; 8 1000   down
    db $1, $8 ; 9 1001   down/right 
    db $2, $8 ; a 1010   down/left
    db $0, $0 ; b 1011 X down/left/right
    db $0, $0 ; c 1100 X down/up
    db $0, $0 ; d 1101 X down/up/right
    db $0, $0 ; e 1110 X down/up/left
    db $0, $0 ; f 1111 X down/up/left/right

MovementSteps:
    dw 2
x = 0.0
rept 32
    ; DIV(x, 12.0)
    ;dw SIN(0.0) >> 16
    ;printf SIN(MUL( x<<6, DIV(256.0, 16.0) ))
    ;PRINTT "\n"
    dw     (SIN(MUL( x<<6, DIV(256.0, 32.0) )) >> 8)+1
x = x + 1.0
endr

MovementStepsDiag:
    dw 2
x = 0.0
rept 32
    dw     (DIV(SIN(MUL( x<<6, DIV(256.0, 32.0) )), 1.41421356237) >> 8)+1
x = x + 1.0
endr

AdvanceSpriteMovement:
    ld hl, wMapObject0+9
    inc [hl]
    ret

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
    
    ld bc, $17
    call CopyData
    
    lda [wCopyRowAmount], $17
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
    ;fillmemory W_OAM, 0, 4*$28
    
    
    ld de, wMapObject0+1
    call SubCameraYAtDE
    ld a, h
    and a
    jr nz, .skip2
    ld a, l
    cp 160
    jr nc, .skip2
    ld [wTmpSpriteY], a
    
    inc de
    call SubCameraXAtDE
    ld a, h
    and a
    jr nz, .skip
    ld a, l
    cp 144+32
    jr nc, .skip
    sub 8
    ld [wTmpSpriteX], a
    inc de
    inc de
    ld a, [de]
    inc de
    add a
    ld b, a
    ld a, [de]
    and %00001000
    sra a
    ld c, a
    
    ld hl, W_OAM
    lda [hli], [wTmpSpriteY]
    lda [hli], [wTmpSpriteX]
    xor a
    add c
    add b
    ld [hli], a
    ld a, b
    sla a
    sla a
    sla a
    sla a
    ld [hli], a
    
    lda [hli], [wTmpSpriteY]
    ld a, [wTmpSpriteX]
    add 8
    ld [hli], a
    ld a, 2
    add c
    sub b
    ld [hli], a
    ld a, b
    sla a
    sla a
    sla a
    sla a
    ld [hli], a
    jr .end
    
.skip2
    inc de
    inc de
.skip
.end
    ret

CameraTowardsSprite:
    ld de, wMapObject0+1
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
    inc de ; suby
    
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
    inc hl
    call CopyTwoCollisionTiles
    ld bc, 64 - 1
    add hl, bc
    ld a, [hli]
CopyTwoCollisionTiles:
    push hl
    call CopyCollisionMapOfTile
    pop hl
    ld a, [hl]
    push hl
    call CopyCollisionMapOfTile
    pop hl
    ret

PreparePlayerCollisionMap:
    ld de, wCollisionTestMap2
    ld hl, PlayerCollisionMask
    ld a, [wCurY]
    and %00000111
    xor %00000111
    add l
    ld l, a
    jr nc, .ncy
    inc h
.ncy
    ld b, 8
.topleftloop
    ld a, [hli]
    ld [de], a
    inc de
    djnz .topleftloop
    ld b, 8
    xor a
.toprightloop
    ld [de], a
    inc de
    djnz .toprightloop
    ld b, 8
.bototmleftloop
    ld a, [hli]
    ld [de], a
    inc de
    djnz .bototmleftloop
    ld b, 8
    xor a
.bottomrightloop
    ld [de], a
    inc de
    djnz .bottomrightloop
    
    ld hl, wCollisionTestMap2
    ld de, wCollisionTestMap2+8
    ld c, 8
.loopy
    ld a, [wCurX]
    and %00000111
    jr z, .donex
    ld b, a
.loopx
    srl [hl]
    ld a, [de]
    rr a
    ld [de], a
    dec b
    jr nz, .loopx
.donex
    inc hl
    inc de
    dec c
    jr nz, .loopy
    
    ld hl, wCollisionTestMap2+16
    ld de, wCollisionTestMap2+24
    ld c, 8
.loopybottom
    ld a, [wCurX]
    and %00000111
    jr z, .donexbottom
    ld b, a
.loopxbottom
    srl [hl]
    ld a, [de]
    rr a
    ld [de], a
    dec b
    jr nz, .loopxbottom
.donexbottom
    inc hl
    inc de
    dec c
    jr nz, .loopybottom
    ret

IsSpriteAtWall:
    ld a, [wTestObjectY]
    add a, 8
    ld [wCurY], a
    ld a, [wTestObjectY+1]
    adc a, 0
    ld [wCurY+1], a
    
    ld a, [wTestObjectX]
    add a, 4
    ld [wCurX], a
    ld a, [wTestObjectX+1]
    adc a, 0
    ld [wCurX+1], a
    call FillCollisionMap
    
    call PreparePlayerCollisionMap
    
    ld b, 8*4
    ld de, wCollisionTestMap
    ld hl, wCollisionTestMap2
.loop
    ld a, [de]
    and [hl]
    ret nz
    inc de
    inc hl
    djnz .loop
    
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

MenuIcons:
    INCBIN "gfx/menuicons.2bpp"
MenuIconsEnd

    incdata SelectorGfx, "gfx/selector.interleave.2bpp"

MenuTilemap0:
    db $b0, $b1, $00,   $b2, $b3, $00,   $b4, $b5, $00,   $b6, $b7, $00,   $b8, $b9, $00
MenuTilemap0End
MenuTilemap1:
    db $c0, $c1, $00,   $c2, $c3, $00,   $c4, $c5, $00,   $c6, $c7, $00,   $c8, $c9, $00
MenuTilemap1End
MenuTilemap2:
    db $d0, $d1, $00,   $d2, $d3, $00,   $d4, $d5, $00,   $d6, $d7, $00,   $d8, $d9, $00
MenuTilemap2End
MenuTilemap3:
    db $e0, $e1, $00,   $e2, $e3, $00,   $e4, $e5, $00,   $e6, $e7, $00,   $e8, $e9, $00
MenuTilemap3End

PlayerCollisionMask:
    db 0, 0, 0, 0, 0, 0, 0, 0 ; padding
    
    db %00000000
    db %01111110
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %01111110
    db %00000000
    
    db 0, 0, 0, 0, 0, 0, 0, 0 ; padding

TileCollisions:
    db 0, 0, 0, 2, 4, 5, 8, 9, 1, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 6, 7,10,11, 0, 0, 0, 0, 0, 0, 0, 0

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
; 5 bottom left
    db %11111000
    db %11111100
    db %11111110
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
; 6 top right
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %01111111
    db %00111111
    db %00011111
; 7 top left
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111111
    db %11111110
    db %11111100
    db %11111000
; 8 bottom right diag
    db %00000001
    db %00000011
    db %00000111
    db %00001111
    db %00011111
    db %00111111
    db %01111111
    db %11111111
; 9 top right diag
    db %10000000
    db %11000000
    db %11100000
    db %11110000
    db %11111000
    db %11111100
    db %11111110
    db %11111111
; 10 bottom left diag
    db %11111111
    db %01111111
    db %00111111
    db %00011111
    db %00001111
    db %00000111
    db %00000011
    db %00000001
; 11 bottom right diag
    db %11111111
    db %11111110
    db %11111100
    db %11111000
    db %11110000
    db %11100000
    db %11000000
    db %10000000

DoOWMenu:
    ld a, [wMenuOpen]
    and a
    jr nz, .isopen
    ld a, [H_JOYNEW]
    bit A_, a
    ret z
    lda [wMenuOpen], 1

    lda [H_WY], 160-32
    lda [H_WX], 32+16+8
    ret

.isopen
    ld a, [H_JOYNEW]
    bit A_, a
    jr nz, .close
    bit LEFT, a
    jr nz, .left
    bit RIGHT, a
    jr nz, .right
    bit UP, a
    jr nz, .up
    bit DOWN, a
    jr nz, .down
    jr .draw
.left
    ld a, [wMenuOption]
    sub 1
    jr c, .draw
    ld [wMenuOption], a
    jr .draw
.right
    ld a, [wMenuOption]
    inc a
    cp 9
    jr nc, .draw
    ld [wMenuOption], a
    jr .draw
.up
.down
.draw
    ld hl, W_OAM + 4 * $26
    
    ld d, 0
    ld a, [wMenuOption]
    cp 5
    jr c, .notrow2
    ld d, 16
.notrow2
    ld a, 160-32
    add d
    ld [hli], a
    ld a, [wMenuOption]
    sub 5
    jr nc, .row2
    add 5
.row2
    ld b, a
    sla a
    sla a
    sla a
    ld b, a
    sla a
    add b
    add 32+16+8
    push af
    ld [hli], a
    ld a, $70
    ld [hli], a
    xor a
    ld [hli], a
    
    ld a, 160-32
    add d
    ld [hli], a
    pop af
    add 16
    ld [hli], a
    ld a, $72
    ld [hli], a
    xor a
    ld [hl], a
    
    
        
    lda [H_WY], 160-32-16
    ret
    
    
.close
    xor a
    ld [wMenuOpen], a
    lda [H_WX], 160
    lda [H_WY], 144
    ld a, 160
    ld [W_OAM + 4 * $26], a
    ld [W_OAM + 4 * $27], a
    
    
    ret





























