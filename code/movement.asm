MovePlayer:
    ld a, [wMenuOpen]
    and a
    jr nz, .stopmoving
    ld a, [H_JOY]
    swap a
    and %00001111
    and a
    jr z, .stopmoving
    ld [wMapObject0+6], a
    jp MoveObject
.stopmoving
    ld hl, wMapObject0+6
    set 4, [hl]
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
    res 4, a
    call MoveObjectTryDirection
    ld a, [wMoved]
    and a
    ;jr .moved
    jr nz, .moved
    ld a, [wMapObject0+6]
    res 4, a
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
    jr nz, .slowdown
    
    ld a, [hli]
    call MoveObjectTryDirection
    ld a, [wMoved]
    and a
    jr nz, .slowdown
    
    ld a, [hli]
    call MoveObjectTryDirection
    ld a, [wMoved]
    and a
    jr nz, .slowdown
    
    ld a, [wMapObject0+7]
    sub 1
    ret c
    ld [wMapObject0+7], a
    
    jr .sprite
    
.moved
    ;ld a, [wMapObject0+7]
    ;and a
    ;jr z, .dontanim
;.dontanim
    ld a, [wMapObject0+6]
    bit 4, a
    jr nz, .slowdown
    
    ld hl, wMapObject0+7
    ld a, [hl]
    inc a
    ld [hl], a
    cp 32
    jr c, .sprite
    dec [hl]
    jr .sprite
.slowdown
    ld a, [wMapObject0+7]
    sub 2
    jr nc, .not0_
    ;xor a
    ;ld [wMapObject0+6], a
    ;ld a, [wMapObject0+8]
    ;and %11111100
    ;ld [wMapObject0+8], a
    xor a
.not0_
    ld [wMapObject0+7], a

.sprite
    call AdvanceSpriteAnim
    ret

DirectionsToSpriteIndices:
    db   0 ; 0 0000   none
    db   0 ; 1 0001   right 
    db   4 ; 2 0010   left
    db   0 ; 3 0011 X right/left
    db   6 ; 4 0100   up
    db   7 ; 5 0101   up/right
    db   5 ; 6 0110   up/left
    db   0 ; 7 0111 X up/left/right
    db   2 ; 8 1000   down
    db   1 ; 9 1001   down/right 
    db   3 ; a 1010   down/left
    db   0 ; b 1011 X down/left/right
    db   0 ; c 1100 X down/up
    db   0 ; d 1101 X down/up/right
    db   0 ; e 1110 X down/up/left
    db   0 ; f 1111 X down/up/left/right


AdvanceSpriteAnim:
    ld a, [wMapObject0+7] ; step
    and a ; are we moving?
    jr nz, .moving
    xor a
    ld b, a
    jr .notinc
.moving
    ld hl, wMapObject0+8
    ld a, [hl]
    and %00000011
    ld b, a
    ld a, [H_TIMER]
    and %00001111
    jr nz, .notinc
    ld a, [hl]
    and %00000011
    inc a
    cp 4
    jr c, .notover
    xor a
.notover
    ld b, a
.notinc
    ld a, [wMapObject0+6]
    and %00001111
    push hl
    ld hl, DirectionsToSpriteIndices
    ld e, a
    ld d, 0
    add hl, de
    ld a, [hl]
    pop hl
    sla a
    sla a
    add b
HandleNewSpriteAnim:
    ld hl, wMapObject0+8
    cp [hl]
    ret z
    or $80
    ld [hl], a
    ret

ScheduleNewSprite:
    inc e
.getaddressloop
    dec e
    jr z, .gotaddress
    add hl, bc
    jr .getaddressloop
.gotaddress
    lda [wCopyGfxSrc], l
    lda [wCopyGfxSrc+1], h
    sra c
    sra c
    sra c
    sra c
    lda [wCopyGfxAmount], c
    
    ret

MoveObjectTryDirection:
    and a
    ret z
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
    db $0, $0, $0 ; 0 0000   none
    db $5, $9, $2 ; 1 0001   right 
    db $6, $a, $1 ; 2 0010   left
    db $0, $0, $0 ; 3 0011 X right/left
    db $6, $5, $8 ; 4 0100   up
    db $4, $1, $a ; 5 0101   up/right
    db $2, $4, $a ; 6 0110   up/left
    db $0, $0, $0 ; 7 0111 X up/left/right
    db $a, $9, $4 ; 8 1000   down
    db $1, $8, $6 ; 9 1001   down/right 
    db $2, $8, $5 ; a 1010   down/left
    db $0, $0, $0 ; b 1011 X down/left/right
    db $0, $0, $0 ; c 1100 X down/up
    db $0, $0, $0 ; d 1101 X down/up/right
    db $0, $0, $0 ; e 1110 X down/up/left
    db $0, $0, $0 ; f 1111 X down/up/left/right

MovementSteps:
    dw 0
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
    dw 0
x = 0.0
rept 32
    dw     (DIV(SIN(MUL( x<<6, DIV(256.0, 32.0) )), 1.41421356237) >> 8)+1
x = x + 1.0
endr


GetTileAt:
    ld a, [wCurY]
    and %11111000
    ld l, a
    lda h, [wCurY+1]
    shiftleft hl, 4
    
    lda c, [wCurX]
    lda b, [wCurX+1]
    shiftright bc, 3
    add hl, bc
    lda c, [wTilemapPtr]
    lda b, [wTilemapPtr+1]
    ld a, [wTilemapPtr+2]
    bankswitch
    add hl, bc
    ld a, [hl] ; tile
    ret

CopyCollisionMapOfTile:
    ld l, a
    ld h, 0
    shiftleft hl, 3
    lda c, [wTilesetMaskPtr]
    lda b, [wTilesetMaskPtr+1]
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
    ld bc, 128 - 1
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
    add a, 8
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

