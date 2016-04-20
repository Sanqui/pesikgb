AddAttributesForTileData:
.loop
    ld a, [hli]
    cp $ff
    ret z
    push hl
    push af
    lda l, [wTileAttributesPtr]
    lda h, [wTileAttributesPtr+1]
    ld a, [wTileAttributesPtr+2]
    bankswitch
    pop af
    add l
    ld l, a
    jr nc, .nc
    inc h
.nc
    ld a, [hl]
    ld [de], a
    inc de
    pop hl
    jr .loop
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
    
    shiftleft hl, 2
    
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
    shiftleft hl, 2
    lda c, [wTilemapPtr]
    lda b, [wTilemapPtr+1]
    ld a, [wTilemapPtr+2]
    bankswitch
    add hl, bc
    lda c, [wCameraX]
    lda b, [wCameraX+1]
    shiftright bc, 3
    dec bc
    add hl, bc
    
    lda [wCopyRowDest], e
    lda [wCopyRowDest+1], d
    ld de, wCopyRowData
    
    ld bc, $17
    call CopyData
    
    ld hl, wCopyRowData
    ld de, wCopyRowDataAttr
    call AddAttributesForTileData
    
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
    
    shiftleft hl, 2
    
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
    shiftleft hl, 2
    lda c, [wTilemapPtr]
    lda b, [wTilemapPtr+1]
    ld a, [wTilemapPtr+2]
    bankswitch
    add hl, bc
    pop bc
    push hl
    lda l, [wCameraX]
    lda h, [wCameraX+1]
    add hl, bc
    push hl
    pop bc
    pop hl
    
    shiftright bc, 3
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
    ld bc, 128
    add hl, bc
    pop bc
    dec b
    jr nz, .copyloop
    
    ld hl, wCopyColData
    ld de, wCopyColDataAttr
    call AddAttributesForTileData
    
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
