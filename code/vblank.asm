VBlankHandler:
    push af
    push bc
    push de
    push hl
    call FastVblank
    call VCopyRow
    call VCopyCol
    call VCopyGfx
    call VCopyPal
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
    
    ld a, [H_GBC]
    and a
    jr z, .notgbc
    
    ld a, 1
    ld [rVBK], a
    lda b, [wCopyRowAmount]
    lda e, [wCopyRowDest]
    lda d, [wCopyRowDest+1]
    ld hl, wCopyRowDataAttr
.copyloop_a
    ld a, [hli]
    ld [de], a
    inc de
    ld a, e
    and %00011111
    jr nz, .decb_a
    ld a, e
    sub %00100000
    ld e, a
    jr nc, .decb_a
    dec d
.decb_a
    dec b
    jr nz, .copyloop_a
    
    xor a
    ld [rVBK], a

.notgbc
    
    ld a, [rLY]
    cp $90
    ret c
    xor a
    ld [wCopyRowAmount], a
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
    
    ld a, [H_GBC]
    and a
    jr z, .notgbc
    
    ld a, 1
    ld [rVBK], a
    lda b, [wCopyColAmount]
    lda e, [wCopyColDest]
    lda d, [wCopyColDest+1]
    ld hl, wCopyColDataAttr
.copyloop_a
    ld a, [hli]
    ld [de], a
    ld a, e
    add 32
    ld e, a
    jr nc, .decb_a
    inc d
    ld a, d
    cp $9c
    jr nz, .decb_a
    dec d
    dec d
    dec d
    dec d
.decb_a
    dec b
    jr nz, .copyloop_a
    
    xor a
    ld [rVBK], a

.notgbc
    
    ld a, [rLY]
    cp $90
    ret c
    lda [wCopyColAmount], b
    ret

VCopyGfx:
    ld a, [wCopyGfxAmount]
    and a
    ret z
    ld b, a
    lda e, [wCopyGfxDest]
    lda d, [wCopyGfxDest+1]
    lda l, [wCopyGfxSrc]
    lda h, [wCopyGfxSrc+1]
.copyloop
rept 16
    ld a, [hli]
    ld [de], a
    inc de
endr
    dec b
    jr nz, .copyloop
    ld a, [rLY]
    cp $90
    ret c
    lda [wCopyGfxAmount], b
    ret

VCopyPal: ; XXX
    ld a, [wCopyPal]
    and a
    ret z
VCopyPalForce:
    ld a, $80
    ld [rBGPI], a
    ld c, rBGPD & $00ff
    ld hl, wBGPal
    ld b, 8*8
.loop
    ld a, [hli]
    ld [c], a
    djnz .loop
    
    ld a, $80
    ld [rOBPI], a
    ld c, rOBPD & $00ff
    ld b, 8*8
.loopo
    ld a, [hli]
    ld [c], a
    djnz .loopo
    ld a, [rLY]
    cp $90
    ret c
    xor a
    ld [wCopyPal], a
    ret
