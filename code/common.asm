

DisableLCD: ; $0061
	xor a
	ld [$ff0f],a
	ld a,[$ffff]
	ld b,a
	res 0,a
	ld [$ffff],a
.waitVBlank
	ld a,[$ff44]
	cp a,$91
	jr nz,.waitVBlank
	ld a,[$ff40]
	and a,$7f	; res 7,a
	ld [$ff40],a
	ld a,b
	ld [$ffff],a
	ret

EnableLCD:
	ld a,[$ff40]
	set 7,a
	ld [$ff40],a
	ret
    
DelayFrame:
    xor a
    ld [H_PASTVBLANK], a
.loop
    halt
    ld a, [H_PASTVBLANK]
    and a
    jr z, .loop
    ret
    
CopyData:
; copy bc bytes of data from hl to de
	ld a,[hli]
	ld [de],a
	inc de
	dec bc
	ld a,c
	or b
	jr nz,CopyData
	ret

CopyDataFF:
; copy data from hl to de ending with $ff (inclusive)
	ld a,[hli]
	ld [de],a
	inc de
	inc a
	ret z
	jr CopyDataFF

WriteDataInc:
; write data in hl increasing a until b.
.loop
    ld [hli], a
    inc a
    cp a, b
    jr nz, .loop
    ret

FillMemory:
; write a in hl b times
.loop
    ld [hli], a
    dec b
    jr nz, .loop
    ret

ModuloC: ; modulo c
.loop
    cp a, c
    ret c
    sub a, c
    jr .loop

AddNTimes:
; Add bc * a to hl.
	and a
	ret z
.loop
	add hl, bc
	dec a
	jr nz, .loop
	ret


WriteSpriteRow:
    ; a = tile id
    ; b = amount
    ; de = xy
    ; hl = target
.loop
    ld [hl], d
    inc hl
    ld [hl], e
    ld c, a
    ld a, e
    add 8
    ld e, a
    ld a, c
    inc hl
    ld [hli], a
    inc a
    inc a
    ld [hl], 0
    inc hl
    dec b
    jr nz, .loop
    ret

ClearOAM:
    xor a
    ld hl, W_OAM
    ld b, 4*$28
    call FillMemory
    ret
    
ClearTilemap:
    ld hl, W_TILEMAP
    ld bc, 20*18
.loop
    xor a
    ld [hli], a
    dec bc
    ld a, b
    or c
    jr nz, .loop
    ret

; a standard function:
; this function directly reads the joypad I/O register
; it reads many times in order to give the joypad a chance to stabilize
; it saves a result in [H_JOY] in the following format
; (set bit indicates pressed button)
; bit 0 - A button
; bit 1 - B button
; bit 2 - Select button
; bit 3 - Start button
; bit 4 - Right
; bit 5 - Left
; bit 6 - Up
; bit 7 - Down
ReadJoypadRegister: ; 15F
    ld a, [H_JOY]
    ld [H_JOYOLD], a
	ld a,%00100000 ; select direction keys
	ld c,$00
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	cpl ; complement the result so that a set bit indicates a pressed key
	and a,%00001111
	swap a ; put direction keys in upper nibble
	ld b,a
	ld a,%00010000 ; select button keys
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	cpl ; complement the result so that a set bit indicates a pressed key
	and a,%00001111
	or b ; put button keys in lower nibble
	ld [$fff8],a ; save joypad state
	ld a,%00110000 ; unselect all keys
	ld [rJOYP],a
	
	ld a, [H_JOY]
	ld b, a
	ld a, [H_JOYOLD]
	xor $ff
	and b
	ld [H_JOYNEW], a
	
	ld a, [H_JOY]
	cp %00001111
	ret nz
	jp Start150
	
	ret

GetRNG:
    ld a, [rDIV]
    ld b, a
    ld a, [H_RNG1]
    xor b
    ld [H_RNG1], a
    ret

WaitForKey:
.loop
    halt
    ld a, [H_JOYNEW]
    and a, %00001001 ; A or START
    jr z, .loop
    ret

; copies DMA routine to HRAM. By GB specifications, all DMA needs to be done in HRAM (no other memory section is available during DMA)
WriteDMACodeToHRAM:
	ld c, $80
	ld b, $a
	ld hl, DMARoutine
.copyLoop
	ld a, [hli]
	ld [$ff00+c], a
	inc c
	dec b
	jr nz, .copyLoop
	ret

; this routine is copied to HRAM and executed there on every VBlank
DMARoutine:
	ld a, W_OAM >> 8
	ld [$ff00+$46], a   ; start DMA
	ld a, $28
.waitLoop               ; wait for DMA to finish
	dec a
	jr nz, .waitLoop
	ret


GetNameFromList:
    ld b, a
.loop
    ld a, b
    and a
    ret z
.strloop
    ld a, [hli]
    cp "@"
    jr nz, .strloop
    dec b
    jr .loop

GetTileAddr: ; bc = xy
    push bc
    push de
    inc c
    ld hl, W_TILEMAP
    ld e, $14
.loop
    dec c
    jr z, .end
    ld a, l
    add e
    ld l, a
    jr nc, .loop
    inc h
    jr .loop
.end
    ld a, l
    add b
    ld l, a
    jr nc, .nc
    inc h
.nc
    pop de
    pop bc
    ret

DrawBox: ; draws a box from bc to de
    call GetTileAddr ; top left corner
    ld a, $f0
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $f1
    call WriteBTimes
    ld a, $f2
    ld [hli], a
    pop bc
    ; top drawn
.mid
    inc c
    ld a, c
    cp e
    jr z, .last
    call GetTileAddr
    ld a, $f3
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    xor a
    call WriteBTimes
    ld a, $f4
    ld [hli], a
    pop bc
    jr .mid
    
.last
    call GetTileAddr ; top left corner
    ld a, $f5
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $f6
    call WriteBTimes
    ld a, $f7
    ld [hli], a
    pop bc
    ret

WriteBTimes:
; write a in hl b times
.loop
    ld [hli], a
    dec b
    jr nz, .loop
    ret

BoxChars:
    INCBIN "gfx/boxchars.2bpp"

DoMenu:
    lda c, [hli] ; y
    lda b, [hli] ; x
    push bc
    push bc
    ld a, [hli] ; h
    add c
    ld e, a
    ld a, [hli] ; 2
    add b
    ld d, a
    push hl
    call DrawBox
    pop hl
    
    pop bc
    inc c
    inc b
    inc b
    
    lda d, [hli]
    dec a
    ld [wVWFCurTileNum], a
    ld [wVWFFirstTileNum], a
    lda [wMenuOptionMode], [hli]
    lda e, [hli]
    ld [wMenuChoices], a
    
    ld a, [wMenuOptionMode]
    and a
    jr z, .loop
    inc hl
    inc hl
    lda [wMenuCallbacks], l
    lda [wMenuCallbacks+1], h
    dec hl
    ld a, [hld]
    ld l, [hl]
    ld h, a
    ld a, [wMenuOptionMode]
    jr .loop
    
.loop
    lda d, [wVWFCurTileNum]
    inc d
    push bc
    
    ld a, [wMenuOptionMode]
.print
    call PrintText
.printed
    
    pop bc
    push hl
    call GetTileAddr
    push bc
    ld a, [wVWFCurTileNum]
    add $81
    ld b, a
    ld a, [wVWFFirstTileNum]
    add $80
    call WriteDataInc
    pop bc
    pop hl
    
    inc c
    dec e
    jr nz, .loop
    
    ld a, [wMenuOptionMode]
    and a
    jr nz, .optionswereoutsourced
    lda [wMenuCallbacks], l
    lda [wMenuCallbacks+1], h
.optionswereoutsourced
    
    refreshscreen
    
    pop bc
    inc b
    inc c
    ld hl, wMenuStarty
    ld [hl], b
    inc hl
    ld [hl], c
    call GetTileAddr
    lda [hl], $f8
    xor a
    ld [wMenuChoice], a
.menuloop
    refreshscreen
    frame
    ;ld a, [H_JOYNEW]
    jrjoynew UP, .up
    jrjoynew DOWN, .down
    jrjoynew A_, .a
    jr .redrawtile
.up
    ld a, [wMenuChoice]
    and a
    jr z, .zeroth
    dec a
    jr .redraw
.zeroth
    ld a, [wMenuChoices]
    dec a
    jr .redraw
.down
    ld a, [wMenuChoices]
    dec a
    ld b, a
    ld a, [wMenuChoice]
    cp b
    jr z, .last
    inc a
    jr .redraw
.last
    xor a
.redraw
    push af
    call GetMenuTileAddr
    xor a
    ld [hl], a
    pop af
    ld [wMenuChoice], a
.redrawtile
    call GetMenuTileAddr
    lda [hl], $f8
    ld a, [H_TIMER]
    bit 4, a
    jr z, .tile2
    lda [hl], $f9
.tile2
    jr .menuloop
.a
    lda l, [wMenuCallbacks]
    lda h, [wMenuCallbacks+1]
    ld a, [hli]
    and a
    jr z, .nocallbacks
.callbacks
    ld a, [wMenuChoice]
    add a
    ld c, a
    ld b, 0
    add hl, bc
    ld a, [hli]
    ld h, [hl]
    ld l, a
    jp hl
.nocallbacks
    ld a, [wMenuChoice]
    ret

GetMenuTileAddr:
    ld hl, wMenuStarty
    lda b, [hli]
    ld a, [hl]
    ld hl, wMenuChoice
    add [hl]
    ld c, a
    jp GetTileAddr

