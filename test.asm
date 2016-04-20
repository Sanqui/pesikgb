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

INCLUDE "code/vblank.asm"
INCLUDE "code/common.asm"
INCLUDE "code/vwf.asm"

Start:
    di
    cp $11
    jr nz, .notgbc
    ld a, 1
    ld [H_GBC], a
    jr .doneconsole
.notgbc
    xor a
    ld [H_GBC], a
.doneconsole
    
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
    ld a, l
    cp $fe
    jr nz, .loop2
    
    pop af
    ld [H_RNG1], a
    
    call WriteDMACodeToHRAM
    
    lda [wCopyRowDataEnd], $ff
    ld [wCopyColDataEnd], a
    
    ; set up ingame graphics
    copy $8f00, BoxChars, $0100
    copy wBGPal, DefaultBGPal
    copy wOAMPal, DefaultOAMPal
    call VCopyPalForce
    xor a
    ld [wCopyPal], a
        
    call EnableLCD
    
    xor a
    ld [$ffff], a
    ld a, %00000011
    ld [$ffff], a
    ; TODO clear interrupt latches
    ei
    halt
    
    call DisableLCD
    ;call WaitForKey
    
    call ClearTilemap
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
    copy $9010, PesikLargeGfx
    copy wBGPal, PesikLargePal
    call VCopyPalForce
    xor a
    ld [wCopyPal], a
    call EnableLCD
    
    printstatic $00, $0f, 1, 6, "Pesik@"
    printstatic $05, $20, $10, 0, "Code by Sanqui@"
    printstatic $10, $20, $11, 0, "Concept by Behold3r@"
    printstatic $20, $20, $0f, 4, "Do not distribute@"
    
    ; rect
    ld a, $01
    ld de, 20-7
    hlcoord 8, 2
    ld c, 6
.loopy
    ld b, 7
.loopx
    ld [hli], a
    inc a
    dec b
    jr nz, .loopx
    add hl, de
    dec c
    jr nz, .loopy
    
    
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

InitObject:
    xor a
    ld [hli], a
    lda [hli], c
    lda [hli], b
    xor a
    ld [hli], a
    lda [hli], e
    lda [hli], d
    xor a
    ld [hli], a
    ld [hli], a
    lda [hli], $80
    lda [hli], $ff
    ret

StartGame:
    frame2
    call ClearTilemap
    refreshscreen
    frame2
    
    call DisableLCD
    copy $9c00, MenuTilemap0
    copy $9c20, MenuTilemap1
    copy $9c40, MenuTilemap2
    copy $9c60, MenuTilemap3
    copy $8000, PesikGfx, 16*6
    copy wOAMPal, PesikPal
    lda [wCopyPal], 1
    copy $8b00, MenuIcons
    copy $8700, SelectorGfx
    
    ld hl, SnowyMap
    call LoadMap
    
    ld hl, wMapObject0
    ld bc, $0114
    ld de, $00c4
    call InitObject
    
    ld hl, wMapObject1
    ld bc, $0150
    ld de, $00f4
    call InitObject
    
    ld hl, wMapObject2
    ld bc, $0180
    ld de, $00c4
    call InitObject
    
    ld hl, wMapObject3
    ld bc, $0200
    ld de, $00ff
    call InitObject
    
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
    
    ld a, [H_JOYNEW]
    bit START, a
    jr z, .loop
    call DisableLCD
    ld hl, CaveMap
    call LoadMap
    ;jpjoynew START, .return
    call EnableLCD
    jr .loop

INCLUDE "code/movement.asm"
INCLUDE "code/camera.asm"

UpdateSprites:
    ;fillmemory W_OAM, 0, 4*$28
    xor a
    ld [wTmpOAMLow], a
    ld [wVisSpriteCount], a
    
    ld de, wMapObject0
    call .UpdateSprite
    ld de, wMapObject1
    call .UpdateSprite
    ld de, wMapObject2
    call .UpdateSprite
    ;ld de, wMapObject3
    ;call .UpdateSprite
    ld h, W_OAM >> 8
    ld a, [wTmpOAMLow]
.clearloop
    ld l, a
    ld [hl], 144+16
    ld a, l
    add 4
    cp $a0
    jr c, .clearloop
    ret
    
.UpdateSprite
    inc de
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
    bit 7, a
    jr z, .gfxnext
    call .UpdateSpriteGfx
    jr .gfxhandled
.gfxnext
    inc de
    ld a, [de]
    dec de
    cp $ff
    call z, .UpdateSpriteGfx
.gfxhandled
    
    xor a
    ld b, a
    inc de
    ld a, [de]
    sla a
    sla a
    sla a
    ld c, a
    
    ld h, W_OAM >> 8
    ld a, [wTmpOAMLow]
    ld l, a
    lda [hli], [wTmpSpriteY]
    lda [hli], [wTmpSpriteX]
    ld a, c
    ld [hli], a
    ld a, b
    ld [hli], a
    
    lda [hli], [wTmpSpriteY]
    ld a, [wTmpSpriteX]
    add 8
    ld [hli], a
    ld a, c
    add 2
    ld [hli], a
    ld a, b
    ld [hli], a
    
    lda [hli], [wTmpSpriteY]
    ld a, [wTmpSpriteX]
    add 16
    ld [hli], a
    ld a, c
    add 4
    ld [hli], a
    ld a, b
    ld [hli], a
    
    lda [wTmpOAMLow], l
    ld a, [wVisSpriteCount]
    inc a
    ld [wVisSpriteCount], a
    ret
    
.skip2
    inc de
    inc de
    inc de
.skip
    inc de
    inc de
    inc de
    ld a, $ff
    ld [de], a
    ret
.UpdateSpriteGfx:
    ld a, [de]
    and $7f
    ld [de], a
    ld [wTmpSpriteIndex], a
    inc de
    ld a, [wVisSpriteCount]
    ld [de], a
    and $01
    rrc a
    
    ld [wCopyGfxDest], a
    ld a, [de]
    srl a
    or $80
    ld [wCopyGfxDest+1], a
    push de
    push bc
    ld bc, 16*6
    ld hl, PesikGfx
    lda e, [wTmpSpriteIndex]
    ld d, 0
    call ScheduleNewSprite
    pop bc
    pop de
    dec de
    ret

LoadMap:
    ld de, wMapHeader
    ld bc, wMapHeaderEnd-wMapHeader
    copy
    ;ptr TilesetGfx  TilesetPal  TilesetMask  TileAttributes  Tilemap
    ldptr wTilesetGfxPtr
    bankswitch
    ld bc, $7f0
    ld de, $9010
    copy
    ld bc, $300
    ld de, $8800
    copy
    
    ldptr wTilesetPalPtr
    bankswitch
    ld bc, 2*4
    ld de, wBGPal
    copy
    
    call VCopyPalForce
    xor a
    ld [wCopyPal], a
    
    lda [rVBK], 1
    fillmemory $9c00, $87, $ff
    xor a
    ld [rVBK], a
    
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
    ret


    incdata PesikGfx, "gfx/pesik.interleave.2bpp"
    incl PesikPal, "gfx/pesik.interleave.pal"
    
    incdata PesikLargeGfx, "gfx/pesik_large.2bpp"
    incl PesikLargePal, "gfx/pesik_large.pal"

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

SnowyTileAttributes:
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $80, $80, $80, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $80, $80, $80, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    
    db $00, $80, $80, $80, $80, $80, $80, $80, $00, $80, $00, $00, $00, $00, $00, $00
    db $00, $80, $80, $80, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $80, $80, $80, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00

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

SnowyTilesetMask:
    db 0, 0, 0, 0, 0, 0, 0, 0
    INCBIN "masks/snowy.1bpp"


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

DefaultBGPal:
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
    RGB 31, 31, 31
    RGB 32/3 * 2, 30, 32/3 * 2
    RGB 32/3, 18, 32/3
    RGB 0, 0, 0, 0
DefaultBGPalEnd
DefaultOAMPal:
    RGB 31, 31, 31
    RGB 31, 31, 31
    RGB 32/3 * 2, 32/3 * 2, 32/3 * 2
    RGB 0, 0, 0, 0
DefaultOAMPalEnd

SnowyMap:
    ptr SnowyTilesetGfx
    ptr SnowyTilesetPal
    ptr SnowyTilesetMask
    ptr SnowyTileAttributes
    ptr SnowyTilemap

CaveMap:
    ptr CaveTilesetGfx
    ptr CaveTilesetPal
    ptr CaveTilesetMask
    ptr SnowyTileAttributes;CaveTileAttributes
    ptr CaveTilemap

CaveTilesetMask:
    db 0,0,0,0,0,0,0,0,0
    INCBIN "masks/cave.1bpp"


SECTION "map0", ROMX, BANK[$1]
SnowyTilemap:
    INCBIN "maps/test.bin";, $0000, $4000
;SECTION "map1", ROMX, BANK[$2]
;    INCBIN "maps/test.bin", $4000, $4000
;SECTION "map2", ROMX, BANK[$3]
;    INCBIN "maps/test.bin", $8000, $4000
;SECTION "map3", ROMX, BANK[$4]
;    INCBIN "maps/test.bin", $c000, $4000


SECTION "cave map", ROMX
CaveTilemap:
    INCBIN "maps/cave.bin"

SECTION "data", ROMX
    incdata CaveTilesetGfx, "gfx/tileset_ice.2bpp"
    incl CaveTilesetPal, "gfx/tileset_ice.pal"


    incdata SnowyTilesetGfx, "gfx/tileset.2bpp"
    incl SnowyTilesetPal, "gfx/tileset.pal"





















