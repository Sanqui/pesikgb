rJOYP EQU $ff00 ; Joypad (R/W)
rSB EQU $ff01 ; Serial transfer data (R/W)
rSC EQU $ff02 ; Serial Transfer Control (R/W)
rDIV EQU $ff04 ; Divider Register (R/W)
rTIMA EQU $ff05 ; Timer counter (R/W)
rTMA EQU $ff06 ; Timer Modulo (R/W)
rTAC EQU $ff07 ; Timer Control (R/W)
rIF EQU $ff0f ; Interrupt Flag (R/W)
rNR10 EQU $ff10 ; Channel 1 Sweep register (R/W)
rNR11 EQU $ff11 ; Channel 1 Sound length/Wave pattern duty (R/W)
rNR12 EQU $ff12 ; Channel 1 Volume Envelope (R/W)
rNR13 EQU $ff13 ; Channel 1 Frequency lo (Write Only)
rNR14 EQU $ff14 ; Channel 1 Frequency hi (R/W)
rNR21 EQU $ff16 ; Channel 2 Sound Length/Wave Pattern Duty (R/W)
rNR22 EQU $ff17 ; Channel 2 Volume Envelope (R/W)
rNR23 EQU $ff18 ; Channel 2 Frequency lo data (W)
rNR24 EQU $ff19 ; Channel 2 Frequency hi data (R/W)
rNR30 EQU $ff1a ; Channel 3 Sound on/off (R/W)
rNR31 EQU $ff1b ; Channel 3 Sound Length
rNR32 EQU $ff1c ; Channel 3 Select output level (R/W)
rNR33 EQU $ff1d ; Channel 3 Frequency's lower data (W)
rNR34 EQU $ff1e ; Channel 3 Frequency's higher data (R/W)
rNR41 EQU $ff20 ; Channel 4 Sound Length (R/W)
rNR42 EQU $ff21 ; Channel 4 Volume Envelope (R/W)
rNR43 EQU $ff22 ; Channel 4 Polynomial Counter (R/W)
rNR44 EQU $ff23 ; Channel 4 Counter/consecutive; Inital (R/W)
rNR50 EQU $ff24 ; Channel control / ON-OFF / Volume (R/W)
rNR51 EQU $ff25 ; Selection of Sound output terminal (R/W)
rNR52 EQU $ff26 ; Sound on/off
rLCDC EQU $ff40 ; LCD Control (R/W)
rSTAT EQU $ff41 ; LCDC Status (R/W)
rSCY EQU $ff42 ; Scroll Y (R/W)
rSCX EQU $ff43 ; Scroll X (R/W)
rLY EQU $ff44 ; LCDC Y-Coordinate (R)
rLYC EQU $ff45 ; LY Compare (R/W)
rDMA EQU $ff46 ; DMA Transfer and Start Address (W)
rBGP EQU $ff47 ; BG Palette Data (R/W) - Non CGB Mode Only
rOBP0 EQU $ff48 ; Object Palette 0 Data (R/W) - Non CGB Mode Only
rOBP1 EQU $ff49 ; Object Palette 1 Data (R/W) - Non CGB Mode Only
rWY EQU $ff4a ; Window Y Position (R/W)
rWX EQU $ff4b ; Window X Position minus 7 (R/W)
rKEY1 EQU $ff4d ; CGB Mode Only - Prepare Speed Switch
rVBK EQU $ff4f ; CGB Mode Only - VRAM Bank
rHDMA1 EQU $ff51 ; CGB Mode Only - New DMA Source, High
rHDMA2 EQU $ff52 ; CGB Mode Only - New DMA Source, Low
rHDMA3 EQU $ff53 ; CGB Mode Only - New DMA Destination, High
rHDMA4 EQU $ff54 ; CGB Mode Only - New DMA Destination, Low
rHDMA5 EQU $ff55 ; CGB Mode Only - New DMA Length/Mode/Start
rRP EQU $ff56 ; CGB Mode Only - Infrared Communications Port
rBGPI EQU $ff68 ; CGB Mode Only - Background Palette Index
rBGPD EQU $ff69 ; CGB Mode Only - Background Palette Data
rOBPI EQU $ff6a ; CGB Mode Only - Sprite Palette Index
rOBPD EQU $ff6b ; CGB Mode Only - Sprite Palette Data
rUNKNOWN1 EQU $ff6c ; (FEh) Bit 0 (Read/Write) - CGB Mode Only
rSVBK EQU $ff70 ; CGB Mode Only - WRAM Bank
rUNKNOWN2 EQU $ff72 ; (00h) - Bit 0-7 (Read/Write)
rUNKNOWN3 EQU $ff73 ; (00h) - Bit 0-7 (Read/Write)
rUNKNOWN4 EQU $ff74 ; (00h) - Bit 0-7 (Read/Write) - CGB Mode Only
rUNKNOWN5 EQU $ff75 ; (8Fh) - Bit 4-6 (Read/Write)
rUNKNOWN6 EQU $ff76 ; (00h) - Always 00h (Read Only)
rUNKNOWN7 EQU $ff77 ; (00h) - Always 00h (Read Only)
rIE EQU $ffff ; Interrupt Enable (R/W)

OAM EQU $FE00

; macro for easily selecting coordinates on screen
W_TILEMAP EQU $C000
W_OAM EQU $C200


;\1 = X
;\2 = Y
FuncCoord: MACRO
Coord = W_TILEMAP + 20 * \2 + \1
	ENDM
	
decoord: MACRO
    FuncCoord \2, \1
    ld de, Coord
    ENDM

hlcoord: MACRO
    FuncCoord \2, \1
    ld hl, Coord
    ENDM

detile: MACRO
    ld de, $9000 + (\1 - $80) * $10
    ENDM

hltile: MACRO
    ld hl, $9000 + (\1 - $80) * $10
    ENDM

; fast copy macros
waithblank: MACRO
.wait\@
    ld a, [rSTAT]
    and %00000011
    and a
    jr nz, .wait\@
    ENDM
    
popdetohli: MACRO
rept \1
    pop de
    ld a, e
    ld [hli], a
    ld a, d
    ld [hli], a
endr
    ENDM

; convenience
lda: MACRO
    ld a, \2
    ld \1, a
    ENDM
ld0: MACRO
    xor a
    ld \1, a
    ENDM

unhl: MACRO
    ld a, [hli]
    ld h, [hl]
    ld l, a
    ENDM

; copy bc bytes of data from hl to de
copy: MACRO
    ld de, \1
    ld hl, \2
if _NARG == 3
    ld bc, \3
else
    ld bc, \2End - \2
endc
    call CopyData
    ENDM

; write a in hl b times
fillmemory: MACRO
    ld hl, \1
    ld a, \2
    ld b, \3
    call FillMemory
    ENDM

printstatic: MACRO        
    ld d, \1
    ld hl, .localtext\@
    call PrintText
    
    hlcoord \3, \4
    ld a, [wVWFCurTileNum]
    add $81
    ld b, a
    ld a, [wVWFFirstTileNum]
    add $80
    call WriteDataInc
    
    jr .end\@
.localtext\@
    db \5
.end\@
    ENDM

print: MACRO  
    call PrintText
    writetexttotilemap \2, \3
    ENDM

writetexttotilemap: MACRO
    hlcoord \1, \2
    ld a, [wVWFCurTileNum]
    add $81
    ld b, a
    ld a, [wVWFFirstTileNum]
    add $80
    call WriteDataInc
    ENDM

refreshscreen: MACRO
    ld a, [H_VBLANKPART]
    and a
    jr nz, .dontrefresh\@
    lda [H_VBLANKPART], 2
.dontrefresh\@
    ENDM
    
frame: MACRO
    call DelayFrame
    ENDM

frame2: MACRO
    call DelayFrame
    call DelayFrame
    ENDM

domenu: MACRO
    ld hl, \1
    call DoMenu
    ENDM

A_ EQU $0
B_ EQU $1
SELECT EQU $2
START EQU $3
RIGHT EQU $4
LEFT EQU $5
UP EQU $6
DOWN EQU $7

jrjoynew: MACRO
    ld a, [H_JOYNEW]
    bit \1, a
    jr nz, \2
    ENDM
    
jpjoynew: MACRO
    ld a, [H_JOYNEW]
    bit \1, a
    jp nz, \2
    ENDM
    
calljoy: MACRO
    ld a, [H_JOY]
    bit \1, a
    call nz, \2
    ENDM

calljoynew: MACRO
    ld a, [H_JOYNEY]
    bit \1, a
    call nz, \2
    ENDM

dec16: MACRO
    ld a, [\1]
    sub a, 1
    ld [\1], a
    jr nc, .done\@
    ld a, [\1+1]
    sbc a, 0
    ld [\1+1], a
.done\@
    ENDM
    
inc16: MACRO
    ld a, [\1]
    add a, 1
    ld [\1], a
    jr nc, .done\@
    ld a, [\1+1]
    adc a, 0
    ld [\1+1], a
.done\@
    ENDM

djnz: MACRO
    dec b
    jr nz, \1
ENDM

signextend: MACRO
    ld \1, 0
    bit 7, \2
    jr z, .done\@
    ld \1, -1
.done\@
ENDM


H_TIMER EQU $FFF0
H_RNG1 EQU $FFF1
H_VBLANKPART EQU $FFF2
H_PASTVBLANK EQU $FFF3
H_SCX EQU $FFF4
H_SCY EQU $FFF5
H_JOY EQU $fff8
H_JOYOLD EQU $fff9
H_JOYNEW EQU $fffA
