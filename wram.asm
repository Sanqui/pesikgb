SECTION "tilemap, sprites",WRAM0[$c000]
    ds $300
SECTION "misc",WRAM0[$c300]
wSPtmp: ds 2
wMenuChoice: ds 1
wMenuChoices: ds 1
wMenuStarty: ds 1
wMenuStartx: ds 1
wMenuOptionMode: ds 1
wMenuCallbacks: ds 2

wCameraY: ds 2
wCameraX: ds 2
;wScrollLeftOff: ds 2

wCopyRowAmount: ds 1
wCopyRowDest: ds 2
wCopyRowData: ds 32
wCopyRowDataEnd: ds 1
wCopyRowDataAttr: ds 32

wCopyColAmount: ds 1
wCopyColDest: ds 2
wCopyColData: ds 8*6
wCopyColDataEnd: ds 1
wCopyColDataAttr: ds 8*6

wCopyGfxAmount: ds 1
wCopyGfxDest: ds 2
wCopyGfxSrc: ds 3

wCopyPal: ds 1

wTmpSpriteY: ds 1
wTmpSpriteX: ds 1

wTmpSpriteHeight: ds 1
wTmpSpriteWidth: ds 1

wCurY: ds 2
wCurX: ds 2

wCollisionTestMap: ds 8*4
wCollisionTestMap2: ds 8*4

wMoveHor: db
wMoveVert: db
wMoved: db

wNewPosition: ds 3
wNewPositionBackup: ds 3

wTestObjectY: dw
wTestObjectX: dw

wMenuOpen: db
wMenuOption: db

SECTION "Map Objects",WRAM0[$c500]
wMapObject0:
.ysub: db
.y: dw
.xsub: db
.x: dw
.dir: db
.step: db
.sprite: db
.count: db

SECTION "vwf",WRAM0[$c600]
wVWFLetterNum:
    ds 1
wVWFChar:
    ds 1
wVWFTileLoc:
    ds 2
wVWFFirstTileNum:
    ds 1
wVWFCurTileNum:
    ds 1
wVWFCurTileCol:
    ds 1
wVWFNumTilesUsed:
    ds 1
wVWFCharWidth:
    ds 1

wVWFBuildArea0:
    ds 8
wVWFBuildArea1:
    ds 8
wVWFBuildArea2:
    ds 8
wVWFBuildArea3:
    ds 8
SECTION "vwfcopy",WRAM0[$c640]
wVWFCopyArea:
SECTION "palettes", WRAM0
wBGPal:
    ds 8*8
wOAMPal:
    ds 8*8
