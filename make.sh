#!/bin/sh
set -ve
#tiled --export-map maps/test.tmx maps/test.json
#tiled --export-map maps/cave.tmx maps/cave.json
python map.py maps/*.json
python gfx_new.py 2bpp gfx/*.png
python gfx_new.py 1bpp masks/*.png
rgbasm -E -o test.o test.asm
rgblink -n test.sym -m test.map -o test.gbc test.o
rgbfix -jv -c -i XXXX -k XX -l 0x33 -m 0x01 -p 0 -r 0 -t test test.gbc

