import struct
from sys import argv
import json

for arg in argv[1:]:
    map = json.load(open(arg))
    with open(arg.split('.')[0]+".bin", 'wb') as out:
        for tile in map['layers'][0]['data']:
            out.write(chr(tile))
    
    """
wMapObject:
.ysub: ds 1
.y: ds 2
.xsub: ds 1
.x: ds 2
.dir: ds 1
.step: ds 1
.sprite: ds 1
.tileindex: ds 1
    """
    
    if len(map['layers']) >= 2:
        with open(arg.split('.')[0]+"_obj.bin", 'wb') as out:
            for obj in map['layers'][1]['objects']:
                pr = obj['properties']
                out.write(struct.pack("<BHBHBBBB",
                    obj['y']%1 * 256, int(obj['y']),
                    obj['x']%1 * 256, int(obj['x']),
                    int(pr['dir']) if 'dir' in pr else 0,
                    0,
                    int(pr['sprite']) if 'sprite' in pr else 0,
                    int(pr['ramtile'], 16) if pr['ramtile'] != "-1" else 0xff
                    ))
                out.write("\x00"*6)
