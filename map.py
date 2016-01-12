import struct
from sys import argv
import json

for arg in argv[1:]:
    map = json.load(open(arg))
    with open(arg.split('.')[0]+".bin", 'wb') as out:
        for tile in map['layers'][0]['data']:
            out.write(chr(tile))
    
    if len(map['layers']) >= 2:
        with open(arg.split('.')[0]+"_obj.bin", 'wb') as out:
            for obj in map['layers'][1]['objects']:
                out.write(struct.pack("SS", obj['x']/8, obj['y']/8))
