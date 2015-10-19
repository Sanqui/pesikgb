from sys import argv
import json

for arg in argv[1:]:
    map = json.load(open(arg))
    with open(arg.split('.')[0]+".bin", 'wb') as out:
        for tile in map['layers'][0]['data']:
            out.write(chr(tile))
