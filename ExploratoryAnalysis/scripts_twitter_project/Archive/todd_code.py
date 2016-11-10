# from __future__ import print_function
from struct import *
import sys



input = open(sys.stdin.fileno(),"rb")


while True:

    raw_data = input.read(27)  # read one tweet's info

    if not raw_data:

        break

    # convert the line to an array of variables, see https://docs.python.org/2/library/struct.html#struct.unpack
    data = unpack('>qffqxh',raw_data)
    
    sick = 1 if (data[4]/2) > 63 else 0
    
    state = round(data[4]/2)%64
    
    print(data[0],data[1],data[2],data[3],sick,state,sep=",")
