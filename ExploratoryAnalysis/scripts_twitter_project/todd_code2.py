import sys
from struct import *
import feather
import pandas as pd

root_path = "/media/drosoneuro/E230270C3026E6EF/tweet_ratings/all_tweets/parsed"

inFile = sys.argv[1]
print(inFile)
sys.stdout.flush()
outFile = inFile[-7:]
export_path = root_path+"/"+outFile+"_parsed.feather"

input = open(inFile,"rb")

#converting and parsing binary file
parsed_info = [] #initiatlising list
i = 0;

while True:
    i = i+1;
    print(i)
    sys.stdout.flush()
    raw_data = input.read(27)  # read one tweet's info
    if not raw_data:
        break

    # convert the line to an array of variables, see https://docs.python.org/2/library/truct.html#struct.unpack
    data = unpack('>qffqxh',raw_data) 
    
    if (data[4]/2) > 63:
        sick = 1          
    else:
        sick = 0
    
    state = round(data[4]/2)%64

    temp_list = [data[0],data[1],data[2],data[3],sick,state]
    parsed_info.append(temp_list)


parsed_info = pd.DataFrame(parsed_info)
feather.write_dataframe(parsed_info,export_path)

