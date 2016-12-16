#using https://github.com/pegler/pytzwhere
#after installing dependencies, call "pip install git+https://github.com/pegler/pytzwhere" within the environment you want to have it installed
#for fast import of data, install feather-format package: https://pypi.python.org/pypi/feather-format
#https://github.com/wesm/feather

from tzwhere import tzwhere
import feather
import pandas as pd

root_path = "/home/drosoneuro/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis"

tz = tzwhere.tzwhere(shapely=True, forceTZ=True) #initialising tzwhere >
#forceTZ gives result even if spatial data is not exactly within predefined timezones, but close
#shapely takes longer for initialisation, but is overall faster for big data sets
#tz.tzNameAt(40.7271, -73.98, forceTZ=True)
import_path = root_path+"/temporary/to_export.feather"
export_path = root_path+"/temporary/to_import.feather"
spatial_data = feather.read_dataframe(import_path) #reading longitude and latitude; spatial_data is a pandas.data.frame: http://pandas.pydata.org/pandas-docs/stable/dsintro.html#indexing-selection
spatial_data.size

#info from data exported from R
time_zones = [] #initialising list
loops = len(spatial_data)
for i in range(0,loops):
    print(str(i)+"/"+str(loops)) #to show user how far time_zone lookup has progressed
    temp_tz = tz.tzNameAt(spatial_data.ix[i,"latitude"],spatial_data.ix[i,"longitude"]) #the order has to be tz.tzNameAt(latitude, longitude)
    time_zones.append(temp_tz)

time_zones = pd.DataFrame(time_zones)
feather.write_dataframe(time_zones,export_path)
