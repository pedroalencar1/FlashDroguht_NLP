#%%
from pytrends.request import TrendReq
from pytrends import dailydata

import pandas as pd
import os
from time import sleep

#%%
# states = ['DE-SH', 'DE-ST', 'DE-HB', 'DE-HH']
# states = ['DE-BW', 'DE-BY', 'DE-BE', 'DE-BB',
#           'DE-HE', 'DE-NI', 'DE-MV', 'DE-NW',
#           'DE-RP', 'DE-SL', 'DE-SN', 'DE-TH',]
states = ['DE-BY']
years = range(2006,2007)
months = range(9,10)
# %%

for state in states:    

    for year in years:
        
        for month in months:
            
            df = pd.DataFrame()
            
            while True:
                try:
                    df = dailydata.get_daily_data('Dürre', year, month, year, month, 
                                                geo = state)
                    break
                    
                except: # Replace Exception with something more specific.
                    print("error ", year, "-", month)
                    sleep(1)
                    continue
        
            name = "/gt_" + str(state) + "_" + str(year) + "_" + str(month) + ".csv"
            df.to_csv(os.getcwd()+name)



# %%
df = dailydata.get_daily_data('Dürre', 2006, 9, 2006, 9, 
                                                geo = 'DE-BY')
# %%
