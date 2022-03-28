import pandas as pd
import glob

# get all csv files from the filtered 5pm data path
path = r".\02_17 Uhr"
all_files = glob.glob(path + "/*.csv")
#print(all_files)

# get the first file and remove it from the list of all csv files
erste_datei = all_files.pop(0)
#print(erste_datei)
#print(all_files)

# create dataframe from the first csv file
main_df = pd.read_csv(erste_datei)
stations_name = erste_datei[15:-10]
main_df.rename(columns={"Adcon_SM_10cm [vol%]": stations_name}, inplace=True)

# go through all other csv files and add their contents to the first file
for csv in all_files:
    df = pd.read_csv(csv)
    if "Adcon_SM_10cm [vol%]" in df:
        stations_name = csv[15:-10]
        df.rename(columns={"Adcon_SM_10cm [vol%]": stations_name}, inplace=True)
        main_df = main_df.merge(df[["TAG", stations_name]], how="left", on="TAG")
    
#main_df = main_df.T
#print(main_df)

# export the combined dataframe, that contains all the contents of all previos csv files, into one single csv file
main_df.rename(columns={"TAG": "Station"}, inplace=True)
main_df.to_csv("17Uhr_final.csv", index=False, na_rep="NULL")

# turn all NA-values to "NULL"
pd.read_csv("17Uhr_final.csv", header=None).T.to_csv("17Uhr_final.csv", header=False, index=False, na_rep="NULL")

# same process for the daily means, combining all csv files into a single one
path = r".\03_SM_Means"
all_files = glob.glob(path + "/*.csv")
#print(all_files)

erste_datei = all_files.pop(0)
#print(erste_datei)
#print(all_files)

main_df = pd.read_csv(erste_datei)
stations_name = erste_datei[17:-10]
main_df.rename(columns={"Adcon_SM_10cm_DailyMeans [vol%]": stations_name}, inplace=True)

for csv in all_files:
    
    df = pd.read_csv(csv)
    if "Adcon_SM_10cm_DailyMeans [vol%]" in df:
        stations_name = csv[17:-10]
        df.rename(columns={"Adcon_SM_10cm_DailyMeans [vol%]": stations_name}, inplace=True)
        main_df = main_df.merge(df[["TAG", stations_name]], how="left", on="TAG")
    
#main_df = main_df.T
#print(main_df)
main_df.rename(columns={"TAG": "Station"}, inplace=True)
main_df.to_csv("daily_means_final.csv", index=False, na_rep="NULL")

pd.read_csv("daily_means_final.csv", header=None).T.to_csv("daily_means_final.csv", header=False, index=False, na_rep="NULL")
