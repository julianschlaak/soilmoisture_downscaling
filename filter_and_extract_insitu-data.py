import glob
import pandas as pd

daten_path = r"./00_Rohdaten" # path of the original insitu-data
sm_path = r"./01_SM" # path of the filtered insitu-data, that only contains the useable soil moisture data
uhr17_path = r"./02_17 Uhr" # path of the filtered soil moisture insitu-data, but just the 5 pm values for each day
sm_means_path = r"./03_SM_Means" # path of the filtered soil moisture insitu-data, but with daily means

# get all csv-files from the original insitu-data path
for csv in glob.glob(daten_path + "/*.csv"):

    date_list = []
    date_17uhr_list = []
    time_list = []
    aquacheck_list = []
    adcon_list = []
    aquacheck_17uhr_list = []
    adcon_17uhr_list = []
	
	# filter if "_CL_" is in the file name of the insitu-data, as only the "_CL_" files contain useable soilmoisture data
    if "_CL_" in str(csv):
		# open the filtered data, seperate it into rows
        csv_content = open(csv, 'rb').readlines()
        # get the header/columns names from the first row
		header = str(csv_content[0])
        header = header.split(",")
		
		# check if the opened insitu-data features one of the three possible header name combinations

        if "AquaCheck_Soilmoisture010cm[% vol]" in header:
            aquacheck = 1
            aquacheck_index = header.index("AquaCheck_Soilmoisture010cm[% vol]")
        else:
            aquacheck = 0
        if "AdconSM1_Soilmoisture010cm[% vol]" in header:
            adcon = 1
            adcon_index = header.index("AdconSM1_Soilmoisture010cm[% vol]")
        else:
            adcon = 0

		# only proceed if the insitu-data file contains one of the three possible header name combinations
        if aquacheck == 1 or adcon == 1:
            print("Lese:", csv, end = "\r")
			
			# remove the first row (header) of the opened insitu-data 
            csv_content.pop(0)
			
			# go through every row/line in the opened insitu-data
            for line in csv_content:
                values = line.decode("utf-8") 
                #values =  str(line)
                values = values.split(",")

				# get date and time variables for filtering
                date_time = values[0]
                date = date_time[0:10]
                year = date[0:4]
                month = date[5:7]
                day = date[8:10]
                
                time = date_time[11:19]

                # filter by year as well as time and extract the relevant soil moisture data into lists
                if "2019" == year or "2020" == year:
                    if aquacheck == 1 and adcon == 1:
                        date_list.append(date)
                        time_list.append(time)
                        aquacheck_list.append(values[aquacheck_index])
                        adcon_list.append(values[adcon_index])

                        if time == "17:00:00":
                            aquacheck_17uhr_list.append(values[aquacheck_index])
                            adcon_17uhr_list.append(values[adcon_index])
                            date_17uhr_list.append(date)

                    elif aquacheck == 1 and adcon == 0:
                        date_list.append(date)
                        time_list.append(time)
                        aquacheck_list.append(values[aquacheck_index])

                        if time == "17:00:00":
                            aquacheck_17uhr_list.append(values[aquacheck_index])
                            date_17uhr_list.append(date)

                    elif aquacheck == 0 and adcon == 1:
                        date_list.append(date)
                        time_list.append(time)
                        adcon_list.append(values[adcon_index])
                        
                        if time == "17:00:00":
                            adcon_17uhr_list.append(values[adcon_index])
                            date_17uhr_list.append(date)

            print("...done")
            #print(len(date_list), len(aquacheck_list), len(adcon_list))

			# create dataframes containing the extracted data
            if aquacheck == 1 and adcon == 1:
                sm_all_dict = {"TAG":date_list, "ZEIT":time_list, "Adcon_SM_10cm [vol%]":adcon_list, "AquaCheck_SM_10cm [vol%]":aquacheck_list}
                sm_17uhr_dict = {"TAG":date_17uhr_list, "Adcon_SM_10cm [vol%]":adcon_17uhr_list, "AquaCheck_SM_10cm [vol%]":aquacheck_17uhr_list}
            elif aquacheck == 1 and adcon == 0:
                sm_all_dict = {"TAG":date_list, "ZEIT":time_list, "AquaCheck_SM_10cm [vol%]":aquacheck_list}
                sm_17uhr_dict = {"TAG":date_17uhr_list, "AquaCheck_SM_10cm [vol%]":aquacheck_17uhr_list}
            elif aquacheck == 0 and adcon == 1:
                sm_all_dict = {"TAG":date_list, "ZEIT":time_list, "Adcon_SM_10cm [vol%]":adcon_list}
                sm_17uhr_dict = {"TAG":date_17uhr_list, "Adcon_SM_10cm [vol%]":adcon_17uhr_list}

			# get name of station from the original csv-file
            stations_name = str(csv)
            stations_name = stations_name.split("_CL_")
            stations_name = stations_name[0]
            stations_name = stations_name[14:]
            sm_export1 = sm_path + "/sm_" + stations_name.lower()
            sm_export2 = uhr17_path + "/sm_" + stations_name.lower()
            #print(export_name)

            print("Ausgabe als: " + sm_export1, end = "\r")
            
			# convert dataframes to csv-files
            sm_all_df = pd.DataFrame(sm_all_dict)
            sm_all_df = sm_all_df.sort_values(by=["TAG"], ascending=True)
            sm_all_df.to_csv(sm_export1 + "_all.csv", index=False)

            sm_17uhr_df = pd.DataFrame(sm_17uhr_dict)
            sm_17uhr_df = sm_17uhr_df.sort_values(by=["TAG"], ascending=True)
            sm_17uhr_df.to_csv(sm_export2 + "_17uhr.csv", index=False)

            print("...done\n")

# load in all the csv-files from filtered soil moisture data path
for sm in glob.glob(sm_path + "/*csv"):

	# check if the csv-file has the given ending string, that was given in the code block above
    if "_all.csv" in sm:
        print("Bearbeite:", sm, end = "\r")
		
		# create a dataframe for every csv-file and sort by day
        sm_df = pd.read_csv(sm, parse_dates=["TAG"])

		# get the name of the station from the original csv file name
        stations_name = str(sm)
        stations_name = stations_name.split("_all")
        stations_name = stations_name[0]
        stations_name = stations_name[8:]
        sm_means_export = sm_means_path + "/" + stations_name.lower()
        
		# create daily means for the filtered insitu-data
        sm_df = sm_df.drop(columns=["ZEIT"])
        if "AquaCheck_SM_10cm [vol%]" in sm_df and "Adcon_SM_10cm [vol%]" in sm_df:
            sm_df["Adcon_SM_10cm_DailyMeans [vol%]"] = sm_df["Adcon_SM_10cm [vol%]"].groupby(sm_df["TAG"]).transform("mean")
            sm_df["AquaCheck_SM_10cm_DailyMeans [vol%]"] = sm_df["AquaCheck_SM_10cm [vol%]"].groupby(sm_df["TAG"]).transform("mean")
            sm_df = sm_df.drop(columns=["AquaCheck_SM_10cm [vol%]", "Adcon_SM_10cm [vol%]"])
        elif "AquaCheck_SM_10cm [vol%]" in sm_df and "Adcon_SM_10cm [vol%]" not in sm_df:
            sm_df["AquaCheck_SM_10cm_DailyMeans [vol%]"] = sm_df["AquaCheck_SM_10cm [vol%]"].groupby(sm_df["TAG"]).transform("mean")
            sm_df = sm_df.drop(columns=["AquaCheck_SM_10cm [vol%]"])
        elif "AquaCheck_SM_10cm [vol%]" not in sm_df and "Adcon_SM_10cm [vol%]" in sm_df:
            sm_df["Adcon_SM_10cm_DailyMeans [vol%]"] = sm_df["Adcon_SM_10cm [vol%]"].groupby(sm_df["TAG"]).transform("mean")
            sm_df = sm_df.drop(columns=["Adcon_SM_10cm [vol%]"])
        sm_df = sm_df.drop_duplicates()
        print("...done")

		# export the file as csv to the given path
        print("Ausgabe als: " + sm_means_export + "_means.csv", end = "\r")
        sm_df.to_csv(sm_means_export + "_means.csv", index=False, na_rep='NULL')
        print("...done\n")
    


