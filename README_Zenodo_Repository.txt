This repository contains the following data sets related to Detecting Plumes in Mobile Air Quality Monitoring Time Series with DBSCAN.

Validated_Data.csv: A .csv file containing the validation set used in the study. Column headings are the following:

"Lat1": GPS latitude of car location in degrees.
"Long1": GPS longitude of car location in degrees.
"LST": Measurement time stamp. Time zone US/Central.
"BC": Black carbon measurements in ng/m^3
"CO2": Carbon dioxide measurements in ppm.
"UFP": Ultrafine particle count in particles/cc.
"NOx": Oxides of nitrogen, defined as the sum of NO and NO2, in ppb.
"Anomaly": What has been manually flagged as "Anomaly" (2) or "Normal" (1).
"Uniq_Fac": Factor from 1-30 mapping to different days of the campaign. For example, all measurements with Uniq_Fac = 1 belong to the same day.

Labeled_DBSCAN_Anomalies.csv: A .csv file containing points labeled as anomalies by the DBSCAN algorithm described in the manuscript. Columns are the following.

"BC": Black carbon measurement (ng/m^3)
"CO2": Carbon dioxide measurement (ppm)
"NOx": Oxides of nitrogen, defined as the sum of NO and NO2 (ppb)
"UFP": Ultrafine particle count (p/cc)
"Anomaly": Whether the DBSCAN algorithm has labeled this point as "Anomaly" (2) or "Normal" (1)
"Uniq_Fac": Factor spanning from 1-277 grouping measurements taken on separate days by car. E.g. all measurements with Uniq_Fac=1 were grouped and analyzed together.
"LST": Timestamp (US/Central)
"Road_Class": TigerLINE census road class designation for the given point. Possible road classes are S1100 - Primary Road, S1200 - Secondary Road, S1400 - Local Road, S1630 - Ramps, S1640 - Service Drives, S1730 - Private Roads
"X": Universal Transverse Mercator Easting for Zone 15N (m).
"Y": Universal Transverse Mercator Northing for Zone 15N (m).

*_To_Be_Validated.csv: A series of files where * denotes the following.

"DB": DBSCAN Algorithm
"QOR": QOR Algorithm
"QAND": QAND Algorithm
"Drew": Drewnick Algorithm

Each file contains the following columns:

"Lat1": GPS latitude of car location in degrees.
"Long1": GPS longitude of car location in degrees.
"LST": Measurement time stamp. Time zone US/Central.
"BC": Black carbon measurements in ng/m^3
"CO2": Carbon dioxide measurements in ppm.
"UFP": Ultrafine particle count in particles/cc.
"NOx": Oxides of nitrogen, defined as the sum of NO and NO2, in ppb.
"Anomaly": What has been flagged as "Anomaly" (2) or "Normal" (1).
"Uniq_Fac": Factor from 1-30 mapping to different days of the campaign. For example, all measurements with Uniq_Fac = 1 belong to the same day.
