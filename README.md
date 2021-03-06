# clean_data

Coursera: Getting and Cleaning Data

Project
=======
You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

===========
DEVELOPMENT
===========

NOTES: ******* CLEANED VARAIBLE NAES BY REMOVING THE '()' CHARACTERS FROM LABELS. VARIABLES ARE PLAINLY SELF-EXPLANATORY
       ******* FILTERED OUT 'MEAN FREQUENCY' VARIABLES, SINCE THEY ARE A CALCULATION FROM ORIGINAL MEASUREMENTS

Please make sure you have the decompressed folder "UCI HAR Dataset" containing the data in your working directory

the code requires the DPLYR and TIDYR packages to run seamlessly. The code includes will make sure those packages are installed.

To run the code use:
source("run_analysis.R") -- it was tested both in RStudio(Version 0.99.467) and R (Version 3.2.1 (2015-06-18))

After the code is run a file named "data_clean.txt" will be created in the working directory
The independent tidy dataset with the average for each subject, activity, variable is: "data_cln_AVG"


--- COOKBOOK ---
================
The following are the steps in which the code is going to ressovle:


 LOADS DPLYR/TIDYR PACKAGES IN CASE RUNNING COMPUTER DOES NOT HAVE THEM
 CREATE VARIABLES TO GET COLUMN NAMES / ACTIVITY DESCRIPTIONS FOR ALL FILES
 STARTS LOOP TO PREPARE/MERGE THE DATA COMING FROM BOTH TEST AND TRAIN FOLDERS
 LIST SUB-FOLDERS IN "UCI HAR Dataset" WITHIN THE WORKING DIRECTORY [getwd()]
 HARDCODED ROOT TO DATA FOLDER ROOTS FOR THE TEST AND TRAIN FOLDERS
 HARDCODED FILE NAMES (2nd FRAGMENT) DEPENDING ON THE ROOT FOLDER WHERE THE DATA IS BEING FETCHED
 LOOP FOR FOLDER ROOT AND FILE NAME (2nd FRAGMENT) 
 LOOP INSIDE EACH SUB-FOLDER TO READ DATA FROM *.TXT FILES	 
 HARDCODED FILE NAMES (1st FRAGMENT) DEPENDING ON THE ROOT FOLDER WHERE THE DATA IS BEING FETCHED
 CREATE VECTORS TO CONTAIN THE ASSEMBLY AND COMPLETE ROOT TO FILES
 FILES ROOT AND FILE NAME CONSTRUCTION - 
 LOOP TO ASSIGN THE VARIABLE NAME TO READ EACH DATA FILE
 READ DATA FROM *.TXT FILES AND CREATE A DATAFRAME VARIABLE WITH THE DATA
 ASSIGN COLUMN NAMES TO EACH DATAFRAME RESULTING FROM THE LOOP
 BIND DATAFRAMES INTO ONE PER TYPE - TEST AND TRAIN
 JOIN MERGED DATASET TO ACTIVITY LABELS TO USE THE DESCRIPTIVE ACTIVITY NAMES
 FILTER TO COLUMNS CONTAINING "MEAN" OR "STD" - TAKE OUT THOSE CALCULATING FREQUENCY
 CREATE A LONG VERSION OF TEH DATASET TO SIMPLIFY THE LAST STEP OF PROJECT
 WRITE *.TXT FILE WITH CLEAN DATASET
 CREATE INDEPENDENT TIDY DATASET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT


VARIABLES

"data_clean" - cLEAN DATASET SOLVING ITEMS 1-4 OF TEH PROJECT
"data_cln_AVG" - AGGREGATED DATASET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT - SOLVES ITEM 5 OF THE PROJECT
"data_file" - ASSEMBLES THE NAME OF THE FILE TO BE READ WITHIN THE TEST AND TRAIN FOLDERS      
"data_test" - MERGED DATASET WITH SUBJECTS, ACTIVITIES AND METRICS FOR TEST
"data_test_subject" - DATAFRAME FROM READING SUBJECT FILE IN TEST FOLDER
"data_test_X" - DATAFRAME FROM READING METRICS FILE IN TEST FOLDER  
"data_test_Y" - DATAFRAME FROM READING ACTIVITIES FILE IN TEST FOLDER      
"data_train" - MERGED DATASET WITH SUBJECTS, ACTIVITIES AND METRICS FOR TRAIN
"data_train_subject" - DATAFRAME FROM READING SUBJECT FILE IN TRAIN FOLDER
"data_train_X" - DATAFRAME FROM READING METRICS FILE IN TEST FOLDER  
"data_train_Y" - DATAFRAME FROM READING ACTIVITIES FILE IN TEST FOLDER       
"desc_Y" - DATAFRAME READING ACTIVITY LABELS         
"dir_file" - ASSEMBLES THE NAME OF THE FILE TO BE "UCI HAR Dataset" TO CREATE ROOT TO READ FILES      
"dir_fl_root" - ASSEMBLES THE FULL PATH TO READ ALL FILES AND CREATE DATAFRAMES WITH THE DATA      
"i" - LOOP VARIABLE TO READ FILES WITH THE DATA (X, Y, sUBJECT)               
"j" - LOOP VARIABLE TO GO OVER TEST AND TRAIN FOLDERS
"ls_dir" - LIST DIRECTORIES AND FILTERS TO TEST AND TRAIN FOLDERS
"ls_file" - HARDCODED VECTOR TO ASSEMBLE THE FILE NAMES TO BE READ
"ls_type" - HARDCODED VECTOR WITH NAMES FOR TEST AND TRAIN FOLDERS
"read_act_labels" - DATAFRAME CONTAINING DE ACTIVITIES DESCRIPTIONS
"read_ttl_X" - DATAFRAME FROM READ FILE FOR VARIABLE NAMES
"ttl_subject" - HARDCODED VECTOR WITH COLUMN TITLE FOR SUBJECTS
"ttl_table" - DATAFRAME BINDING COLUMNS FOR SUBJECT, ACTIVITIES AND MEASURES (SUBJECT, Y, X)
"ttl_X" - DATAGFRAME GETS THE VARIABLE TITLES AND REMOVES '()' FROM THE TITLES
"ttl_X_cols" - HARDCODES THE COLUMN NAMES FOR VARIABLE LABELS DATAFRAME (FEATURES)
"ttl_Y" - HARDCODED LABEL FOR ACTIVITY DATAFRAME
"var_name" - CALCULATES THE NAME TO DATAFRAMES TO CONTAIN THE DATA
