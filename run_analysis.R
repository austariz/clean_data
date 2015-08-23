## LOADS DPLYR/TIDYR PACKAGES IN CASE RUNNING COMPUTER DOES NOT HAVE THEM
require(dplyr)
require(tidyr)
library(dplyr)
library(tidyr)

## CREATE VARIABLES TO GET COLUMN NAMES / ACTIVITY DESCRIPTIONS FOR ALL FILES
read_ttl_X <- read.table("UCI HAR Dataset/features.txt")
ttl_X_cols <- c("id","Xtitle")
colnames(read_ttl_X) <- ttl_X_cols
ttl_X <- read_ttl_X$Xtitle
ttl_X <-  gsub(pattern = "\\()", replacement = "", ttl_X)

ttl_Y <- c("Activity_Id")
ttl_subject <- c("Subject_No")

read_act_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
colnames(read_act_labels) <- c("Activity_Id","Activity")

ttl_table <- bind_rows(as.data.frame(ttl_subject),
                       as.data.frame(ttl_Y),
					   as.data.frame(ttl_X))

desc_Y <- read.table("UCI HAR Dataset/activity_labels.txt")

## STARTS LOOP TO PREPARE/MERGE THE DATA COMING FROM BOTH TEST AND TRAIN FOLDERS
for (i in 1:2) 
    {
## LIST SUB-FOLDERS IN "UCI HAR Dataset" WITHIN THE WORKING DIRECTORY [getwd()]
     ls_dir <- list.dirs(path = "UCI HAR Dataset", full.names = TRUE)
## HARDCODED ROOT TO DATA FOLDER ROOTS FOR THE TEST AND TRAIN FOLDERS
     ls_dir <- ls_dir[c(2,4)]
## HARDCODED FILE NAMES (2nd FRAGMENT) DEPENDING ON THE ROOT FOLDER WHERE THE
## DATA IS BEING FETCHED
	 ls_type <- c("test","train")
## LOOP FOR FOLDER ROOT AND FILE NAME (2nd FRAGMENT) 
	 ls_dir <- ls_dir[i]
	 ls_type <- ls_type[i]

## LOOP INSIDE EACH SUB-FOLDER TO READ DATA FROM *.TXT FILES	 
	 for (j in 1:3)
	     {
## HARDCODED FILE NAMES (1st FRAGMENT) DEPENDING ON THE ROOT FOLDER WHERE THE
## DATA IS BEING FETCHED
	      ls_file <- c("X","Y","subject")
		 ## LOOP FOR FILE NAME (1st FRAGMENT) 
		  ls_file <- ls_file[j]

## CREATE VECTORS TO CONTAIN THE ASSEMBLY AND COMPLETE ROOT TO FILES
		  dir_file <- vector()
		  data_file <- vector()
		  dir_fl_root <- vector()

## FILES ROOT AND FILE NAME CONSTRUCTION - 
		  data_file <- paste(ls_file,ls_type,sep="_")
		  dir_file <- paste(ls_dir,data_file,sep="/")
		  dir_fl_root <- paste(dir_file,"txt",sep=".")

## LOOP TO ASSIGN THE VARIABLE NAME TO READ EACH DATA FILE
		  var_name <- paste(paste("data",ls_type,sep="_"),ls_file,sep="_")
		  
## READ DATA FROM *.TXT FILES AND CREATE A DATAFRAME VARIABLE WITH THE DATA
		  assign(var_name,
		            read.table(dir_fl_root, , header = FALSE,
                                  , nrows = -1, na.strings = "NA"))
		  
          #print(var_name)
		 }
    }
##   ASSIGN COLUMN NAMES TO EACH DATAFRAME RESULTING FROM THE LOOP
colnames(data_test_X) <- ttl_X
colnames(data_test_Y) <- ttl_Y
colnames(data_test_subject) <- ttl_subject
data_test <- bind_cols(data_test_subject,data_test_Y,data_test_X)
colnames(data_train_X) <- ttl_X
colnames(data_train_Y) <- ttl_Y
colnames(data_train_subject) <- ttl_subject

##   BIND DATAFRAMES INTO ONE PER TYPE - TEST AND TRAIN
data_train <- bind_cols(data_train_subject,data_train_Y,data_train_X) 
data_clean <- bind_rows(data_test,data_train)

##   JOIN MERGED DATASET TO ACTIVITY LABELS TO USE THE DESCRIPTIVE ACTIVITY NAMES
data_clean <- merge(data_clean, read_act_labels, "Activity_Id") %>%
              select(-Activity_Id) 
##   FILTER TO COLUMNS CONTAINING "MEAN" OR "STD" - TAKE OUT THOSE CALCULATING FREQUENCY
data_clean <- select(data_clean,Subject_No, Activity, matches("mean|std"))
data_clean <- select(data_clean,-contains("freq"))

##   CREATE A LONG VERSION OF TEH DATASET TO SIMPLIFY THE LAST STEP OF PROJECT
data_clean <- gather(data_clean, Variable, Value, -Subject_No, -Activity)

##   CREATE INDEPENDENT TIDY DATASET WITH THE AVERAGE OF EACH VARIABLE FOR
##   EACH ACTIVITY AND EACH SUBJECT
data_cln_AVG <- data.frame()
data_cln_AVG <- data_clean %>%
  group_by(Subject_No, Activity, Variable) %>%
  summarize(Average = mean(Value))


##   WRITE *.TXT FILE WITH CLEAN DATASET
write.table(data_cln_AVG, file="data_clean.txt", col.names=TRUE, row.names=FALSE, sep=";")	