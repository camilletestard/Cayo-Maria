#THIS SCRIPT TAKES RAW INPUT FROM THE RAW FOCAL DATA AND ADLIB DATA TO MAKE STANDARISED OUTPUTS
# THE OUTPUTS ARE: 
#A FOCAL FOLLOW DATASHEET, 
#A GROOMING EDGES DATA SHEET, 
#AN ANTAGONSITIC INTERACTIONS DATASHEET, 
#AND A PROXIMITY GROUPS DATASHEET


########################################################################
# LOAD PACKAGES, LOAD AND PRE PROCESSES DATA
#########################################################################


##LOAD PACAKAGES
#.libPaths("U:/R Studio/Packages") #Defines path to where packages are stored. Probably not necessarry for others
require(tidyverse)
require(lubridate)
require(stringr)


#For each group, each year separately: 
group = c("V","V","V","KK","KK") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2016,2017,2015, 2017)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2016","V2017","KK2015","KK2017") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 

gy=4
for (gy in 3:length(groupyears)){ #For each group
  
  setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Database Complete/Data All Cleaned") 
  prev.data = read.csv(paste("Group",groupyears[gy],"_AgonsiticActions.txt",sep="")) 
  sanity.check = nrow(prev.data)
  ##LOAD DATA
  
  #Set paths to be used for working directories 
  raw.location="C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Database Complete/Data All Raw"  #path name to where to where the raw data is kept for that group in that year
  output.location="C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned"  #path to where the output is to be stored
  
  #raw.location="C:/Users/sellis4/Desktop/Database/Database/Database Complete/Data All Raw"  #path name to where to where the raw data is kept for that group in that year
  #output.location="C:/Users/sellis4/Desktop/Database/Database/Database Complete/Data All Cleaned"  #path to where the output is to be stored
  
  
  #raw.location="C:/Users/se308/Google Drive/Work/Rhesus Macaques/Projects/Database/Database Complete/Data All Raw"
  #output.location="C:/Users/se308/Google Drive/Work/Rhesus Macaques/Projects/Database/Database Complete/Data All Cleaned"
  
  
  #load population data. Special case as it only needs to be loaded once. Probably won't need to be changed too often
  setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Database Complete/Data All Cleaned") # files path to where the populaiton data is stored
  edited.populationdata=read_delim("Population details_allgroups.allyears.txt",delim=",") 
  
  
  #LOAD THE RAW FOCAL DATA
  GROUP=group[gy]  
  YEAR=years[gy] 
  GROUPYEAR=paste("Group",GROUP,YEAR,sep="")  #Add group name and year in here to save having to keep rewriting it. Later on this GROUPYEAR is pasted to make file paths
  
  setwd(raw.location)
  # raw.focaldata=read_delim(paste(GROUPYEAR,"_raw.txt",sep=""),delim="\t")
  raw.focaldata=read_delim(paste(GROUPYEAR,"_raw v2.txt",sep=""),delim=",") #If a groupyear is being reedone use this as it will work from a version (Saved later on) where the column names have already been changed
  raw.focaldata
  data.details=GROUPYEAR #Record of the name- to be changed depending on the data being processesd
  names(raw.focaldata)
  ##Edit header names . 
  #Must have headers labeeld (case and space sensitive):"Event Name", "Observation name", "Date" (insead of Day),"Month","Year","Start Time","Stop Time", "Timeblock","Observer"
  # Focal ID","Duration ,"Duration_revised", "Focal Sex","GroomNotes", Food Type", "Aggression Type", "VocType", "Redirected, "Direction , "ejinfMom", "Winner","Initiator"
  # "Activity, "Partner ID", and "PartnerSex", "In2m_ID","In3-5m_ID"
  #Changes can be made by using the code names(raw.focaldata)[X]<-"new name" where X is the column number of the offending title
  if(groupyears[gy]=="V2019"){names(raw.focaldata)[43]="PartnerSex"
  names(raw.focaldata)[37]="In3-5m_ID"
  }
  
  ####PRE PROCESSING OF FOCAL DATA
  #1. Simplfy represeantion of sex. makes all sexes into either 'male' 'female' or 'unkown'. Needs to be checked as it goes along to check all varients have been caught
  raw.focaldata$`Focal Sex`=toupper(raw.focaldata$`Focal Sex`)
  unique(raw.focaldata$`Focal Sex`)# Need to correct differnt represetations of sex
  raw.focaldata$`Focal Sex`=str_replace(raw.focaldata$`Focal Sex`," ","")
  raw.focaldata$`Focal Sex`=ifelse(raw.focaldata$`Focal Sex`=="FEMALE"|raw.focaldata$`Focal Sex`=="F","Female",raw.focaldata$`Focal Sex`)
  raw.focaldata$`Focal Sex`=ifelse(raw.focaldata$`Focal Sex`=="M"|raw.focaldata$`Focal Sex`=="MALE","Male",raw.focaldata$`Focal Sex`)
  unique(raw.focaldata$`Focal Sex`)# check all possibilites are covered
  raw.focaldata$`PartnerSex`=toupper(raw.focaldata$`PartnerSex`)
  unique(raw.focaldata$`PartnerSex`)#check that all the possibleities are covered
  raw.focaldata$`PartnerSex`=str_replace(raw.focaldata$`PartnerSex`," ","") # take out spaces
  raw.focaldata$`PartnerSex`=ifelse(raw.focaldata$`PartnerSex`=="FEMALE"|raw.focaldata$`PartnerSex`=="F","Female",raw.focaldata$`PartnerSex`)
  raw.focaldata$`PartnerSex`=ifelse(raw.focaldata$`PartnerSex`=="MALE"|raw.focaldata$`PartnerSex`=="M","Male",raw.focaldata$`PartnerSex`)
  raw.focaldata$`PartnerSex`=ifelse(raw.focaldata$`PartnerSex`=="UNKNOWN"|raw.focaldata$PartnerSex=="UNK"|raw.focaldata$PartnerSex=="?","Unknown",raw.focaldata$`PartnerSex`)
  raw.focaldata$`PartnerSex`=ifelse(raw.focaldata$`PartnerSex`=="HUMAN"|raw.focaldata$`PartnerSex`=="#N/A"|raw.focaldata$`PartnerSex`=="N/A"|raw.focaldata$PartnerSex=="0"|raw.focaldata$`PartnerSex`=="N/A"|raw.focaldata$`PartnerSex`=="NA",NA,raw.focaldata$`PartnerSex`)
  unique(raw.focaldata$`PartnerSex`)#check that all the possibleities are covered
  
  #2. Capilitalise names
  raw.focaldata$`Focal ID`= toupper(raw.focaldata$`Focal ID`)
  raw.focaldata$In2m_ID=toupper(raw.focaldata$In2m_ID)
  raw.focaldata$`In3-5m_ID`=toupper(raw.focaldata$`In3-5m_ID`)
  raw.focaldata$`Partner ID`=toupper(raw.focaldata$`Partner ID`)
  
  #3. Replace the year with the year we have type behcasue of inconsitency in eyar form
  raw.focaldata$Year=rep.int(YEAR,nrow(raw.focaldata))
  
  #4. Deal with Duplicates
  duplicates=select(raw.focaldata,`Observation name`,`Focal ID`,`Event Name`,`Start Time`,`Stop Time`)
  sum(duplicated(duplicates)) #how many duplicates
  raw.focaldata=raw.focaldata[duplicated(duplicates)==FALSE,]
  
  #5.Recalualte duration- there are problems with the raw data
  raw.focaldata<- raw.focaldata %>%
    group_by(`Observation name`) %>% # sepearte into the different observations
    mutate(sess.start= min(as.numeric(`Start Time`))) %>% #choose the minimum time in secnds
    mutate(sess.end=min(as.numeric(`Start Time`))+(10*60) )%>% # add 10 minutes to that
    ungroup()
  raw.focaldata$sess.start
  raw.focaldata$sess.end
  
  R=nrow(raw.focaldata)
  raw.focaldata$raw.duration=raw.focaldata$Duration
  raw.focaldata$raw.Duration_revised=raw.focaldata$Duration_revised
  for(i in 1:R){
    if(i %% 1000 == 0){print(paste(i, " of ",R,sep=""))}
    behv.start.secs=as.numeric(raw.focaldata$`Start Time`[i])
    behv.end.secs=as.numeric(raw.focaldata$`Stop Time`[i])
    
    raw.focaldata$Duration[i]=behv.end.secs-behv.start.secs
    
    constrained.end=ifelse(behv.end.secs>raw.focaldata$sess.end[i],raw.focaldata$sess.end[i],behv.end.secs)
    
    constrained.duration=constrained.end-behv.start.secs
    raw.focaldata$Duration_revised[i]=ifelse(constrained.duration>=0,constrained.duration,NA)
    
  }
  
  raw.focaldata
  
  ######LOAD and PROCESS THE ADLIB DATA
  setwd(raw.location)
  raw.FFadlib=read_delim(paste(GROUPYEAR,"_adlib_FFaggression.txt",sep=""),delim="\t")
  raw.MMadlib=read_delim(paste(GROUPYEAR,"_adlib_MMaggression.txt",sep=""),delim="\t")
  if (groupyears[gy]=="V2019"){names(raw.FFadlib)=c("data","winner","loser","type")}
  #edit the adlib data: edit dates, check the sexes and remove the wrong sexes, uppercase the ids
  raw.FFadlib$winner=toupper(raw.FFadlib$winner)#uppercasing ids
  raw.FFadlib$loser=toupper(raw.FFadlib$loser)
  raw.MMadlib$winner=toupper(raw.MMadlib$winner)
  raw.MMadlib$loser=toupper(raw.MMadlib$loser)
  sexes=character(nrow(raw.FFadlib)) 
  for(i in 1:nrow(raw.FFadlib)){ #loop to convert any 2 character years (e.g. 16) to 4 character (e.g. 2016)
    splitdate=str_split(raw.FFadlib$date[i],"/")
    year=ifelse(nchar(splitdate[[1]][3])==4,splitdate[[1]][3],paste("20",splitdate[[1]][3],sep=""))
    raw.FFadlib$date[i]=paste(splitdate[[1]][1],splitdate[[1]][2],year,sep="/")
    
    sex.1=left_join(raw.FFadlib,edited.populationdata,c("winner"="id"))$sex
    sex.2=left_join(raw.FFadlib,edited.populationdata,c("loser"="id"))$sex
    sexes=ifelse(sex.1=="F"&sex.2=="F","bothF","mixup")
  } #Loop also to check the FF sexes are matching and make a new variable 'sexes' that just says whether they are both F or not. 
  raw.FFadlib=raw.FFadlib[sexes=="bothF",] # discard adlib data not about FF interations
  sexes=character(nrow(raw.MMadlib)) #same as above for Ms
  for(i in 1:nrow(raw.MMadlib)){
    splitdate=str_split(raw.MMadlib$date[i],"/")
    year=ifelse(nchar(splitdate[[1]][3])==4,splitdate[[1]][3],paste("20",splitdate[[1]][3],sep=""))
    raw.MMadlib$date[i]=paste(splitdate[[1]][1],splitdate[[1]][2],year,sep="/")
    
    sex.1=left_join(raw.MMadlib,edited.populationdata,c("winner"="id"))$sex
    sex.2=left_join(raw.MMadlib,edited.populationdata,c("loser"="id"))$sex
    sexes=ifelse(sex.1=="M"&sex.2=="M","bothM","mixup")
  }
  raw.MMadlib=raw.MMadlib[sexes=="bothM",]
  
  ######################ACTION###########################################
  raw.FFadlib$date
  raw.MMadlib$date
  raw.FFadlib$newdate=as.Date(as.character(raw.FFadlib$date), "%m/%d/%Y") # may need editing depending on date format. Y [capitalised]
  raw.MMadlib$newdate=as.Date(as.character(raw.MMadlib$date), "%m/%d/%Y")
  filter(raw.FFadlib,is.na(newdate)&!is.na(date)) #checks for failed dates. If it is NA for the new data and not for the old date there is a problem NBIf a whole row is NAs it means the excel file had empty columns at the end.
  filter(raw.MMadlib,is.na(newdate)&!is.na(date))
  for(i in 1:nrow(raw.FFadlib)){
    if(is.na(raw.FFadlib$newdate[i])&!is.na(raw.FFadlib$date[i])){
      raw.FFadlib$newdate[i]=as.Date(as.character(raw.FFadlib$date[i]), "%d/%m/%Y") #or whateveter other new format is needed
    }
  }
  for(i in 1:nrow(raw.MMadlib)){
    if(is.na(raw.MMadlib$newdate[i])&!is.na(raw.MMadlib$date[i])){
      raw.MMadlib$newdate[i]=as.Date(as.character(raw.MMadlib$date[i]), "%d/%m/%Y") #or whateveter other new format is needed
    }
  }
  
  filter(raw.FFadlib,is.na(newdate)&!is.na(date)) #checks for failed dates. If it is NA for the new data and not for the old date there is a problem NBIf a whole row is NAs it means the excel file had empty columns at the end.
  filter(raw.MMadlib,is.na(newdate)&!is.na(date))  
  raw.FFadlib$date=raw.FFadlib$newdate
  raw.MMadlib$date=raw.MMadlib$newdate
  
  
  setwd(output.location)
  
  
  #####################################################################################################
  #CREATE AGONISTIC INTERACTION LIST
  #######################################################################################################
  #USES THE RAW FOCAL DATA + ADLIB DATA+ OVERTIME DATA TO CREATE A LIST OF ALL ANTAGONSITIC INTERACITONS IN THE DATABASE
  
  ###three trypes of entry data: focal follows, overstime from focal follows, ad lib. Deal wiuth each spereatly
  
  ##Focal data.
  names(raw.focaldata)
  aggressive.edges=filter(raw.focaldata,`Event Name`=="contactAgg"|`Event Name`=="displace"|`Event Name`=="noncontactAgg"|`Event Name`=="threat"|`Event Name`=="avoid"|`Event Name`=="FearGrm"|`Event Name`=="Submit")
  aggressive.edges$full.date=as.Date(paste(aggressive.edges$Date,aggressive.edges$Month,aggressive.edges$Year,sep="/"),format="%d/%m/%Y")
  
  #make sure Direction and Winner columns are filled. #CT added
  winner_idx = str_detect(aggressive.edges$`Behavior Modifier_1`,"winner") 
  direction_idx = str_detect(aggressive.edges$`Behavior Modifier`,"direct'n")
  aggressive.edges$Winner[winner_idx] = aggressive.edges$`Behavior Modifier_1`[winner_idx]
  aggressive.edges$Direction[direction_idx]=aggressive.edges$`Behavior Modifier`[direction_idx]
  
  #work out who won
  R=nrow(aggressive.edges)
  aggressive.edges$victor=character(R)
  for(i in 1:R){
    if(!is.na(aggressive.edges$Winner[i])){ #for those interacitons a winner column filled in
      result=aggressive.edges$Winner[i]
      focal.win=str_detect(result,"foc") #assuming focal winner is defined as "winner? (foc)" and opposite with "winner? (partnr)"
      focal.lose=str_detect(result,"partnr")
      aggressive.edges$victor[i]=ifelse(focal.win==TRUE,"focal",(
        ifelse(focal.lose==TRUE, "partner","UNK") #adds some redundantcy in case netiher are said to win (for some reason)
      )
      )
    } else {
      if(!is.na(aggressive.edges$Direction[i])){ #those colonums wiht a direction colum filled in
        result=aggressive.edges$Direction[i]
        if(aggressive.edges$`Event Name`[i]=="FearGrm"|aggressive.edges$`Event Name`[i]=="Submit"){
          focal.win=str_detect(result,"receive")
          focal.lose=str_detect(result,"give")
        } else {
          focal.win=str_detect(result,"give")
          focal.lose=str_detect(result,"recieve")
        }
        
        aggressive.edges$victor[i]=ifelse(focal.win==TRUE,"focal",(
          ifelse(focal.lose==TRUE, "partner","UNK") #adds some redundantcy in case netiher are said to win (for some reason)
        )
        )
      } else {aggressive.edges$victor[i]="UNK"} #should be the case if something is wrong
      
    }
  }
  aggressive.edges$victor
  filter(aggressive.edges,victor=="UNK")
  #create agnotsic list section
  names(aggressive.edges)
  aggressive.edges_focalwin=filter(aggressive.edges,victor=="focal")
  aggressive.edges_partnerwin=filter(aggressive.edges,victor=="partner")
  agonistic.1_win=select(aggressive.edges_focalwin,`Focal ID`, `Partner ID`,`Event Name`,`Focal Sex`,`PartnerSex`,full.date,Timeblock,Observer)
  agonistic.1_win=mutate(agonistic.1_win,focal.monkey=rep.int("agonsim.winner",nrow(agonistic.1_win)))
  agonistic.1_lose=select(aggressive.edges_partnerwin,`Partner ID`, `Focal ID`,`Event Name`,`PartnerSex`,`Focal Sex`,full.date,Timeblock,Observer )
  agonistic.1_lose=mutate(agonistic.1_lose,focal.monkey=rep.int("agonsim.loser",nrow(agonistic.1_lose)))
  names(agonistic.1_win)=c("agonsim.winner","agonsim.loser","agonsim.type","winner.sex","loser.sex","date","timeblock","observer","focal.individual")
  names(agonistic.1_lose)=c("agonsim.winner","agonsim.loser","agonsim.type","winner.sex","loser.sex","date","timeblock","observer","focal.individual")
  agonistic.1=rbind(agonistic.1_win,agonistic.1_lose)
  rm(agonistic.1_win)
  rm(agonistic.1_lose)
  agonistic.1=mutate(agonistic.1,data.source=rep.int("focalfollow",nrow(agonistic.1)))
  agonistic.1
  
  
  ##Overtime focal data
  overtime.edges=filter(raw.focaldata,`Event Name`=="contactAgg_overtime"|`Event Name`=="displace_overtime"|`Event Name`=="noncontactAgg_overtime"|`Event Name`=="threat_overtime"|`Event Name`=="avoid_overtime"|`Event Name`=="FearGrm_overtime"|`Event Name`=="Submit_overtime")
  overtime.edges$full.date=as.Date(paste(overtime.edges$Date,overtime.edges$Month,overtime.edges$Year,sep="/"),format="%d/%m/%Y")
  #work out who won
  R=nrow(overtime.edges)
  if(R>0){
    overtime.edges$victor=character(R)
    for(i in 1:R){
      if(!is.na(overtime.edges$Winner[i])){ #for those interacitons a winner column filled in
        result=overtime.edges$Winner[i]
        focal.win=str_detect(result,"foc") #assuming focal winner is defined as "winner? (foc)" and opposite with "winner? (partnr)"
        focal.lose=str_detect(result,"partnr")
        overtime.edges$victor[i]=ifelse(focal.win==TRUE,"focal",(
          ifelse(focal.lose==TRUE, "partner","UNK") #adds some redundantcy in case netiher are said to win (for some reason)
        )
        )
      } else {
        if(!is.na(overtime.edges$Direction[i])){ #those colonums wiht a direction colum filled in
          result=overtime.edges$Direction[i]
          if(overtime.edges$`Event Name`[i]=="FearGrm"|overtime.edges$`Event Name`[i]=="Submit"){
            focal.win=str_detect(result,"receive")
            focal.lose=str_detect(result,"give")
          } else {
            focal.win=str_detect(result,"give")
            focal.lose=str_detect(result,"recieve")
          }
          
          overtime.edges$victor[i]=ifelse(focal.win==TRUE,"focal",(
            ifelse(focal.lose==TRUE, "partner","UNK") #adds some redundantcy in case netiher are said to win (for some reason)
          )
          )
        } else {overtime.edges$victor[i]="UNK"} #should be the case if something is wrong
        
      }
    }
    
  } else {overtime.edges$victor=numeric(0)}
  overtime.edges$victor
  filter(overtime.edges,victor=="UNK")
  #create agnotsic list section
  names(overtime.edges)
  overtime.edges_focalwin=filter(overtime.edges,victor=="focal")
  overtime.edges_partnerwin=filter(overtime.edges,victor=="partner")
  agonistic.2_win=select(overtime.edges_focalwin,`Focal ID`, `Partner ID`,`Event Name`,`Focal Sex`,`PartnerSex`,full.date,Timeblock,Observer)
  agonistic.2_win=mutate(agonistic.2_win,focal.monkey=rep.int("agonsim.winner",nrow(agonistic.2_win)))
  agonistic.2_lose=select(overtime.edges_partnerwin,`Partner ID`, `Focal ID`,`Event Name`,`PartnerSex`,`Focal Sex`,full.date,Timeblock,Observer )
  agonistic.2_lose=mutate(agonistic.2_lose,focal.monkey=rep.int("agonsim.loser",nrow(agonistic.2_lose)))
  names(agonistic.2_win)=c("agonsim.winner","agonsim.loser","agonsim.type","winner.sex","loser.sex","date","timeblock","observer","focal.individual")
  names(agonistic.2_lose)=c("agonsim.winner","agonsim.loser","agonsim.type","winner.sex","loser.sex","date","timeblock","observer","focal.individual")
  agonistic.2=rbind(agonistic.2_win,agonistic.2_lose)
  rm(agonistic.2_win)
  rm(agonistic.2_lose)
  agonistic.2=mutate(agonistic.2,data.source=rep.int("overtime.focalfollow",nrow(agonistic.2)))
  agonistic.2$agonsim.type=str_replace(agonistic.2$agonsim.type,"_overtime","")
  agonistic.2
  
  ##ad lib data
  raw.FFadlib
  agonistic.3_F=select(raw.FFadlib,`winner`,`loser`,`type`,date, Observer)
  names(agonistic.3_F)=c("agonsim.winner","agonsim.loser","agonsim.type","date","observer")
  agonistic.3_F<-
    agonistic.3_F %>%
    mutate(winner.sex=rep.int("Female",nrow(agonistic.3_F))) %>%
    mutate(loser.sex=rep.int("Female",nrow(agonistic.3_F))) %>%
    mutate(focal.individual=rep.int(NA,nrow(agonistic.3_F)))
  raw.MMadlib
  agonistic.3_M=select(raw.MMadlib,`winner`,`loser`,`type`,date, Observer)
  names(agonistic.3_M)=c("agonsim.winner","agonsim.loser","agonsim.type","date","observer")
  agonistic.3_M<-
    agonistic.3_M %>%
    mutate(winner.sex=rep.int("Male",nrow(agonistic.3_M))) %>%
    mutate(loser.sex=rep.int("Male",nrow(agonistic.3_M))) %>%
    mutate(focal.individual=rep.int(NA,nrow(agonistic.3_M)))
  agonistic.3=rbind(agonistic.3_F,agonistic.3_M)
  rm(agonistic.3_F)
  rm(agonistic.3_M)
  agonistic.3=filter(agonistic.3,!is.na(agonsim.type)) #empty rows seem to be introduced
  agonistic.3
  #translate ad lib types to the same as the focal follows
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="D"|agonistic.3$agonsim.type=="d"|agonistic.3$agonsim.type=="DS","displace_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="FG"|agonistic.3$agonsim.type=="fg"|agonistic.3$agonsim.type=="Fg","FearGrm_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="NC"|agonistic.3$agonsim.type=="nc","noncontactAgg_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="CA"|agonistic.3$agonsim.type=="CA ","contactAgg_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="ca","contactAgg_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="C"|agonistic.3$agonsim.type=="c","contactAgg_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="A"|agonistic.3$agonsim.type=="a","avoid_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="TS","Submit/threat_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="ts"|agonistic.3$agonsim.type=="T/S","Submit/threat_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="AV","avoid_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="NCA","noncontactAgg_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="T","threat_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="th","threat_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="t","threat_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="S","submit_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=ifelse(agonistic.3$agonsim.type=="Lean","submit_1",agonistic.3$agonsim.type)
  unique(agonistic.3$agonsim.type)# NEED TO CHECK. Seems to be quite a varitey of differnt codes used. Should all be words preferably matching focal data
  agonistic.3$agonsim.type=ifelse(str_detect(agonistic.3$agonsim.type,"_1")==FALSE,"ambigiously.recorded_1",agonistic.3$agonsim.type)
  agonistic.3$agonsim.type=str_replace(agonistic.3$agonsim.type,"_1","")
  #add final columns
  agonistic.3=mutate(agonistic.3,data.source=rep.int("adlib.observation",nrow(agonistic.3)))
  agonistic.3$timeblock = NA
  
  ##combine all agonisitic interactions into a single action
  agonistic.actions=rbind(agonistic.1,agonistic.2,agonistic.3)
  agonistic.actions$observer=toupper(agonistic.actions$observer)
  agonistic.actions
  
  #ONLY FOR KK2017 - quick and dirty fix for now. 
  if (groupyears[gy]=="KK2017"){
    data.winner = agonistic.actions$agonsim.winner[which(agonistic.actions$focal.individual =="agonsim.winner")]
    prev.data.winner = as.character(prev.data$agonsim.winner[which(prev.data$focal.individual =="agonsim.winner")])
    all_equal(data.winner, prev.data.winner)
    
    data.loser = agonistic.actions$agonsim.loser[which(agonistic.actions$focal.individual =="agonsim.loser")]
    prev.data.loser = as.character(prev.data$agonsim.loser[which(prev.data$focal.individual =="agonsim.loser")])
    #Eliminate one observation that is not in sync with current file
    A=cbind(data.loser, prev.data.loser[1:380])
    prev.data=prev.data[-412,]
    #Reecomput data loser column and check match
    prev.data.loser = as.character(prev.data$agonsim.loser[which(prev.data$focal.individual =="agonsim.loser")])
    all_equal(data.loser, prev.data.loser)
    
    prev.data$timeblock = agonistic.actions$timeblock
    agonistic.actions =prev.data
    sanity.check=nrow(prev.data)
  }
  
  #Sanity check that there is the same number of observations
  if (sanity.check != nrow(agonistic.actions)){stop()}
  
  setwd(output.location)
  write_delim(agonistic.actions,paste(data.details,"_AgonsiticActions.txt",sep=""),delim=",") #written as txt to preserve id's with middle 'e' which csv/excel convert to a number
  
}