library(dplyr)
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',
              'stormdata.csv')
stormData <- read.csv('stormdata.csv')
head(stormData)
dim(stormData)

file.size('stormdata.csv')
object.size(stormData)

#======================================================

data <- stormData

# remove data with ALL FATALITIES/INJURIES/CROPDMG/PROPDMG == 0 
# result in 201318 rows from 902297 from

noDmgLogical <- (data$FATALITIES == 0)&
  (data$INJURIES == 0)&
  (data$PROPDMG == 0)&
  (data$CROPDMG == 0)


data <- subset(data, !(noDmgLogical))
dim(data)

## Exponent column handling function

expFunc <- function(x){
  if ((x=='B') | (x=='b')) {x <- 1e+9
  }else if ((x=='M') | (x=='m')){x <- 1e+6
  }else if((x=='K') | (x=='k')){x <- 1e+3
  }else if((x=='H') | (x=='h')){x <- 1e+2
  }else if(x %in% as.character(0:8)){x <- 1e+1
  }else{x <- 1}
}

# Change alphabets to exponents
# for CROPDMGEXP
data$CROPDMGEXP <- sapply(data$CROPDMGEXP,expFunc)
table(data$CROPDMGEXP)

# Make a new column for actual damage
data <- mutate(data, totCropDmg=CROPDMG * CROPDMGEXP)

unique(data$totCropDmg)
tail(data[,c("CROPDMG",'CROPDMGEXP','totCropDmg')])

# for PROPDMGEXP
unique(data$PROPDMGEXP)
data$PROPDMGEXP <- sapply(data$PROPDMGEXP, expFunc)
data <- mutate(data, totPropDmg=PROPDMG*PROPDMGEXP)

# EVTYPE has 488 unique values
length(unique(data$EVTYPE))

# change all to upper case then 447 EVTYPE

data$EVTYPE <- toupper(data$EVTYPE)
length(unique(data$EVTYPE))


# Change BGN_DATE column to Date class

library(lubridate)
date <- gsub('/','-',data$BGN_DATE)
date <- gsub(' 0:00:00','',date)
date <- mdy(date)
data$BGN_DATE <- date



#========================================================================
# calculate total damage before proceed to do clean up

healthDmgSum <- data %>% 
  group_by(EVTYPE) %>%
  summarize(fatalitiesSum=sum(FATALITIES),injuriesSum=sum(INJURIES)) %>%
  mutate(total=fatalitiesSum+injuriesSum) %>%
  arrange(desc(total))

healthDmgSum


economicDmgSum <- data %>%
  group_by(EVTYPE) %>%
  summarize(cropSum=sum(totCropDmg),propSum=sum(totPropDmg)) %>%
  mutate(total=cropSum+propSum) %>%
  arrange(desc(total))

economicDmgSum




#=====================================================================
# Remove unnecessary columns
data <- select(data,c('STATE__',
                            "STATE",
                            "BGN_DATE",
                            'EVTYPE',
                            "LENGTH",
                            "WIDTH",
                            "F",
                            "MAG",
                            "FATALITIES",
                            "INJURIES",
                            "PROPDMG",
                            "PROPDMGEXP",
                            "totPropDmg",
                            "CROPDMG",
                            "CROPDMGEXP",
                            "totCropDmg",
                            "REMARKS"
                            ))

dim(data)

# get full list of official event types
events <- read.table('events.txt',sep = '\n')

# Official event types are 48
events <- toupper(events$V1)
events

# 401 events in data are unlisted  
notInListEvents <- unique(data[!(data$EVTYPE %in% events),'EVTYPE'])


# Check how much those data affect on total damage

healthDmgForUnlisted <- healthDmgSum %>%
  subset(EVTYPE %in% notInListEvents) %>%
         arrange(desc(total))

economicDmgForUnlisted <- economicDmgSum %>%
  subset(EVTYPE %in% notInListEvents) %>%
  arrange(desc(total))

# Remove unlisted events data less than 10 fatality/injury AND 1000$ damage
NotBigDmgListHealth <- subset(healthDmgForUnlisted,total < 10)$EVTYPE
NotBigDmgListEconomy <- subset(economicDmgForUnlisted,total < 1000)$EVTYPE

removeEvents <- intersect(NotBigDmgListHealth,NotBigDmgListEconomy)

data <- subset(data,!(EVTYPE %in% removeEvents))

# Unlisted events down to 347 from 401....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^\\s*TSTM WIND",data$EVTYPE)] <- "THUNDERSTORM WIND"

# Unlisted events down to 329 from 347....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^HURRICANE",data$EVTYPE)] <- "HURRICANE (TYPHOON)"  

# Unlisted events down to 321 from 329....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^THUNDERSTORM",data$EVTYPE)] <- "THUNDERSTORM WIND"

# Unlisted events down to 284 from 321....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FLASH FLOOD",data$EVTYPE)] <- "FLASH FLOOD"

# Unlisted events down to 271 from 284....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("TORNADO",data$EVTYPE)] <- "TORNADO"

# Unlisted events down to 261 from 271....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^HIGH WIND",data$EVTYPE)] <- "HIGH WIND" 

# Unlisted events down to 247 from 261....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^WIND",data$EVTYPE)] <- "HIGH WIND"

# Unlisted events down to 242 from 247....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^HEAVY RAIN",data$EVTYPE)] <- "HEAVY RAIN"

# Unlisted events down to 234 from 242....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])
data$EVTYPE[grepl("LIGHTNING",data$EVTYPE)] <- "LIGHTNING"

# Unlisted events down to 229 from 234....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("COLD",data$EVTYPE)&
                     !grepl("EXTREME",data$EVTYPE)&
                     !grepl('SNOW',data$EVTYPE)] <- "COLD/WIND CHILL"

# Unlisted events down to 223 from 229....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^FLOOD",data$EVTYPE)&
         !grepl("FLASH",data$EVTYPE)] <- "FLOOD"

# Unlisted events down to 218 from 223....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("EXTREME COLD",data$EVTYPE)] <- "EXTREME COLD/WIND CHILL"

# Unlisted events down to 217 from 218....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FREEZE",data$EVTYPE)] <- "FROST/FREEZE"

# Unlisted events down to 212 from 217....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FLOOD",data$EVTYPE)&
                   !grepl("FLASH",data$EVTYPE)&
                     !grepl("COSTAL",data$EVTYPE)&
                     !grepl("LAKESHORE",data$EVTYPE)&
                     !grepl("COASTAL",data$EVTYPE)&
                     !grepl("LAKE",data$EVTYPE)] <-"FLOOD"

# Unlisted events down to 194 from 212....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("MARINE MISHAP",data$EVTYPE)] <- "MARINE HIGH WIND"

# Unlisted events down to 193 from 194....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^HIGH",data$EVTYPE)&
                     !(grepl('WIND',data$EVTYPE))] <- "HIGH SURF"

# Unlisted events down to 189 from 193....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("SEVERE TURBULENCE",data$EVTYPE)] <- "THUNDERSTORM WIND"

# Unlisted events down to 188 from 189....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("SNOW",data$EVTYPE)&
                     !(grepl('LAKE',data$EVTYPE))] <- "HEAVY SNOW" 

# Unlisted events down to 148 from 188....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("APACHE COUNTY",data$EVTYPE)] <- "THUNDERSTORM WIND"

# Unlisted events down to 147 from 148....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[(grepl("TUNDERSTORM",data$EVTYPE)|
                     grepl("THUDERSTORM",data$EVTYPE)|
                     grepl("THUNDERSTORM",data$EVTYPE)|
                     grepl("THUNDEERSTORM",data$EVTYPE)|
                     grepl("THUNDERTORM",data$EVTYPE)|
                     grepl("THUNDERESTORM",data$EVTYPE)|
                     grepl("THUNERSTORM",data$EVTYPE))&
                     !grepl("MARINE THUNDERSTORM WIND",data$EVTYPE)] <- "THUNDERSTORM WIND"

# Unlisted events down to 138 from 147....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("TYPHOON",data$EVTYPE)] <- "HURRICANE (TYPHOON)"

# Unlisted events down to 137 from 138....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("GUSTY",data$EVTYPE)] <- "STRONG WIND"

# Unlisted events down to 132 from 137....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("LAKE.+SNOW",data$EVTYPE)] <- "LAKE-EFFECT SNOW"

# Unlisted events down to 130 from 132....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("HAIL",data$EVTYPE)&
                     !grepl('MARINE',data$EVTYPE)] <- "HAIL" 

# Unlisted events down to 116 from 130....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("SLIDE",data$EVTYPE)] <- "DEBRIS FLOW"

# Unlisted events down to 109 from 116....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("TROPICAL",data$EVTYPE)&
                     !grepl('DEPRESSION',data$EVTYPE)] <- "TROPICAL STORM"

# Unlisted events down to 105 from 109....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("RAIN",data$EVTYPE)] <- "HEAVY RAIN"

# Unlisted events down to 97 from 105....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FOG",data$EVTYPE)&
              !grepl('FREEZING',data$EVTYPE)] <- "DENSE FOG"

# Unlisted events down to 96 from 97....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("COASTAL FLOODING",data$EVTYPE)|
                     grepl('COASTAL FLOODING/EROSION',data$EVTYPE)] <- "COASTAL FLOOD"

# Unlisted events down to 93 from 96....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("HIGH.+WINDS$",data$EVTYPE)] <- "HIGH WIND"

# Unlisted events down to 90 from 93....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FIRE",data$EVTYPE)] <- "WILDFIRE"

# Unlisted events down to 83 from 90....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("ICE",data$EVTYPE)&
                     !grepl("STORM",data$EVTYPE)] <- "FROST/FREEZE"

# Unlisted events down to 76 from 83....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FROST",data$EVTYPE)|
                      grepl("GLAZE",data$EVTYPE)|
                      grepl("ICY ROADS",data$EVTYPE)|
                      grepl("EARLY FROST",data$EVTYPE)] <- "FROST/FREEZE"

# Unlisted events down to 72 from 76....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("DROUGHT",data$EVTYPE)] <- "DROUGHT"

# Unlisted events down to 69 from 72....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("HEAT",data$EVTYPE)&
                     grepl('EX',data$EVTYPE)] <- "EXCESSIVE HEAT"

# Unlisted events down to 67 from 69....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("CHILL",data$EVTYPE)&
                     grepl('EX',data$EVTYPE)] <- "EXTREME COLD/WIND CHILL"

# Unlisted events down to 65 from 67....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("FL.*D",data$EVTYPE)&
                     grepl('FLASH',data$EVTYPE)] <- "FLASH FLOOD"

# Unlisted events down to 61 from 65....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("SURF",data$EVTYPE)] <- "HIGH SURF"



data$EVTYPE[grepl("WINTER",data$EVTYPE)&
                     !grepl('STORM',data$EVTYPE)] <- "WINTER WEATHER"

# Unlisted events down to 55 from 57....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("BLIZZARD",data$EVTYPE)] <- "BLIZZARD"

# Unlisted events down to 53 from 55....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[(grepl("WIN[^DG].+",data$EVTYPE)|
                      grepl('COOL',data$EVTYPE)|
                      grepl('FREEZING\\s[^F]',data$EVTYPE))&
                     !grepl('STORM',data$EVTYPE)] <- "WINTER WEATHER"

# Unlisted events down to 50 from 53....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("HEAVY\\s[PM]",data$EVTYPE)|
                     grepl("EXCESSIVE WETNESS",data$EVTYPE)|
                     grepl("PRECIP",data$EVTYPE)] <- "HEAVY RAIN"


# Unlisted events down to 45 from 50....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("^[^MHST].+WIN[DS]$",data$EVTYPE)|
                     grepl('FORCE',data$EVTYPE)] <- "STRONG WIND"

# Unlisted events down to 42 from 45....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("URBAN",data$EVTYPE)] <- "FLOOD"

# Unlisted events down to 38 from 42....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("MICROBURST",data$EVTYPE)] <- "STRONG WIND"

# Unlisted events down to 34 from 38....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])
data$EVTYPE[grepl("COASTAL.+[N]$",data$EVTYPE)] <- "COASTAL FLOOD"

# Unlisted events down to 32 from 34....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("HEAT WAVE",data$EVTYPE)|
                     grepl("RECORD HEAT",data$EVTYPE)|
                     grepl("WARM",data$EVTYPE)] <- "HEAT"

# Unlisted events down to 28 from 32....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("WINTER STORMS",data$EVTYPE)] <- "WINTER STORM" 
data$EVTYPE[grepl("STRONG WINDS",data$EVTYPE)] <- "STRONG WIND"
data$EVTYPE[grepl("RIP CURRENTS",data$EVTYPE)] <- "RIP CURRENT"
data$EVTYPE[grepl("WATERSPOUT-",data$EVTYPE)] <- "WATERSPOUT"


# Unlisted events down to 24 from 28....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("TSTMW",data$EVTYPE)|
                     grepl("THUNDERSTROM WIND",data$EVTYPE)] <- "THUNDERSTORM WIND"


# Unlisted events down to 22 from 24....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("STORM SURGE",data$EVTYPE)|
              grepl("COASTAL SURGE",data$EVTYPE)] <- "STORM SURGE/TIDE"

# Unlisted events down to 20 from 22....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("LIGNTNING",data$EVTYPE)|
              grepl("LIGHTING",data$EVTYPE)] <- "LIGHTNING"

# Unlisted events down to 18 from 20....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("HEAVY SWELLS",data$EVTYPE)|
                     grepl("ROUGH SEAS",data$EVTYPE)] <- "HIGH SURF"

# Unlisted events down to 16 from 18....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("NON-SEVERE WIND DAMAGE",data$EVTYPE)|
                     grepl("BLOWING DUST",data$EVTYPE)] <- "STRONG WIND" 
data$EVTYPE[grepl("MARINE TSTM WIND",data$EVTYPE)] <- "MARINE THUNDERSTORM WIND"

# Unlisted events down to 13 from 16....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("LANDSPOUT",data$EVTYPE)] <- "TORNADO"
data$EVTYPE[grepl("DOWNBURST",data$EVTYPE)] <- "STRONG WIND"
data$EVTYPE[grepl("COASTAL STORM",data$EVTYPE)] <- "MARINE THUNDERSTORM WIND"
data$EVTYPE[grepl("ASTRONOMICAL HIGH TIDE",data$EVTYPE)] <- "ASTRONOMICAL LOW TIDE"
data$EVTYPE[grepl("BEACH EROSION",data$EVTYPE)] <- "DEBRIS FLOW"

# Unlisted events down to 8 from 13....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("SLEET/ICE STORM",data$EVTYPE)] <- "SLEET"
data$EVTYPE[grepl("LAKE FLOOD",data$EVTYPE)] <- "LAKESHORE FLOOD"
data$EVTYPE[grepl("LANDSLUMP",data$EVTYPE)] <- "DEBRIS FLOW"

# Unlisted events down to 5 from 8....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("MARINE ACCIDENT",data$EVTYPE)] <- "HIGH SURF"
data$EVTYPE[grepl("DAM BREAK",data$EVTYPE)] <- "HEAVY RAIN"

# Unlisted events down to 3 from 5....
unique(data$EVTYPE[!(data$EVTYPE %in% events)])

data$EVTYPE[grepl("GUSTNADO",data$EVTYPE)] <- "THUNDERSTORM WIND"

# remove '?' EVTYPE since no information for weather

dim(data[data$EVTYPE=="OTHER",''])

#===========================
events[grepl('WIND',events)] 


stormData[stormData$EVTYPE=="?",]
rownames(data[data$EVTYPE=="?",])

