
data <- read.csv("stormdata.csv.bz2", stringsAsFactors = F)
names(data) <- tolower(names(data))
data <- select(data, evtype, fatalities:cropdmgexp)

data <- select(data, evtype, fatalities:cropdmgexp)
data <- mutate(data, cropdmgexp = tolower(cropdmgexp))
data <- mutate(data, evtype = tolower(evtype))


data1 %>% select(evtype) %>% group_by(evtype) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>% print(n = 20)
temp <- table(data1$evtype)
temp <- as.data.frame(temp)
temp <- arrange(temp, desc(Freq))
sum(temp$Freq)
sum(temp$Freq[1:10])
data1$evtype <- gsub("thunderstorm winds", "thunderstorm wind", data1$evtype)
data1$evtype <- gsub("tstm wind", "thunderstorm wind", data1$evtype)

data1 %>% select(propdmgexp) %>% group_by(propdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))
data1$propdmgexp <- data1$propdmgexp %>% gsub("k", "3", .) %>% gsub("m", "6", .) %>% gsub("b", "9", .) %>% gsub("\\?", "0", .) %>% gsub("h", "0", .) %>% gsub("\\+", "0", .) %>% gsub("\\-", "0", .)
data1$propdmgexp <- as.numeric(data1$propdmgexp)
data1$propdmgexp[is.na(data1$propdmgexp)] <- 0

data1 %>% select(cropdmgexp) %>% group_by(cropdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))
data1$cropdmgexp <- data1$cropdmgexp %>% gsub("k", "3", .) %>% gsub("m", "6", .) %>% gsub("b", "9", .) %>% gsub("\\?", "0", .)
data1$cropdmgexp <- as.numeric(data1$cropdmgexp)
data1$cropdmgexp[is.na(data1$cropdmgexp)] <- 0

data2 <- mutate(data1, dmg = (propdmg * 10^propdmgexp) + (cropdmg * 10^cropdmgexp), harm = fatalities + injuries )
data2 <- group_by(data2, evtype)

data2 %>% summarise(sumdmg = sum(dmg)) %>% arrange(desc(sumdmg)) %T>% print(n = 15) %>% slice(1:10) %>% qplot(data = ., y = sumdmg, x = evtype, geom = "bar", stat = "identity")
