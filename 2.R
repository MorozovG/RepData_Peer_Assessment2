library(magrittr)

data <- read.csv("stormdata.csv.bz2", stringsAsFactors = F)
str(data)
names(data) <- tolower(names(data))
data %<>% select(evtype, fatalities:cropdmgexp) %>% 
        mutate(cropdmgexp = tolower(cropdmgexp)) %>%
        mutate(propdmgexp = tolower(propdmgexp)) %>% mutate(evtype = tolower(evtype))
str(data)

data1 %>% select(evtype) %>% group_by(evtype) %>% summarise(freq = n()) %>%
        arrange(desc(freq)) %T>% print(n = 20) %>%
        with(cat("Fraction first ten event type:", round(sum(freq[1:10])/sum(freq), 2)*100, "%"))
data1$evtype %<>% gsub("thunderstorm winds", "thunderstorm wind", .) %>%
        gsub("tstm wind", "thunderstorm wind", .)
data1 %>% select(evtype) %>% group_by(evtype) %>% summarise(freq = n()) %>%
        arrange(desc(freq)) %T>% print(n = 20) %>%
        with(cat("Fraction first ten event type:", round(sum(freq[1:10])/sum(freq), 2)*100, "%"))

data1 %>% select(propdmgexp) %>% group_by(propdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))
data1$propdmgexp %<>% gsub("k", "3", .) %>% gsub("m", "6", .) %>% gsub("b", "9", .) %>%
        gsub("\\?", "1", .) %>% gsub("h", "1", .) %>% gsub("\\+", "1", .) %>% gsub("\\-", "1", .)
data1 %>% select(propdmgexp) %>% group_by(propdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))
data1$propdmgexp <- as.numeric(data1$propdmgexp)
data1$propdmgexp[is.na(data1$propdmgexp)] <- 0
data1 %>% select(propdmgexp) %>% group_by(propdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))

data1 %>% select(cropdmgexp) %>% group_by(cropdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))
data1$cropdmgexp %<>% gsub("k", "3", .) %>% gsub("m", "6", .) %>% gsub("b", "9", .) %>% gsub("\\?", "1", .)
data1$cropdmgexp <- as.numeric(data1$cropdmgexp)
data1$cropdmgexp[is.na(data1$cropdmgexp)] <- 0
data1 %>% select(cropdmgexp) %>% group_by(cropdmgexp) %>% summarise(freq = n()) %>% arrange(desc(freq))

data1 %<>% mutate(dmg = (propdmg * 10^propdmgexp) + (cropdmg * 10^cropdmgexp), harm = fatalities + injuries )
data1 %>%group_by(evtype) %>% summarise(sumdmg = sum(dmg)) %>% arrange(desc(sumdmg))
data1$evtype %<>% gsub("hurricane", "hurricane/typhoon", .) %>%
        gsub("river flood", "flood", .) %>% gsub("/typhoon", "", .)




data1 %>%group_by(evtype) %>% summarise(sumfat = sum(fatalities)) %>% arrange(desc(sumfat)) %T>% 
        print(n = 10) %>% slice(1:10) %>%
        qplot(data = ., y = sumfat, x = evtype, geom = "bar", fill = I("steelblue"), stat = "identity")


data1 %>%group_by(evtype) %>% summarise(suminj = sum(injuries)) %>% arrange(desc(suminj)) %T>% 
        print(n = 10) %>% slice(1:10) %>%
        qplot(data = ., y = suminj, x = evtype, geom = "bar", fill = I("steelblue"), stat = "identity")

data1 %>%group_by(evtype) %>% summarise(sumdmg = sum(dmg)) %>% arrange(desc(sumdmg)) %T>% 
        print(n = 10) %>% slice(1:10) %>%
        qplot(data = ., y = sumdmg, x = evtype, geom = "bar", fill = I("steelblue"), stat = "identity")
