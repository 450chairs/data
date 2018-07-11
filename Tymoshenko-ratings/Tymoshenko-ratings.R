# R-studio script to replicate the following blog:
# https://450chairs.com/2018/06/25/пальцем-в-рейтинг/

# http://ukraine-elections.com.ua is a catalog of all electoral polls in Ukraine
# We gathered data from this website and created a file "ukraine-elections.csv"

# Some analyses and visuals for Tymoshenko's presidential polls are produced

# For more information contact us at the450chairs@gmail.com
#################################################################################

require("zoo")
require("plotly")
require("ggplot2")

# Open the data

data <- read.csv("ukraine-elections.csv")
head(data)

# Check the variables

str(data)
dim(data)
names(data)

# Check the type (method) of a survey

table(data$method)

# Selecting only face to face surveys

data <- data[grepl('личное интервью', data$method), ]
dim(data)

# Variables as dates

data$start_date <- as.Date(substring(data$period, 1, 10), "%d.%m.%Y")
data$end_date <- as.Date(substring(data$period, 14, 23), "%d.%m.%Y")
data$n_days <- as.numeric(data$end_date - data$start_date)

summary(data$n_days)
hist(data$n_days)

data$period[data$n_days < 0]
data$link[data$n_days < 0]

# Cleaning the data #1
data$period <- as.character(data$period)
data$period[data$period == "04.07.2015 - 14.05.2015"] <- "04.07.2015 - 14.07.2015"
data$period[data$period == "20.07.2017 - 29.07.2014"] <- "20.07.2017 - 29.07.2017"
data$period[data$period == "26.03.2015 - 11.03.2015"] <- "26.02.2015 - 11.03.2015"

# Repeat with new dates
data$start_date <- as.Date(substring(data$period, 1, 10), "%d.%m.%Y")
data$end_date <- as.Date(substring(data$period, 14, 23), "%d.%m.%Y")
data$n_days <- as.numeric(data$end_date - data$start_date)

summary(data$n_days)
hist(data$n_days)

# Select only Tymoshenko
colnames(data)[grep("Тимошенко", colnames(data))]

data_t <- data[, c("link", "company", "votes.Тимошенко", "start_date", "end_date", "n_days", "sample")]
names(data_t)[grepl("Тимошенко", names(data_t))] <- "tymoshenko"
data_t <- data_t[!is.na(data_t$tymoshenko), ]
dim(data_t) # 134 surveys

# Recode names
sort(table(data_t$company))

data_t$company <- as.character(data_t$company)
data_t$company_recoded <- data_t$company

# Dealing with some unpleasant characters
data_t$company_recoded <- gsub("(\"|”|')", "", data_t$company_recoded, perl = T)

# Detect companies

list_of_companies = list(
  "Рейтинг" = "Рейтинг",
  "КМІС" = "(?i)кмис",
  "SOCIS" = "(?i)социс|socis",
  "Разумков" = "(?i)разумков",
  "Софія" = "(?i)софия",
  "RB" = "(?i)branding|gfk",
  "Інститут Майбутнього" = "(?i)будущего",
  "Демократичне коло" = "(?i)коло",
  "Институт Горшенина" = "(?i)горшен",
  "QQ" = "(?i)Q&Q",
  "ЦСП" = "(?i)социальная персп",
  "ППІ" = "(?i)передовые",
  "AG" = "(?i)act.ve",
  "УІСД" = "УИСИ|Яр.менк",
  "ИАП" = "ИАП",
  "СМ" = "(?i)альный монит",
  "Z&B" = "Z&B",
  "ФДІ" = "(?i)(фонд дем|кратические инициатив)",
  "ІАП" = "ИАП",
  "П" = "АПК",
  "ІК" = "(?i)имидж"
)

get_companies = function(s) {
  res = c()
  for (i in names(list_of_companies)) {
    if (grepl(list_of_companies[i][[1]], s, perl = T)) {
      res = c(res, i)
    }
  }
  if (is.null(res)) {
    res = "Інші"
  }
  res = paste(res, collapse = ", ")
  return(res)
}

data_t$company_recoded <- sapply(data_t$company_recoded, get_companies)

# Select surveys one behalf of another agency
data_t$behalf <- 0
data_t$behalf[grep("заказу|поддерже", data_t$company, perl = T)] <- 1
table(data_t$behalf)
data_t[data_t$behalf == 1,]

# Select if collaboration
data_t$collaboration <- 0
data_t$collaboration[grep(", ", data_t$company_recoded)] <- 1
table(data_t$collaboration)
data_t[data_t$collaboration == 1,]

# Delete Internet polls, regional polls
data_t <- data_t[data_t$company != "Центр Разумкова, УИСИ и Социальный мониторинг по заказу ИГЛС", ]
data_t <- data_t[data_t$company != "КМИС (опрос по юго-востоку Украины)", ]
data_t <- data_t[data_t$company != "Consulting Agency F5 по заказу НикВести", ]
data_t <- data_t[data_t$company != "Объединенный пул социологических служб Харьковской обл \"Слобожанский рейтинг\"", ]
data_t <- data_t[data_t$company != "Українське демократичне коло (опрос по Киеву)", ]

hist(data_t$tymoshenko)
summary(data_t$tymoshenko)

# Check outliers
data_t$link[data_t$tymoshenko == 2.4]
data_t <- data_t[data_t$tymoshenko != 2.4, ]

hist(data_t$tymoshenko)
summary(data_t$tymoshenko)

data_t$link[data_t$tymoshenko == 3.7]
data_t$company[data_t$tymoshenko == 3.7] #seems legit

# Another strategy for outliers
summary(data_t$tymoshenko); sd(data_t$tymoshenko)
data_t$link[data_t$tymoshenko < 7]
data_t$company[data_t$tymoshenko < 7]

data_t <- data_t[data_t$link != "https://news.pn/ru/public/192870", ] #mykolaiv
data_t <- data_t[data_t$link != "http://new.activegroup.com.ua/?p=2950", ] #cities
data_t <- data_t[data_t$link != "ttp://www.kiis.com.ua/?lang=ukr&cat=reports&id=541&page=1", ] #mykolaiv
data_t <- data_t[data_t$link != "https://www.slideshare.net/SocioStream/sociostream-june-2016-63640491", ] #cities
data_t <- data_t[data_t$link != "http://www.slideshare.net/SocioStream/sociostream-june-2016-63640491", ] #cities

hist(data_t$tymoshenko)
summary(data_t$tymoshenko); sd(data_t$tymoshenko)

data_t$link[data_t$tymoshenko > 13]
data_t$company[data_t$tymoshenko > 13]

data_t <- data_t[data_t$link != "https://znaj.ua/ru/capital/kyevlyane-nazvaly-budushego-prezydenta-y-partyyu-lydera", ] #kyiv
data_t <- data_t[data_t$link != "http://www.rbc.ua/rus/news/society/menee-poloviny-ukraintsev-schitayut-chto-vybory-v-radu-izmenyat-05092014112500", ] #IDPs

hist(data_t$tymoshenko)
summary(data_t$tymoshenko); sd(data_t$tymoshenko)

# Another strategy for outliers

summary(data_t$n_days)
table(data_t$n_days)

data_t$link[data_t$n_days <= 4]
data_t$n_days[data_t$n_days <= 3] <- NA

dim(data_t) #N = 118 observations
plot(data_t$n_days, data_t$tymoshenko)
cor(data_t$n_days, data_t$tymoshenko, use = "complete.obs")

# New variables for years, months, quarters

data_t$year <- format(data_t$start_date,"%Y")
data_t$month <- format(data_t$start_date,"%m")
data_t$yerm <- format(data_t$start_date,"%Y%m")
data_t$qtr <- as.yearqtr(data_t$yerm , "%Y%m")

#Check stats per periods
data_t$id <- 1

tapply(data_t$tymoshenko, data_t$year, mean)
tapply(data_t$tymoshenko, data_t$year, sd)
tapply(data_t$id, data_t$year, sum)

tapply(data_t$tymoshenko, data_t$qtr, mean)
tapply(data_t$tymoshenko, data_t$qtr, sd)
tapply(data_t$id, data_t$qtr, sum)

data_t <- data_t[data_t$year != "2011", ] #only 4 surveys in 2011

data_t$qtr_cat <- NA
data_t$qtr_cat[grep("Q1", data_t$qtr)] <- "Q1"
data_t$qtr_cat[grep("Q2", data_t$qtr)] <- "Q2"
data_t$qtr_cat[grep("Q3", data_t$qtr)] <- "Q3"
data_t$qtr_cat[grep("Q4", data_t$qtr)] <- "Q4"
table(data_t$qtr_cat)

#Check some plots

plot_ly(data_t, y= ~tymoshenko, type = "box", x =~year ) %>%
  layout(xaxis = list(title = "Рік"), yaxis = list(title = "Рейтинг"))

plot_ly(data_t, y= ~tymoshenko, type = "box", x =~qtr ) %>%
  layout(xaxis = list(title = "Рік"), yaxis = list(title = "Рейтинг"))

data_t$sd.y <- as.vector(tapply(data_t$tymoshenko, data_t$year, sd)[data_t$year])
data_t$mean.y <- as.vector(tapply(data_t$tymoshenko, data_t$year, mean)[data_t$year])

# Difference between Tymoshenko and Mean(Tymoshenko)

data_t$diff_1 <- data_t$tymoshenko - data_t$mean

# New variable using cyrillic for deviations

data_t$trend <- "тренд"
data_t$trend[abs(data_t$diff_1) > data_t$sd] <- "одне відхилення"
data_t$trend[abs(data_t$diff_1) > 2*(data_t$sd)] <- "два відхилення"

# Plotly plots

pal <- c("#DD2C00", "#388E3C", "#1565C0")
plot_ly(type = 'scatter', mode = 'markers', data = data_t,
        x = ~as.character(qtr),
        y = ~tymoshenko,
        color = ~as.factor(trend),
        symbol = ~collaboration,
        size = ~n_days,
        symbols = c('circle','x'),
        text = ~paste("Рейтинг: ", tymoshenko, '<br>Компанія:', company_recoded),
        colors = pal
        ) %>%
  layout(legend = list(orientation = 'h'),
         xaxis = list(title = "Рік та квартал"),
         yaxis = list(title = "Президентський рейтинг Тимошенко"))
sort(data_t$sample)

plot_ly(type = 'scatter', mode = 'markers', data = data_t[data_t$sample <= 6200,],
        x = ~as.character(qtr),
        y = ~tymoshenko,
        color = ~as.factor(trend),
        symbol = ~collaboration,
        size = ~sample,
        symbols = c('circle','x'),
        text = ~paste("Рейтинг: ", tymoshenko, '<br>Компанія:', company_recoded),
        colors = pal
) %>%
  layout(legend = list(orientation = 'h'),
         xaxis = list(title = "Рік та квартал"),
         yaxis = list(title = "Президентський рейтинг Тимошенко"))

## Ggplot plots

# Plots

BlANK.FORMAT = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))

p1 <- ggplot(data_t, aes(x = as.character(qtr), y = tymoshenko,
                           label = company_recoded)) +
  geom_point() + BlANK.FORMAT +
  geom_label(aes(fill = factor(trend)), colour = "white", size = 3)+
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Квартал", y = "Президентський рейтинг Тимошенко")

#Plot 2
p2 <- ggplot(data_t, aes(x = as.character(qtr), y = tymoshenko,
                           label = company_recoded)) +
  BlANK.FORMAT +
  geom_text(check_overlap = TRUE, size = 3, aes(colour = factor(trend))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "top") +
  labs(x = "Квартал", y = "Президентський рейтинг Тимошенко",
       color = "Відхилення від річного середнього")

#Plot 3
p3 <- ggplot(data_t, aes(x=as.character(qtr), y=tymoshenko, color = factor(trend))) +
  BlANK.FORMAT +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "top") +
  labs(x = "Квартал", y = "Президентський рейтинг Тимошенко",
       color = "Відхилення від річного середнього")
p1
p2
p3


jpeg("PlotTymoshenko.jpeg", width = 1000)
plot(p2)
dev.off()
