library(Rserve)
library(purrr)
library(RODBC)
library("DBI")
library(odbc)
library(RJDBC)
library(glue)
library(prophet)
library(shiny)
library(forecast)
library(ggplot2)
library(ggfortify)
library(ggplot2)
library(highcharter)
library(zoo)
library(tseries)
#library(data.world)
library(rtweet)
library(quantmod)
library(RMySQL)
library(shiny)
library(RJSONIO)
library(crypto)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(stringr)
library(tidyverse)
library(tidyquant)
library(timeDate)
library(timetk)
library(DT)
library(e1071)
library(broom)
library(lattice)
library(r2d3)
library(caret)
library(class)
library(shiny.i18n)
library(shinythemes)
library(nycflights13)
library(gapminder)
library(corrplot)
library(gridExtra)
library(randomForest)
library(readr)
# Verbindung mit HANA erstellen
Aineah <-odbcConnect("BWT",uid="#######",pwd="########" )
# Daten hochladen 
STM_RAND_ORG <- read_delim("C:/Users/###############", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>% data.frame()
# Erste Spalte selektieren und Stichtag in richtige Datumformat konvertieren
df <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) # parse date column using lubridate ymd_hms function
  ) %>%  arrange(date) %>% 
  select(Arb_Am_Gulp.Stichtag)

# Funktionale Programmierung(Funktionsname = Prognose) mit ‹bergabe Parameter Dataframe = df, Startsdatum = datum, Wochen im Vorau = h
# Wie sollte die neue PrognoseSpalte heiﬂen = name

prognose <- function(df, datum, h,name) {
  # Zeitreihe Prognose erstellen
  edf_r <- ts((df), freq=365.25/7, 
              start=decimal_date(ymd(datum)))
  # Transformation
  lambda <- BoxCox.lambda(edf_r)
  df1 <- data.frame(edf_r)
  one <- forecast(tbats(edf_r, biasadj = TRUE), h = h, lambda=lambda)
  f <- one["mean"] %>% data.frame()
  one["mean"] %>% data.frame() 
  
  df2 <- one["mean"] %>% data.frame()  %>%
    data.frame() %>%
    round(2) %>%
    mutate("Datum" = seq.Date(
      as.Date(datum),
      by = "week",
      length.out = nrow(f)
    ))# %>%
  # rename("Forecast"=.)
  #as.character.Date()
  names(df2)[1] <- name
  f <- df2[1,1]
  final <- data.frame(rbind(df1,f))
  prog <- final %>% round() %>% mutate("Stichtag" = seq.Date(
    as.Date(datum),
    by = "week",
    length.out = nrow(final)
  ))
  return (prog)
}
# Erste Implementierung mit Business Area GSS
gss <- prognose(df=df, datum="2011-01-04", h = 2, name="GSS")
# Zweite Funktionale Programmierung nun  mit mehrere Modellen und Daraus den Mittelwert bestimmen.
mittelwert  <- function(df, datum, h, name) {
  edf_r <- ts((df), freq = 365.25 / 7,
              start = decimal_date(ymd(datum)))
  lambda <- BoxCox.lambda(edf_r)
  df1 <- data.frame(edf_r)
  one <-
    forecast(tbats(edf_r, biasadj = TRUE), h = h, lambda = lambda)
  two <-
    forecast(tbats(edf_ma, lambda = lambda, biasadj = TRUE), h = h)
  three <-
    forecast(tbats(edf_ma, lambda = 0, biasadj = TRUE), h  = h)
  four <-
    forecast(auto.arima(edf_ma, lambda = 0 , biasadj = TRUE), h = h)
  five <-   forecast(auto.arima(edf_ma, biasadj = TRUE), h = h)
  six <- forecast(ets(edf_ma, lambda = 0, biasadj = TRUE), h = h)
  seven  <-
    forecast(ets(edf_ma, lambda = lambda, biasadj = TRUE), h = h)
  
  
  Combination <-
    round((one[["mean"]] + two[["mean"]] + three[["mean"]] + four[["mean"]] +
             five[["mean"]]) / 5)
  f <-   Combination  %>% data.frame()
  
  df2 <- Combination %>%
    data.frame() %>%
    round(2) %>%
    mutate("Datum" = seq.Date(
      as.Date("2020-02-12"),
      by = "week",
      length.out = nrow(f)
    ))# %>%
  # rename("Forecast"=.)
  #as.character.Date()
  names(df2)[1] <- name
  f <- df2[1, 1]
  final <- data.frame(rbind(df1, f))
  prog <- final %>% round() %>% mutate("Stichtag" = seq.Date(
    as.Date(datum),
    by = "week",
    length.out = nrow(final)
  ))
  return (prog)
  
  
}

# Nun folgt Dataframe Definition bzw. Sektion aus der Quelle je nach Business Area.
rand <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) # 
  )  %>% arrange(date) %>% select(Randstad)
# randg <- mittelwert(df=rand , datum="2011-01-04", h = 2,  name="Randstadt")
# randg
randG <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) 
  )  %>% arrange(date) %>% select(Arb_am_Rand)
automotiv <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) # 
  )  %>% arrange(date) %>% select(Arb_AM_BA_AU.Stichtag ) %>% filter(Arb_AM_BA_AU.Stichtag > 0)
north <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) 
  )  %>% arrange(date) %>% select(Arb_am_BA_N.Stichtag )

BANorth <- prognose(df=north, datum="2011-01-04", h = 2, name="BANorth")
BANorth 
east <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) #
  )  %>% arrange(date) %>% select(Arb_Am_BA_E.Stichtag)

BAEast <- prognose(df=east, datum="2011-01-04", h = 2,  name="BAEast")
BAEast
south <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) # 
  )  %>% arrange(date) %>% select(Arb_AM_BA_S.Stichtag)

BASouth <- prognose(df=south, datum="2011-01-04", h = 2, name="BASouth")
BASouth
randstadGesamt <- prognose(df=randG, datum="2011-01-04", h = 2,  name="RandstadGesamt")
mittelwert(df=randG, datum="2011-01-04", h = 2, name="RandstadGesamt")
randstadGesamt
automotive <- prognose(df=automotiv, datum="2016-06-28", h = 2,  name="Automotive")
automotive
randstad <- prognose(df=rand, datum="2011-01-04", h = 2,  name="Randstad")
ymd(randstad$Stichtag)
# Eine Datenframe mit Prognose Werte
STM_Prognose <- data.frame(BANorth,BAEast,BASouth,randstad, gss,randstadGesamt) %>%
  select(Stichtag, Arb_am_BA_N.Stichtag,Arb_Am_BA_E.Stichtag,Arb_AM_BA_S.Stichtag,Randstad, 
         Arb_Am_Gulp.Stichtag,Arb_am_Rand )
# Nullwert ist nicht erlaubt , deshalb filtern und BA Automotive dazu nehmen.
stm <- STM_Prognose %>% filter(Stichtag>= "2016-06-28")
STM_Prognose <- data.frame(stm, automotive) %>%
  select(Stichtag, Arb_am_BA_N.Stichtag,Arb_Am_BA_E.Stichtag,Arb_AM_BA_S.Stichtag,Arb_AM_BA_AU.Stichtag,Randstad, 
         Arb_Am_Gulp.Stichtag,Arb_am_Rand)
STM_Prognose
# Spalten Umbennen 
names(STM_Prognose)[2] <- "BA North"
names(STM_Prognose)[3] <- "BA East"
names(STM_Prognose)[4] <- "BA South"
names(STM_Prognose)[5] <- "BA Automotive"
names(STM_Prognose)[6] <- "Randstad"
names(STM_Prognose)[7] <- "GSS(3000)"
names(STM_Prognose)[8] <- "RandstadGesamt"
tail(STM_Prognose)
# Hier folgt neues Modell von Fb
dfs <-  STM_RAND_ORG  %>%
  mutate (
    
    date = dmy(Stichtag) # parse date column using lubridate ymd_hms function
  ) %>% mutate(y=Arb_am_Rand) %>% mutate(ds=date)
dff <- column_to_rownames(dfs, var = "date")
help("prophet")
m <- prophet(dff)


future <- make_future_dataframe(m, freq = 'week', periods = 2)
forecast <- predict(m, future)
tail(forecast)
plot(m, forecast)
dyplot.prophet(m, forecast)

# In Deutsches Datumformat Konvertieren sonst gibt es Fehler bei HANA
# STM_Prognose$Stichtag <- format.Date(STM_Prognose$Stichtag, "%Y/%m/%d")  X$newdate <- strptime(as.character(X$date), "%d/%m/%Y")

# STM_Prognose$Stichtag <- strptime(as.character(STM_Prognose$Stichtag),"%d/%m/%Y")
# STM_Prognose$Stichtag
STM_Prognose
STM_Prognose$Stichtag <- format(STM_Prognose$Stichtag, "%Y/%m/%d")
tail(STM_Prognose)
# Falls diese Tabelle existiert, erst entfernen.
sqlDrop(Aineah,'STM_Prognose' , errors = T)
# # Neue Tabelle in HANA erstellen 
sqlSave(Aineah, STM_Prognose, rownames = FALSE)

R.Version()