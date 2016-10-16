#library(dplyr)
library(ggplot2)
library(chron)
library(RColorBrewer)
# library(openxlsx)
# 
# test <- read.xlsx("ITH - Incidents by service cause.xlsx")
thedata <- read.table("ITH_Incidents_by_service_cause.csv",
                      sep = ",",
                      header = T,
                      na.strings = c(""),
                      fill = T,
                      col.names = c("Incident.ID", "Updated.By.Cust",
                          "Service", "Category","Cause.Code",
                          "Status", "Major.Incident","Priority",
                          "Customer","Customer.Email.Address","Location",
                          "VIP","Owner","Source",
                          "Team","Created.On","Modified.On",
                          "Last.Modified.By","Symptom","Inbound.email.address",
                          "First.Time.Fix")
                      , 
                      colClasses = c("integer", "factor",
                          "factor",  "factor", "factor",
                          "factor",  "factor", "factor",
                          "factor",  "character", "factor",
                          "factor",  "factor", "factor",
                          "factor",  "character", "character",
                          "factor",  "character", "character",
                          "factor")
                      )

  thedata$Created.On <- base::as.Date(thedata$Created.On, format = "%d/%m/%Y")
  thedata$Modified.On<- base::as.Date(thedata$Modified.On, format = "%d/%m/%Y")

  thedata$Symptom[is.na(thedata$Symptom)] <- c("none")
  thedata$Symptom  <- as.factor(thedata$Symptom)
  thedata$Cause.Code <- as.character(thedata$Cause.Code)

## remove spam
  thedata_nospam <- thedata[thedata$Cause.Code != "Spam" &
                              is.weekend(thedata$Created.On) == F &
                              thedata$Created.On != c("2015-04-17","2015-07-07")
                            , ]
  
## plot requests over time excluding spam
  dnss_symptom <- data.frame(table(thedata_nospam$Created.On, thedata_nospam$Symptom))
  names(dnss_symptom) <- c("Created.On","Symptom","fqcy")
  dnss_symptom$Created.On <- as.Date(dnss_symptom$Created.On, format = "%Y-%m-%d")

  ps <- ggplot(dnss_symptom, aes(Created.On, fqcy)) + geom_line() + facet_wrap(~Symptom, ncol = 2)
  print(ps + labs(x = "Date Created (excludes Weekends)", y = "Number of Incidents per day", main = "HEAT Incidents per day by Symptom"))

# dnss_team <- data.frame(table(thedata_nospam$Created.On, thedata_nospam$Team))
# names(dnss_team) <- c("Created.On","Team","fqcy")
# dnss_team$Created.On <- as.Date(dnss_team$Created.On, format = "%Y-%m-%d")
# 
# pt <- qplot(Created.On, fqcy,data = dnss_team,  xlab = "Date Created", ylab = "Number of Incidents per day")
# print(pt + geom_line())

q <- qplot(Created.On, fqcy, data = dnss_symptom, geom = c("line","smooth"), method = "loess", colour = Symptom, log(y),
           xlab = "Date Created (excludes Weekends)", ylab = "Number of Incidents per day", main = "HEAT Incidents per day by Symptom\n(unified with Loess smoother)")
# print(q)

dnss_team            <- data.frame(table(thedata_nospam$Created.On, thedata_nospam$Team))
names(dnss_team)     <- c("Created.On","Team","fqcy")
dnss_team$Created.On <- as.Date(dnss_team$Created.On, format = "%Y-%m-%d")
dnss_team_activity   <- aggregate(dnss_team$fqcy, by = list(dnss_team$Team), "sum")
dnss_team_busy       <- dnss_team_activity[dnss_team_activity$x > 100, 1]


r <- ggplot(dnss_team[dnss_team$Team %in% dnss_team_busy,], aes(Created.On, fqcy)) + geom_line() + facet_wrap(~Team, ncol = 5)
print(r + labs(x = "Date Created (excludes weekends)", y = "Number of Incidents per day"))

## look at who puts in which requests
# requests <- thedata[thedata$Cause.Code == "Request", 1]
requests <-    data.frame(table(thedata_nospam$Owner,thedata_nospam$Symptom, thedata_nospam$Created.On))
names(requests) <-  c("Owner","Symptom","Created.On","fqcy")
requests$Created.On <- as.Date(requests$Created.On, format = "%Y-%m-%d")
  tmp <-   data.frame(table(thedata_nospam$Owner))
  names(tmp) <-  c("Owner","fqcy")
top_requesters <- tmp[tmp$fqcy > 200, 1]

s <- ggplot(requests[requests$Owner %in% top_requesters, ], aes(Created.On, fqcy, colour = Symptom)) + geom_line() + facet_wrap(~Owner, ncol = 4) + scale_fill_brewer(palette = "Accent")
print(s + labs(x = "Date Created (excludes weekends)", y = "Number of Incidents per day") )

causes <- data.frame(table(thedata_nospam$Symptom, thedata_nospam$Cause.Code))
names(causes) <- c("Symptom", "Cause.Code", "fqcy")
t <- ggplot(causes, aes(Symptom,Cause.Code)) + geom_point(size = log2(causes$fqcy))
print(t)

library(circlize)
tab <- table(thedata_nospam$Symptom, thedata_nospam$Cause.Code)
chordDiagram(tab, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3))
# we go back to the first track and customize sector labels
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
              niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
