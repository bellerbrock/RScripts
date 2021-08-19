#load necessary libraries
library(ggplot2)
library(dplyr)

#read in data
yielddat = read.csv("/home/bryan/Desktop/08-18yieldandweatherHCRCUN.csv", header=T)

#filter out rows with no yield data
yielddat <- yielddat %>% subset(!is.na(US1_kgplot))

# filter out clones that have been measured for yield less than 'n' times 
 filtered <- yielddat %>% 
  group_by(germplasmName) %>% 
  filter(n() >= 75)

# write and re-read data. avoids strange error in ordering step that otherwise still sees 633 clones in dataset after filtering
write.csv(filtered, file ="/home/bryan/filter_file.csv")
data1 <- read.csv("/home/bryan/filter_file.csv", header=T)

# calculate yields on plant basis and adjust for length of trial # What's up with Bonita?
data1$no1perplant <- (data1$US1_kgplot/data1$PltsHrv_plot)
data1$totalperplant <- (data1$Tot_kgplot/data1$PltsHrv_plot)
data1$no1perplantDAP <- (data1$US1_kgplot/data1$PltsHrv_plot)/data1$DAP*100
data1$totalperplantDAP <- (data1$Tot_kgplot/data1$PltsHrv_plot)/data1$DAP*100

# NO1: determine correct name and color orders in relation to the boxplot medians, then plot using native boxplot function
data1$germplasmName <- with(data1, reorder(germplasmName , -no1perplant, median , na.rm=T))
colors <- levels(with(data1, reorder(skinColor , -no1perplant, median , na.rm=T)))
b <- boxplot(no1perplant ~ germplasmName, data=data1, main="2008-2018 Elite Clone No1 Yield", ylab="No 1 Yield kg/plant", notch=T, xaxt="n", col=colors)
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[2,] + 0.04, paste("n=",b$n, sep='')) # add text for number of mesurements
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[3,] + 0.04, format(round(b$stats[3,], 2), nsmall = 2)) # add text of mean value
text(unique(data1$germplasmName), par("usr")[3] - .05, labels = unique(data1$germplasmName), srt = 45, adj = 1, xpd = TRUE); # add 45 deg labels

# NO1 DAP: determine correct name and color orders in relation to the boxplot medians, then plot using native boxplot function
data1$germplasmName <- with(data1, reorder(germplasmName , -no1perplantDAP, median , na.rm=T))
colors <- levels(with(data1, reorder(skinColor , -no1perplantDAP, median , na.rm=T)))
b <- boxplot(no1perplantDAP ~ germplasmName, data=data1, main="2008-2018 Elite Clone No1 Yield DAP adj", ylab="No 1 Yield kg/plant at 100 DAP", notch=T, xaxt="n", col=colors)
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[2,] + 0.04, paste("n=",b$n, sep='')) # add text for number of mesurements
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[3,] + 0.04, format(round(b$stats[3,], 2), nsmall = 2)) # add text of mean value
text(unique(data1$germplasmName), par("usr")[3] - .05, labels = unique(data1$germplasmName), srt = 45, adj = 1, xpd = TRUE); # add 45 deg labels

# Total: determine correct name and color orders in relation to the boxplot medians, then plot using native boxplot function
data1$germplasmName <- with(data1, reorder(germplasmName , -totalperplant, median , na.rm=T))
colors <- levels(with(data1, reorder(skinColor , -totalperplant, median , na.rm=T)))
b <- boxplot(totalperplant ~ germplasmName, data=data1, main="2008-2018 Elite Clone Total Yield", ylab="Total Yield kg/plant", notch=T, xaxt="n", col=colors)
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[2,] + 0.04, paste("n=",b$n, sep='')) # add text for number of mesurements
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[3,] + 0.04, format(round(b$stats[3,], 2), nsmall = 2)) # add text of mean value
text(unique(data1$germplasmName), par("usr")[3] - .05, labels = unique(data1$germplasmName), srt = 45, adj = 1, xpd = TRUE); # add 45 deg labels

# Total DAP: determine correct name and color orders in relation to the boxplot medians, then plot using native boxplot function
data1$germplasmName <- with(data1, reorder(germplasmName , -totalperplantDAP, median , na.rm=T))
colors <- levels(with(data1, reorder(skinColor , -totalperplantDAP, median , na.rm=T)))
b <- boxplot(totalperplantDAP ~ germplasmName, data=data1, main="2008-2018 Elite Clone Total Yield DAP adj", ylab="Total Yield kg/plant at 100 DAP", notch=T, xaxt="n", col=colors)
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[2,] + 0.04, paste("n=",b$n, sep='')) # add text for number of mesurements
text(seq_along(na.omit(unique(data1$germplasmName))), b$stats[3,] + 0.04, format(round(b$stats[3,], 2), nsmall = 2)) # add text of mean value
text(unique(data1$germplasmName), par("usr")[3] - .05, labels = unique(data1$germplasmName), srt = 45, adj = 1, xpd = TRUE); # add 45 deg labels


# alternative option - plot using ggplot
ggplot(data1, aes(x = fct_reorder(germplasmName, no1perplant, fun = median , .desc = T), y = no1perplant)) + geom_boxplot(notch = TRUE) +
  theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("2008-2018 Elite Clone Performance")