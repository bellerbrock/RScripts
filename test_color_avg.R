setwd("~/projects/RScripts/Blockiness")
exeter_header <- read.csv("exeter_header.csv", header = FALSE, check.names = TRUE,  stringsAsFactors = FALSE)
blockiness_controls <- read.csv("blockiness_controls.csv", header = FALSE, check.names = TRUE,  stringsAsFactors = FALSE)
colnames(blockiness_controls) <- head(exeter_header, 1)
plot_data <- read.csv("2020allrootsv2.csv", header = TRUE, check.names = TRUE,  stringsAsFactors = FALSE)
common_cols <- intersect(colnames(blockiness_controls), colnames(plot_data))
data <- rbind(
  blockiness_controls[common_cols], 
  plot_data[common_cols]
)

identifiers <- strcapture(
  "(^[^_]+)_([^_]+)_([^_]+)_(.*)",
  as.character(data$plot_name),
  data.frame(trial = character(), plotnum = character(), rep = character(), accession = character())
)
data <- cbind(identifiers, data)

# convert hundreths of inch to cm and 10ths of ounces to grams
data$diametercm <- as.numeric(data$diameter) * 0.0254 # cm per 1/100 inch
data$lengthcm <- as.numeric(data$length) * 0.0254 # cm per 1/100 inch
data$weightg <- as.numeric(data$weight) * 2.8349523 # g per 1/10 ounce

# calculate max cylindrical volume in cubic cenimeters
data$max.volumecc <- pi * (as.numeric(data$diametercm)/2)^2 * as.numeric(data$lengthcm)  # cyclindrical volume = pi*r2*h

# calculate blockiness as ratio of weight in grams to maximum theoretical volume
# 1 = perfectly blockly (all of possible cylindrical volume filled), 0 = infinitely not blocky (none of possible cylindrical volume filled)
blockiness <- data$weightg / data$max.volumecc
data <- cbind(blockiness, data)

# Remove canners and culls by keeping only roots with diam >= 200 and length >= 299 and weight >=50
No1s_and_Jumbos <- data[data$trial == "21BLOK0002HCR" | (data$diameter >= 200 & data$length >= 300 & data$weight >= 50),]

# Set No1s as remaining roots with diam <= 350 and length <= 900 and weight <=220
No1s <- No1s_and_Jumbos[No1s_and_Jumbos$trial == "21BLOK0002HCR" | (No1s_and_Jumbos$diameter <= 350 & No1s_and_Jumbos$length <= 900 & No1s_and_Jumbos$weight <= 220),] 

# Set Jumbos as remaining roots with diam > 350 or length > 900 or weight > 220
Jumbos <- No1s_and_Jumbos[No1s_and_Jumbos$trial == "21BLOK0002HCR" | (No1s_and_Jumbos$diameter > 350 | No1s_and_Jumbos$length > 900 | No1s_and_Jumbos$weight > 220),]

average_colors <- function(indiv_colors) {
  print(indiv_colors)
  reds<- substr(indiv_colors, 1, 2)
  greens<- substr(indiv_colors, 3, 4)
  blues<- substr(indiv_colors, 5, 6)
  rsquared <- strtoi(reds, base=16)^2
  gsquared <- strtoi(greens, base=16)^2
  bsquared <- strtoi(blues, base=16)^2
  return(
    rgb(
      round(sqrt(mean(rsquared))),
      round(sqrt(mean(gsquared))),
      round(sqrt(mean(bsquared))),
      maxColorValue=255
    )
  )
}

# Adjust plot size to fit vertical x labels
par(mar=c(10,5,4,2))

# Draw No1 and Jumbo boxplot and add sample size text

trialname <- "20FFMC0040HCRN11"
title <- paste(trialname, "No1 and Jumbo Blockiness", sep=" ") 

FFMC <- No1s_and_Jumbos[No1s_and_Jumbos$trial == "21BLOK0002HCR" | No1s_and_Jumbos$trial == "20FFMC0040HCRN11",]

FFMC$accession <- with(FFMC, reorder(accession , -blockiness, median , na.rm=T))

plot_colors <- tapply(as.hexmode(substr(FFMC$colorhex1, 2, nchar(FFMC$colorhex1))), FFMC$accession, average_colors)

b <- boxplot(blockiness ~ accession, data=FFMC, main=title, ylab=expression(bold("Blockiness")), las=2, col=plot_colors, xaxt="n", xlab="")

title(xlab= "Accession", line=6, cex.lab=1, family="Calibri Light", adj=0.45, font.lab = 2)

# To add sample sizes and tilted labels
text(seq_along(na.omit(unique(FFMC$accession))), b$stats[3,] + 0.012, paste("n=", b$n))
text(unique(FFMC$accession), par("usr")[3] - .05, labels = unique(FFMC$accession), srt = 45, adj = 1, xpd = TRUE);


table(No1s_and_Jumbos$trial)
