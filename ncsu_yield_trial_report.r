library(brapirv1)
library(dplyr)
library(reshape)
library(ggplot2)

### Settings ###

trialName <- c("19ADYT0008HCRF10") 
plotSize <- 175  # in square feet, e.g. 10ft plot by 3.5ft wide = 35

# Retrieve data from spbase
spbase <- brapi_db()$sweetpotatobase
programs <- brapi_get_programs(spbase, pageSize = 9999999)
program <- "NCSU"
programDbId <- programs$programDbId[programs$programName==program]
trials <- brapi_get_studies(spbase, programDbId = programDbId, pageSize = 9999999)
trialDbIds <- trials$studyDbId[trials$studyName %in% trialName]

searchResults <- brapi_post_search_observationtables(
  spbase, 
  studyDbIds = trialDbIds,
  pageSize = 9999999
)

spDataset <- brapi_get_search_observationtables_searchResultsDbId(
  spbase,
  Accept = "text/csv",
  searchResults$searchResultsDbId,
  pageSize = 9999999
)

# remove header duplicated in first row
phenodata <- spDataset[-1, ]

# extract relevant columns and shorten trait names
traits = c(
  "germplasmName",
  "Weight of cull storage roots measuring kg per plot|CO_331:0000612|77235", 
  "Weight of canner storage roots measuring kg per plot|CO_331:0000610|77239",
  "Weight of 90 count US no. 1 storage roots measuring kg per plot|CO_331:0000613|77237",
  "Weight of 55 count US no. 1 storage roots measuring kg per plot|CO_331:0000614|77231",
  "Weight of 40 count US no. 1 storage roots measuring kg per plot|CO_331:0000615|77226",
  "Weight of 32 count US no. 1 storage roots measuring kg per plot|CO_331:0000616|77223",
  "Weight of jumbo storage roots measuring kg per plot|CO_331:0000611|77232"  
)
raw_data.barplot = as.data.frame(subset(phenodata, select = c(traits)))
colnames(raw_data.barplot) = c("germplasmName","CUL","CAN","C90","C55","C40","C32","JUM")

# make data numeric and remove plot rows with no yield data
raw_data.barplot[2:8] = lapply(raw_data.barplot[2:8], FUN = function(y){as.numeric(y)})
clean_data.barplot <- na.omit(raw_data.barplot)

# Convert kg/plot to 50lb bushels per acre (bpa)
  # 50lbs = 22.6796kg
  # 1 acre = 43560 square ft
  # Need to do a better job tracking plot size, and what fraction thereof is run through the Exeter!
  # Plot size can be set at beginning, otherwise defaults to 10ft ( 3.5ft wide * 10ft long = 35 square ft )

clean_data.barplot[2:8] = lapply(clean_data.barplot[2:8], FUN = function(y){as.numeric(
  if (plotSize) y * (1/22.6796) * 43560 * (1/plotSize) else y * (1/22.6796) * 43560 * (1/35)
)})

# Calculate mean across reps
data.barplot = group_by(clean_data.barplot, germplasmName) %>% summarize(
  meanCULbpa = mean(CUL),
  meanCANbpa = mean(CAN),
  meanC90bpa = mean(C90),
  meanC55bpa = mean(C55),
  meanC40bpa = mean(C40),
  meanC32bpa = mean(C32),
  meanJUMbpa = mean(JUM)
)
data.barplot <- as.data.frame(data.barplot)

# Calculate max total yield value for setting y scale
total_root_weights = data.barplot$meanCULbpa + data.barplot$meanCANbpa + data.barplot$meanC90bpa +
  data.barplot$meanC55bpa + data.barplot$meanC40bpa + data.barplot$meanC32bpa + data.barplot$meanJUMbpa
max = max(total_root_weights, na.rm=TRUE) * 1.2


# Transform data structure for plotting
data.barplot.melt <- melt(data.barplot)
data.barplot.melt$variable = gsub("meanCULbpa", "Cull" ,data.barplot.melt$variable)
data.barplot.melt$variable = gsub("meanCANbpa", "Canner",data.barplot.melt$variable)
data.barplot.melt$variable = gsub("meanC90bpa", "No.1 (5.0-9.4oz)",data.barplot.melt$variable)
data.barplot.melt$variable = gsub("meanC55bpa", "No.1 (9.5-14oz)",data.barplot.melt$variable)
data.barplot.melt$variable = gsub("meanC40bpa", "No.1 (14.1-18oz)",data.barplot.melt$variable)
data.barplot.melt$variable = gsub("meanC32bpa", "No.1(18.1-22oz)",data.barplot.melt$variable)
data.barplot.melt$variable = gsub("meanJUMbpa", "Jumbo",data.barplot.melt$variable)

data.barplot.melt$variable = factor(data.barplot.melt$variable,
                                    levels = c("Cull", "Canner", "No.1 (5.0-9.4oz)",
                                               "No.1 (9.5-14oz)", "No.1 (14.1-18oz)",
                                               "No.1(18.1-22oz)", "Jumbo"))

# Construct stacked barplot
plot = ggplot( data.barplot.melt, aes(x = germplasmName, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.65) + 
  coord_cartesian(ylim = c(0,max)) +
  scale_fill_viridis_d(direction = -1)+
  # scale_fill_manual(values = c("#FF3300", # red - cull
  #                              "#FF9900", # yellow - canner
  #                              "#3dff3d", # lightest green - 5.0-9.4oz
  #                              "#00ed00", # light green - 9.5-14oz
  #                              "#00b200", # green - 14.1-18oz
  #                              "#006400", # dark green - 18.1-22oz
  #                              "#000099") # blue - jumbo
  #                   ) + 
  theme(axis.text.x = element_text(color="black", size=14,angle = 45, hjust = 1),
        axis.text.y = element_text(color="black", size=14),
        axis.title.y=element_text(size=16,face="bold") ,
        axis.title.x=element_text(size=16,face="bold") ,
        plot.title = element_text(size=18, face="bold.italic", hjust = 0.5),
        plot.margin = unit(c(0.5,6,0.5,1), "cm"),
        legend.position = c(1.15, 0.7),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14) ) +
        labs(fill="Yield Class") +
        labs(y = "Yield (50lb Bushel/acre)" , x = "Clone",
        title = phenodata$studyName)

theme(legend.title = element_text(colour="blue", size=16, face="bold"))

print(plot)
