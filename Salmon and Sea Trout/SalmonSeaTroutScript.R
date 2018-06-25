#Retrieve open data from Marine Scotland Open Data Portal

library(dkanr)
dkanr_setup(url="https://data.marine.gov.scot")

all_datasets <- list_nodes_all(filters=c(type="dataset"))


salmon_datasets <- all_datasets[(grep("Salmon", all_datasets$title)),]

#Let's check the titles to find the relevant dataset
salmon_datasets$title
#The second one on the list, looks interesting. We need to rertrieve the field nid for this one to progress
salmon_datasets$nid[2]


#So that's nid = 1441 - we will jsut retrieve a single dataset in this example
ds_meta <- retrieve_node(nid='1441', as = 'list')

#Now list the associated resources.
resource_list <- get_resource_nids(ds_meta)

length(resource_list)


#Create metadata and retrieve data from each resource

#First resource

#Let's first get the metadata for the downloadable resource
rs1m <- retrieve_node(nid = resource_list[1], as = 'list')

#Now lets check the title so we know what it is
rs1m$title

#It's fixed engine fisheries. Wwe want a copy of this data table, first we check if the data is in the datastore
ds_is_available(rs1m)

#That returned TRUE, so we can access data directly. We now use the uuid form the resource metadata to retrieve it

rs1data <- ds_search_all(resource_id=rs1m$uuid, as= "df")

#Second resource

#Let's get the metadata for the next downloadable resource
rs2m <- retrieve_node(nid = resource_list[2], as = 'list')

#Check what it is - Net & Coble in this case
rs2m$title

#Check if we can access directly
ds_is_available(rs2m)

rs2data <- ds_search_all(resource_id=rs2m$uuid, as= "df")

#Rod and Line - released
rs3m <- retrieve_node(nid = resource_list[3], as = 'list')
rs3data <- ds_search_all(resource_id=rs3m$uuid, as= "df")

#Rod and line retained
rs4m <- retrieve_node(nid=resource_list[4], as ='list')
rs4data <- ds_search_all(resource_id=rs4m$uuid, as= "df")




#inspecting the data, we find monthly values. We want to do two things:
# 1) create a column using the year and month to create a continuous variable for plotting

#First let's create the continuous variables for each data set
rs1data$decimalyear <- as.numeric(rs1data$Year) + (as.numeric(rs1data$"Month Number")/12)
rs2data$decimalyear <- as.numeric(rs2data$Year) + (as.numeric(rs2data$"Month Number")/12)
rs3data$decimalyear <- as.numeric(rs3data$Year) + (as.numeric(rs3data$"Month Number")/12)
rs4data$decimalyear <- as.numeric(rs4data$Year) + (as.numeric(rs4data$"Month Number")/12)

#Also lets clean up names as many have spaces in column headings
names(rs1data) <- make.names(names(rs1data), unique=TRUE)
names(rs2data) <- make.names(names(rs2data), unique=TRUE)
names(rs3data) <- make.names(names(rs3data), unique=TRUE)
names(rs4data) <- make.names(names(rs4data), unique=TRUE)


#At this stage it is becoming awkard to have the files split, since we want to be able to compare different 
types of methods with each other for each region.
#For this example, we are not going to use the two columns that are different
#(netting effort median trap number in fixed engine data (rs1data))
#(Netting effort median crew number in net & coble (rs2data)
#so we are going to remove these, and then add a column to each dataset to identify method (using the metadata title)

rs1data[21] <- NULL
rs2data[21] <- NULL

rs1data$Source <- 1
rs2data$Source <- 2
rs3data$Source <- 3
rs4data$Source <- 4


dataset <- rbind(rs1data, rs2data)
dataset <- rbind(dataset,rs3data)
dataset <- rbind(dataset,rs4data)


#We want a list of all the Scottish Marine Regions included 
smr <- unique(dataset$Scottish.Marine.Region)

#So now we can generate a plot for each of the marine regions on demand
#The smr vector has 14 regions listed (use length(smr) to check)
#This example starts with the first region by setting the Region valiable to 1
#The rest of the script can then be repeated for each region to create the plot

#This loop will run through all the scottish marine regions, and create a plot of annual catches by method
Region <- 1

sub <- dataset[(grep(smr[Region],dataset$Scottish.Marine.Region)),]
#We sort by year to avoid arbitraty jumps between lines in our plots
sub <- sub[order(sub$decimalyear),]

with (sub[(sub$Source==1),], plot(Wild.Salmon.Number~decimalyear, type="l", title = smr[Region], xlab= "Time", ylab="Number of Fish/Month"))
if(nrow(sub[(sub$Source==2),])>0) {
with (sub[(sub$Source==2),],lines(Wild.Salmon.Number~decimalyear, col="red"))
}
if(nrow(sub[(sub$Source==3),])>0) {
with (sub[(sub$Source==3),],lines(Wild.Salmon.Number~decimalyear, col="green"))
}
if(nrow(sub[(sub$Source==4),])>0) {
with (sub[(sub$Source==4),],lines(Wild.Salmon.Number~decimalyear, col="blue"))
}


#Next - let's create annual values. This makes it easier to see the data. In this case, we are jsut aggregating numbers, but wieghts could be added to the first list in the expression
annual_data <- aggregate(list(as.numeric(dataset$Wild.Salmon.Number), as.numeric(dataset$Wild.Grilse.Number),as.numeric(dataset$Sea.Trout.Number), as.numeric(dataset$Finnock.Number), as.numeric(dataset$Farmed.Salmon.Number), as.numeric(dataset$Farmed.Grilse.Number)) , by = list(dataset$Scottish.Marine.Region, dataset$Year, dataset$Source), FUN=sum)
names(annual_data) <- c("SMR", "Year","Method", "WildSalmon", "WildGrilse", "SeaTrout", "Finnock", "FarmedSalmon", "FarmedGrilse")


#Now we can create annual plots that are a little easier to read, using a similar approach as the one used to create the monthly plots. 

#This colour palette is color blind friendly, so should help with accessibility/viewing of graph
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for(i in 1:length(smr)) {


smr_region <- i #(still using the smr list from before) - this can be used for the trout plot if done separately
region_subset <- annual_data[(grep(smr[smr_region], annual_data$SMR)),]
region_subset <- region_subset[order(region_subset$Year),]

#We need to find the maximum value in the plotted dataset to scale the graph
ymax = max(region_subset$WildSalmon + region_subset$WildGrilse)

#Setting titles and the name of the image that will be saved for each plot (you can comment it out if you don't wnat to save images)
maintitle = paste("Annual Salmon & Grilse Catch: ",smr[smr_region], sep="")
imgname = paste("AnnualSalmon",gsub(" ","",smr[smr_region]),".png",sep="")

#Create the plot, showing each catch method as a separate line
with(region_subset[(region_subset$Method==1),], plot((WildSalmon+WildGrilse)~Year, type="l", ylim=c(0,ymax), main=maintitle, xlab="Year", ylab="Number of Fish/Year", lty=1, lwd=2, col=cbbPalette[1]))
with(region_subset[(region_subset$Method==2),], lines((WildSalmon+WildGrilse)~Year, type="l", lty=1, lwd=2,col=cbbPalette[2] ))
with(region_subset[(region_subset$Method==3),], lines((WildSalmon+WildGrilse)~Year, type="l", lty=1, lwd=2, col=cbbPalette[3]))
with(region_subset[(region_subset$Method==4),], lines((WildSalmon+WildGrilse)~Year, type="l", lty=1, lwd=2,col=cbbPalette[4]))
legend( "topright", c("Fixed Engine", "Net & Coble", "Rod & Line (release)", "Rod & Line (retain)"), lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c(cbbPalette[1],cbbPalette[2],cbbPalette[3],cbbPalette[4] ))

#Save the image - here set to 800x600 pixels. You can switch off or change image size below
dev.copy(png,filename=imgname, width = 800, height=600);
#Hiding the output window if you are saving image
dev.off();
dev.off();


#Next part is for annual plots of sea trout

ymax = max(region_subset$SeaTrout)
maintitle = paste("Annual Sea Trout Catch: ",smr[smr_region], sep="")
imgname = paste("AnnualSeaTrout",gsub(" ","",smr[smr_region]),".png", sep="")

with(region_subset[(region_subset$Method==1),], plot((SeaTrout)~Year, type="l", ylim=c(0,ymax), main=maintitle, xlab="Year", ylab="Number of Fish/Year", lty=1, lwd=2, col=cbbPalette[1]))
with(region_subset[(region_subset$Method==2),], lines((SeaTrout)~Year, type="l", lty=1, lwd=2,col=cbbPalette[2] ))
with(region_subset[(region_subset$Method==3),], lines((SeaTrout)~Year, type="l", lty=1, lwd=2, col=cbbPalette[3]))
with(region_subset[(region_subset$Method==4),], lines((SeaTrout)~Year, type="l", lty=1, lwd=2,col=cbbPalette[4]))
legend( "topright", c("Fixed Engine", "Net & Coble", "Rod & Line (release)", "Rod & Line (retain)"), lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c(cbbPalette[1],cbbPalette[2],cbbPalette[3],cbbPalette[4] ))

#Save the image - here set to 800x600 pixels. You can switch off or change image size below
dev.copy(png,filename=imgname, width = 800, height=600);
#Hiding the output window if you are saving image
dev.off();
dev.off();

}