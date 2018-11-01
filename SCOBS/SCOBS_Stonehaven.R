#Let's create a script that
# 1. imports available data for the Scottish Coastal Observatory site in Stonehaven (East coast of Scotland)
# 2. Creates an overview panel of some temperature and salinity data - you can of course modify this to show other parameters, or use different plotting libraries. These are added purely as an example.

#We use the dkanr package to retrieve the data, and ggplot2 + gridExtra to create the plots
library(dkanr)
library(ggplot2)
library(gridExtra) 

#Set up the package with the url to the Marine Scotland Data portal
dkanr_setup(url="https://data.marine.gov.scot")

#Each of the datasets published on MS DAta portal has a corersponding node id that is needed to retrieve metadata
#These can be found by pulling down a list of all the datasets and finding the right one
# For simplicity, the nid for the Stonehaven site is 1981

#So let's retrieve the metadata first
metadata <- retrieve_node(nid='1981', as = 'list')

#This retrieves the title and UUID for the dataset so we can pull down individual downloadable resources
#For csv files the resources on MS Data can be retrieve dierctly to a data table 
#But first we need a lsit of the available resources

resources <- get_resource_nids(metadata)

#This returns a simple list of node ids for each avaialble resource. You can now use the retrieve_node command from above to inspect each of these resources. For example:

resource_info <- retrieve_node(nid=resources[1], as ='list')

#The first one on the list is the environmental data (nid = 1986)
#This is the one we weant to download to create environmental graph panels
# We should always check if the resource is avaialble via the machine readable/datastore services first

ds_is_available(resource_info)

#This returns true - so we are ok to progress and download via an API call to the data portal
# You can of course just download the csv - but this way will eanble you to repeat the process in the future when additional data has been added, or you want to make sure you are using the originally published data..


stn_env_data <- ds_search_all(resource_id=resource_info$uuid, as = 'df')

#Now you have got the data. It is now IMPORTANT to read the documentation for the resource
#Note that these descriptios can be quite long, and may be useful to read directly on the data portal instead

#You can retrieve this by calling
resource_info$body

#Let's first plot the surface and bottom water temperatures by Julian dates, so we can see the spread of temperatures and overall annual pattern
#Note that all parameters have quality flags. All data - including erroneous, but recorded data, are included. So we want to filter out bad data first
# Also note that R will interpret most data as character data at this stage, so we will need to force it to numeric

#We now only include data with quality flags 1,2,5 and 8 on the temperature measurement. We select only the relevant parameters from the main dataset for this exercise, but include year so we can create a tiem series as well)
#The selected quality flags are based on the recommendation in the resource description. If you didn't read it - please check it on :
# https://data.marine.gov.scot/dataset/scottish-coastal-observatory-stonehaven-site/resource/193b02c3-0eba-4655-b160-0061a3067b3e

temp_data <- subset(stn_env_data, (F17_Temperature ==1 | F17_Temperature ==2 | F17_Temperature ==5 | F17_Temperature ==8), select=c("D9_Julian Day", "D12_Year","D16_Depth", "D17_Temperature _oC" ))

# There are slight variations in the sampled depth for surface and near bottom temperatures, so we will add a grouping column
temp_data$bins <- cut(as.numeric(temp_data$D16_Depth), breaks=c(0,15,100), labels=c("Near Suface","Near bottom"))

#Now we can create the plot of all values across days. This means you will get a scatter plot that shows recorded (and filtered on the quality flag) data for all the years on top of each other. This can show you the "typical" patter, but might also include some noise if there are anomalous years.
#We assign the plotting command to a variable so it can be added to a grid later. You can also preview the individual graphs jsut by calling that variable

temp1 <- ggplot(temp_data, aes(x=as.numeric(get("D9_Julian Day")), y = as.numeric(get("D17_Temperature _oC")), colour=bins)) + geom_point() +
labs(title="Water Temperature at Stonehaven Site") +
labs(x = "Julian Day", y = "Temperature ºC") 

#We will also create a time series plot to view annual varitation. To do this, we create a continuous variable from year and Julian date
temp_data$decimalYear = as.numeric(temp_data$D12_Year) + (as.numeric(temp_data$"D9_Julian Day")/ 365)

#This is the plotting command for the time series
temp2 <- ggplot(temp_data, aes(x=decimalYear, y = as.numeric(get("D17_Temperature _oC")), colour=bins)) + geom_line() +
labs(title="Water Temperature time series at Stonehaven Site") +
labs(x = "Time", y = "Temperature ºC") 



#Now we can do the same for salinity

#We now only include data with quality flags 1,2,5 and 8 on the temperature measurement (as per the recommendations in the description of the dataset)

salinity_data <- subset(stn_env_data, (F18_Salinity ==1 | F18_Salinity ==2 | F18_Salinity ==5 | F18_Salinity ==8), select=c("D9_Julian Day","D12_Year", "D16_Depth", "D18_Salinity"))

# There are slight variations in the sampled depth for surface and near bottom temperatures, so we will add a grouping column
salinity_data$bins <- cut(as.numeric(salinity_data$D16_Depth), breaks=c(0,15,100), labels=c("Near Suface","Near bottom"))

#Add the time series decimal year
salinity_data$decimalYear <- as.numeric(salinity_data$D12_Year) + (as.numeric(salinity_data$"D9_Julian Day")/ 365)


sal1 <- ggplot(salinity_data, aes(x=as.numeric(get("D9_Julian Day")), y = as.numeric(get("D18_Salinity")), colour=bins)) + geom_point() +
labs(title="Salinity at Stonehaven Site") +
labs(x = "Julian Day", y = "Salinity") 

sal2 <- ggplot(salinity_data, aes(x=decimalYear, y = as.numeric(get("D18_Salinity")), colour=bins)) + geom_line() +
labs(title="Water Salinity time series at Stonehaven Site") +
labs(x = "Time", y = "Salinity") 

#Finally it's time to arrange the plots on a page. The time series plots are being laid out to be wider with the layout matrix settings

grid.arrange(temp2,temp1, sal2, sal1, layout_matrix = rbind(c(1,1,2), c(3,3,4)), respect=TRUE)

#The plot might look at bit squished in the default output window, but this can be fixed if you are outputting the content by defining the aspect ration
#Here we will save the plot to a png file instead (the file is set to 900 by 600 pixels in this example)

png("Stonehaven_Temperature_and_Salinity_plots.png", 900,600)
grid.arrange(temp2,temp1, sal2, sal1, layout_matrix = rbind(c(1,1,2), c(3,3,4)), respect=TRUE)
dev.off()
