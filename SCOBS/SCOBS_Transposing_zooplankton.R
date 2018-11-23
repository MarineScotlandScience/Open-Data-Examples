#Script to transpose SCOBS zooplankton data from row based to place zooplankton species in column


library(dkanr)
library(reshape)

#Set up the package with the url to the Marine Scotland Data portal
dkanr_setup(url="https://data.marine.gov.scot")


#Each of the datasets published on MS DAta portal has a corersponding node id that is needed to retrieve metadata
#These can be found by pulling down a list of all the datasets and finding the right one
# For simplicity, the nid for the Stonehaven site is 1981, and Loch Ewe is 2056

#So let's retrieve the metadata first
metadata <- retrieve_node(nid='2056', as = 'list')

#This retrieves the title and UUID for the dataset so we can pull down individual downloadable resources
#For csv files the resources on MS Data can be retrieve dierctly to a data table 
#But first we need a lsit of the available resources

resources <- get_resource_nids(metadata)

#The resource nid's are now in, and we can find out which one we need. In this case it is the third resource in the list, you can confirm this with@
resource_info <- retrieve_node(nid=resources[3], as ='list')

#Double check the title of the resource and confirm this is the zooplankton data.

#Confirm that the zooplankton data is in the datastore to retrieve via API
ds_is_available(resource_info)

#As long as this returns TRUE, we can pull data directly.
#Note for larger datasets, this can take a little while...
zooplankton_data <- ds_search_all(resource_id=resource_info$uuid, as = 'df')

#The table contains 4 headings 

#In this case, we can now simply use the cast command from the reshape package (alternatives in dplyr and reshape2 exist)
zooplankton_table <- cast(zooplankton_data, DateSampled ~ Species, value.var=Concentration)



