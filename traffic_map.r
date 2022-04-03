# This script generates an animation of a map of the highways in the Netherlands coloured by their traffic intensity

# Load the necessary packages
library(sf)
library(sp)
library(readxl)
library(tidyr)
library(animation)
library(tibble)

#Load the road and traffic data
roads <- sf::st_read("INWEVA_2019/B. Outputbestanden Shapes/Definitief_INWEVA_2019_Weekdag_uren_deel1.shp")
roads <- roads[roads$HECTO_LTTR %in% c('#'),]
traffic <- read_excel("INWEVA_2019/A. Outputbestanden Excel/INWEVA_2019_weekdag_uren.xlsx")


# Uncomment one of these 4 depending on the traffic type you want
# traffic_type <- 'AL_uur' #all vehicles
traffic_type <- 'L1_uur' #personenauto's (cars)
# traffic_type <- 'L2_uur' #middelzwaar vrachtverkeer (middleweight cargo traffic)
# traffic_type <- 'L3_uur' #zwaar vrachtverkeer (heavy cargo traffic)

# Select only the relevant columns for the chosen traffic type
list_variables <- colnames(traffic[grepl(traffic_type, names( traffic))])
traffic <- traffic[c(c('NwbId'), list_variables)]

# Select the maximum and minimum per hour seen throughout the day. We will need it for the colour scale later
max_traffic <- max(traffic[,list_variables])
min_traffic <- min(traffic[,list_variables])

# Unnest the dataframe
traffic <- traffic %>%
  mutate(NwbId = strsplit(as.character(NwbId), ",")) %>%
  unnest(NwbId)

# Merge the traffic and roads dataframes, and keep only the relevant traffic variables and the geometry
roads <- merge(x = roads, y = traffic, by.x = "WVK_ID", by.y = "NwbId", all.x = TRUE)
roads <- roads[,c(list_variables, c('geometry'))]
colnames(roads) <- paste(colnames(roads),"0",sep="")
roads <- as.data.frame(roads)

# Add intermediate columns, to make animation transitions smoother
frame_subdivisions <- 10
original_columns <- colnames(roads)

for (i in 1:24){
  col1 <- original_columns[i]
  if(i<24){
    col2 <- original_columns[i+1]
  }
  else{
    col2 <- original_columns[1]
  }
  for(j in 2:frame_subdivisions-1){
    temp_col <- as.data.frame(roads[col1] + j*(roads[col2]-roads[col1])/frame_subdivisions)
    colnames(temp_col) <- paste(substr(col1,1,nchar(col1)-1), as.character(j), sep="")
    
    if(i<24){
      roads <- add_column(roads, temp_col, .before = col2)
    }
    else{
      roads <- add_column(roads, temp_col, .before = 'geometry0')
    }
  }
}

# Convert to SpatialLinesDataFrame
list_variables_2 <- colnames(roads)[1:240]
geom <- as(roads$geometry, "Spatial")
traffic_density <- roads[,list_variables_2]
geom <- SpatialLinesDataFrame(sl = geom, data=as.data.frame(traffic_density), match.ID = FALSE)


# Define a function for the color gradient
color.gradient <- function(x, colors=c("#4D4C7D","#3282B8","#346751","#4E9F3D","#FEC260","#E79E4F","#FF4301","#AF0404"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min_traffic,0.8*max_traffic, length.out=colsteps))])
}

# Plotting text
plot_name <- function(string) {
  par(mar = c(0,0,0,0), bg = '#22303C')
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, string, cex = 1.6, col = "#3282B8")
}


# Create animation. hour should take values between 1 and 239, but my laptop could not handle it and I had to break it into 2 pieces
animation::saveGIF(
  expr = {
      for(hour in 1:150){
    # for(hour in 151:239){
      
      layout(matrix(c(1,2), 2, 1), widths=c(1,1), heights=c(1,8))
      plot_name(paste0(hour%/%10,":00"))
      plot(geom,col=color.gradient(geom[[list_variables_2[hour]]]), lwd=3,bg = '#22303C')
    }
  },
  movie.name = "my_traffic_video.gif"
)