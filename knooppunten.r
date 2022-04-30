# This script generates an animations with 4 graphs showing the number of vehicles per hour
# in a selection of highway exchanges (knooppunten) in the Netherlands

# Load the necessary packages
library(readxl)
library(dplyr)
library(sf)
library(sp)

# Load the data
load_data <- function(traffic_file, roads_file) {
  traffic <- read_excel(traffic_file)
  traffic <- traffic[traffic$Traject_naar != traffic$Traject_van,]
  roads <- sf::st_read(roads_file)
  final_traffic <- as.data.frame(c(0:23))
  colnames(final_traffic) <- 'hour'
  return(list(traffic,roads,final_traffic))
}

# Function that returns the traffic data for the chosen knooppunten, as well as the road objects
get_traffic_summary <- function(knooppunten,final_traffic,traffic_file, roads_file, traffic_type) {
  temp <- load_data(traffic_file, roads_file)
  traffic <- temp[[1]]
  roads <- temp[[2]]
  final_traffic <- temp[[3]]
  traffic_type <- paste(traffic_type,'_uur', sep="")
  
  list_kp_objects <- list()
  i <- 1
  for (knooppunt in knooppunten){
    # Select only the relevant road sections and traffic type from the traffic data
    traffic_temp <- traffic[traffic$Traject_naar %in% knooppunt,]
    traffic_temp <- traffic_temp[grepl(traffic_type, names( traffic_temp))]
    traffic_temp <- as.data.frame(colSums(traffic_temp))
    
    # "Clean" the column names of spaces and dashes
    colnames(traffic_temp) <- knooppunt
    new_name <- gsub(" ", "_", knooppunt, fixed = TRUE)
    new_name <- gsub("-", "_", new_name, fixed = TRUE)
    final_traffic[new_name] <- traffic_temp[knooppunt]
    
    # Select only the relevant roads from the road data
    temp_var <- roads %>% filter(STT_NAAM == knooppunt)
    rownames(temp_var) <- NULL
    temp_var <- temp_var[1:(nrow(temp_var)-5),]
    temp_var <- as(temp_var$geometry, "Spatial")
    list_kp_objects[[i]] <- temp_var
    i <- i + 1
  }
  return(list(final_traffic,list_kp_objects))
}

# Plotting
axes_color <- '#3282B8'
plot_chart <- function(to_plot, line_color, to_plot2, line_color2, max_hour, xaxis_show, y_axis_lim, yaxis_marks, yaxis_labels) {
  par(mar=c(2,4,1,2), bg = '#22303C')
  if(xaxis_show==TRUE){
    plot(final_traffic_2019$hour[1:max_hour], to_plot[1:max_hour]/1000
         , type = 'l', lwd=2, col = line_color, pch = 21
         , axes = FALSE
         ,ylab = 'Vehicles / h', col.lab = axes_color, cex.lab = 1.2
         ,ylim = y_axis_lim
         ,xlim = c(0,24)
         ,fg =axes_color
    )
    axis(1, at = c(0,5,10,15,20,23), labels = c('0am','5am','10am','3pm','8pm','11pm'), col=axes_color, col.ticks=axes_color, col.axis=axes_color, cex.axis = 1.2)
    axis(2, at = yaxis_marks, labels = yaxis_labels, col=axes_color, col.ticks=axes_color, col.axis=axes_color, cex.axis = 1.2)
  } 
  else {
    plot(final_traffic_2019$hour[1:max_hour], to_plot[1:max_hour]/1000
         , type = "l", lwd=2, col = line_color, pch = 21
         , axes = FALSE
         ,ylab = "Vehicles / h", col.lab = axes_color, cex.lab = 1.2
         ,xlab = ""
         ,ylim = y_axis_lim
         ,xlim = c(0,24)
         ,fg =axes_color
    )
    axis(2, at = yaxis_marks, labels = yaxis_labels, col=axes_color, col.ticks=axes_color, col.axis=axes_color, cex.axis = 1.2)
  }
}

# Plotting the roads
plot_kp <- function(to_plot) {
  par(mar=c(0.5,0,0,0), bg = '#22303C')
  plot(to_plot,col='#dca861', lwd=0.5, bg = '#22303C')
}

# Plotting text
plot_name <- function(string) {
  par(mar = c(0,0,0,0), bg = '#22303C')
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, string, cex = 1.6, col = "#3282B8")
}

# Run everything
# --------------------------------------------------------
traffic_file_2019 <- "INWEVA_2019/A. Outputbestanden Excel/INWEVA_2019_weekdag_uren.xlsx"
roads_file <- "INWEVA_2019/B. Outputbestanden Shapes/Definitief_INWEVA_2019_Weekdag_uren_deel2.shp"

# Highway Exchanges. I chose these 4 because they are some of the busiest and are each next to one of the major cities, but feel free to choose others
knooppunten <- c('Kp Oudenrijn','Kp Prins Clausplein','Kp Ridderkerk-Noord','Kp Badhoevedorp')

# Uncomment one of these 4 depending on the traffic type you want
# traffic_type <- 'AL' #all vehicles
traffic_type <- 'L1' #personenauto's (cars)
# traffic_type <- 'L2' #middelzwaar vrachtverkeer (middleweight cargo traffic)
# traffic_type <- 'L3' #zwaar vrachtverkeer (heavy cargo traffic)

temp_list <- get_traffic_summary(knooppunten, final_traffic, traffic_file_2019, roads_file, traffic_type)
final_traffic_2019 <- temp_list[[1]]
list_kp_objects_2019 <- temp_list[[2]]

# Set different axes depending on the traffic type
if(traffic_type %in% c('AL','L1')) {
  y_axis_lim <- c(0,50)
  yaxis_marks <- c(0,25,50)
  yaxis_labels <- c('0','25k','50k')
} else if(traffic_type %in% c('L2')) {
  y_axis_lim <- c(0,5)
  yaxis_marks <- c(0,2.5,5)
  yaxis_labels <- c('0','2500','5000')
} else if(traffic_type %in% c('L3')) {
  y_axis_lim <- c(0,3)
  yaxis_marks <- c(0,1,2,3)
  yaxis_labels <- c('0','1k','2k','3k')
}
  
# Create an animation
animation::saveGIF(
  expr = {
    for(hour in 1:24){
      layout(matrix(c(1,3,2,3,
                      4,6,5,6,
                      7,9,8,9,
                      10,12,11,12), 8, 2, byrow = TRUE),
             widths=c(1,2), heights=c(1,1,1,1,1,1,1,1))

      for (i in c(1:4)){
        plot_name(knooppunten[i])
        plot_kp(list_kp_objects_2019[[i]])
        use_axis <- FALSE
        if(i==4){
          use_axis <- TRUE
        }
        # plot_chart(final_traffic[,i+1],24, use_axis)
        plot_chart(final_traffic_2019[,i+1], '#dca861', final_traffic_2019[,i+1], "#3282B8", hour, use_axis, y_axis_lim, yaxis_marks, yaxis_labels)
      }
    }
  },
  movie.name = "my_graphs_animation.gif"
)
