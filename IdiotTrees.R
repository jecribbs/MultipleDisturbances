# Idiot Trees Script

source("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Scripts/0_TreePositionCalculationFunctions.R")

relativeTreeCalculation <- function(start_E, start_N, bearing, distance) {
  hypotenuse = distance  # Calculate hypotenuse
  # Convert azimuth from magnetic to true and degrees to radians
  bearing_rad = (bearing + 12.5)  * pi / 180 
  x_offset = hypotenuse * sin(bearing_rad) # Calculate x offset
  y_offset = hypotenuse * cos(bearing_rad) # Calculate y offset
  tree_UTM_E = start_E + x_offset  # Final easting
  tree_UTM_N = start_N + y_offset   # Final northing
  print(list(tree_UTM_E = tree_UTM_E, tree_UTM_N = tree_UTM_N))
} 

# plot 50 tree 1
relativeTreeCalculation(0763102, 4188809, 8, 121.3)
relativeTreeCalculation(0763102, 4188809, 26, 251)
relativeTreeCalculation(0763102, 4188809, 29, 257)
relativeTreeCalculation(0763102, 4188809, 36, 152)
