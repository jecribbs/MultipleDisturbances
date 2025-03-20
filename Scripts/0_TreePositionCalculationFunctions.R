
# Tree Position Calculation Functions

# Code Description
# function to calculate all tree positions based on the plot beg and relative positions (dOut_m and dSide_final)

# Inputs
# function takes the whole dataframe as a single argument 
# dOut_m is the distance out along the plot transect
# dSide_final is the distance perpendicular to the transect
# plot_azimuth is the magnetic bearing from North as read off the compass without adjustment for declination
# the 12.5 is the declination adjustment for Yosemite in 2023--modify this for other regions of years
# plot beginning eastings and northings are in UTMs

# Output
# an easting and northing for each tree in the dataframe (tree_UTM_E, tree_UTM_N)

# rename columns

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

relativeTreeCalculation(0271554, 4182436, 262, 900.9)

# calculate UTM positions for each item in a dataframe
calculate_tree_positions <- function(data) {
  
  calculatedUTMs <- data %>%
    mutate(
      hypotenuse = sqrt(dOut_final^2 + dSide_final^2),  # Step 1: Calculate hypotenuse
      angle_dSide = atan2(dSide_final, dOut_final),     # Step 2: Calculate angle opposite Dside
      azimuth_rad = (plot_azimuth + 12.5)  * pi / 180,         # Convert azimuth from magnetic to true and degrees to radians
      adjusted_angle = azimuth_rad + angle_dSide, # Step 3: Adjust azimuth by angle_Dside
      x_offset = hypotenuse * sin(adjusted_angle), # Step 4: Calculate x offset
      y_offset = hypotenuse * cos(adjusted_angle), # Step 4: Calculate y offset
      tree_UTM_E = plot_beg_UTM_E + x_offset,  # Step 5: Final easting
      tree_UTM_N = plot_beg_UTM_N + y_offset   # Step 5: Final northing
    )
  return(calculatedUTMs)
}
# calculate a tree position for a single tree relative to another point (start_E, start_N) with distance and bearing
# this was useful for a handful of trees with weird notes
# takes 4 arguments
  # 1. start easting in UTMs
  # 2. start northing in UTMs
  # 3. the magnetic bearing from the reference point to the target in degrees
  # 4. the distance from the reference point to the target
  
relativeTreeCalculationPlot37 <- function(start_E, start_N, bearing, distance) {
  hypotenuse = distance  # Calculate hypotenuse
  # Convert azimuth from magnetic to true and degrees to radians
  bearing_rad = (bearing)  * pi / 180 
  x_offset = hypotenuse * sin(bearing_rad) # Calculate x offset
  y_offset = hypotenuse * cos(bearing_rad) # Calculate y offset
  tree_UTM_E = start_E + x_offset  # Final easting
  tree_UTM_N = start_N + y_offset   # Final northing
  print(list(tree_UTM_E = tree_UTM_E, tree_UTM_N = tree_UTM_N))
} 

relativeTreeCalculationPlot37(0247726, 4193157, 77, 200)
