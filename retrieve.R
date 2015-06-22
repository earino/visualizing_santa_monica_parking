library(RSocrata)
library(readr)

# This will take a LOT of time. It's > 1 million data points
parking_data <- read.socrata("https://data.smgov.net/Transportation/Santa-Monica-Parking-Lot-Counts/ng8m-khuz")

# Note that this is not compressed.
write_csv(parking_data, "parking.csv")
