
cars <- read.table(file.choose(), header = TRUE)
head(cars)

library(dplyr)
temp <- dplyr::select(.data = cars, 
                      Transmission,
                      Cylinders,
                      Fuel.Economy)

head(temp)

## Filetr only Automatic
temp <- dplyr::filter(.data = temp,
                      Transmission == 'Automatic')
head(temp)

## Convert fuel economy from US Miles per galon to metric equivalent kms/leader
## Computer a new column
temp <- dplyr::mutate(.data = temp,
                      Consumption = Fuel.Economy * 0.425)

head(temp)

## Group by number of cylinders

temp <- dplyr::group_by(.data = temp,
                        Cylinders)
head(temp)


## Aggregate based on Groups

temp <- dplyr::summarise(.data = temp,
                         Avg.Consumption = mean(Consumption))

head(temp)

## Sort the data by Avg: Most Fuel efficient on top

temp <- dplyr::arrange(.data = temp,
                       desc(Avg.Consumption))

head(temp)

## Convert the tibble to data frame

efficient <- as.data.frame(temp)
head(efficient)

## Achieving the same with piping/ chaining methods

efficiency <- cars %>% 
              dplyr::select(Transmission, Cylinders, Fuel.Economy) %>%
              dplyr::filter(Transmission == 'Automatic') %>%
              dplyr::mutate(Consumption = Fuel.Economy * 0.425) %>%
              dplyr::group_by(Cylinders) %>%
              dplyr::summarise(Avg.Consumption = mean(Consumption)) %>%
              dplyr::arrange(desc(Avg.Consumption)) %>%
              as.data.frame()

head(efficiency)

## Writing these results to a CSV

write.csv(x = efficiency, file = "Fuel Efficiency.csv", row.names = FALSE) ## Dont want to write the unique ID for each row to our CSV, row.names = FALSE

