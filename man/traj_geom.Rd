% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_util.R
\name{traj_geom}
\alias{traj_geom}
\alias{pts_traj}
\alias{coord_traj}
\title{Return a list of sf_POINTS or a data.frame from a sftraj object}
\usage{
pts_traj(traj)

coord_traj(traj)
}
\arguments{
\item{traj}{a trajectory geometery from sf_traj}
}
\description{
Return a list of sf_POINTS or a data.frame from a sftraj object
}
\examples{
raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
  # Input is a data.frame
my_traj <- as_sftraj(raccoon_data, time_col ='acquisition_time',
  error = NA, coords = c('longitude','latitude'),
  burst_list =burstz)
print(my_traj, 5, 10)

# extract a list of points
pts_traj(my_traj)

# or a data.frame of points
coord_traj(my_traj)
}
