#' sftrack Class
#' @title Sftrack Class
#' @description This is the highest level class that collects the error, time, and burst class.
#' It converts x,y,z data into an sftrack object and gives it an sf$geometry column,
#' and creates and error, time, and burst column as well of each respective class.
#'
#' @param data Data.frame input, these columns will remain unchanged, and any columns refered to
#' in later parameters are not deleted from this column. The function simply copies the data.frame
#' and adds the appropriate columns.
#' @param proj4 projection (for sf)
#' @param time vector of time
#' @param id vector of ids for the data
#' @param burst list of named vectors
#' @param error error vector
#' @param coords vector of three column names for the x,y,z coordinates, in that order.
#' @param tz timezone component, same as as.POSIX
#'
#' @import sf
#' @export new_sftrack
#' @examples
data(raccoon_data)

## sf/data.frame
rac_sfdf <- st_as_sf(raccoon_data, coords = c("longitude", "latitude", "height"), crs = 4326)
rac_sfdf$t <- as.POSIXct(rac_sfdf$acquisition_time, tz = "UTC")
rac_sfdf_track <- sftrack(data = rac_sfdf, burst = "sensor_code")
str(rac_sfdf_track)

## data.frame
rac_df <- raccoon_data
rac_df$t <- as.POSIXct(rac_df$acquisition_time, tz = "UTC")
rac_df_track <- sftrack(data = rac_df, coords = c("longitude", "latitude", "height"), burst = "sensor_code", crs = 4326)
str(rac_df_track)

## sf + vectors
rac_sf_vec_track <- sftrack(coords = rac_sfdf$geometry, timestamp = rac_sfdf$t, burst = rac_sfdf$sensor_code)
str(rac_sf_vec_track)

## data.frame of coordinates + vectors
rac_df_vec_track <- sftrack(coords = rac_df[, c("longitude", "latitude", "height")], timestamp = rac_df$t, burst = rac_df$sensor_code)
rac_df_vec_track2 <- sftrack(data = NULL, rac_df[, c("longitude", "latitude", "height")], rac_df$t, rac_df$sensor_code)
str(rac_df_vec_track)
all.equal(rac_df_vec_track, rac_df_vec_track2)

## sftrack itself
class(rac_sfdf_track)
names(rac_sfdf_track)
sft2 <- sftrack(rac_sfdf_track, burst = "sensor_code")
str(sft2)
all.equal(sft2, rac_sfdf_track)

sftrack <- function(data = NULL, coords, timestamp, burst, error = NULL, crs) {

    ## 1) 'data' inherits from 'sf' (and 'coords', 'timestamp', 'burst',
    ## 'error' are character) ✓
    ##
    ## 1.a) which means that if applied to an existing 'sftrack', it
    ## simply "cleans" it. ✓
    ##
    ## 1.b) 'data' inherits from 'sftraj' (conversion back to
    ## 'sftrack')
    ##
    ## 2) 'coords' inherits from sfc (and 'timestamp' is POSIXt, burst
    ## is vector, and error is?) ✓
    ##
    ## 3) 'data' inherits from data.frame (and 'coords', 'timestamp',
    ## 'burst', 'error' are character) ✓
    ##
    ## 4) 'coords' is data.frame , 'timestamp' is POSIXt, burst is
    ## vector, and error is? ✓
    ##
    ## 5) 'data' comes from other packages (ltraj, move, etc.)

    ## If <data> provided ('sf/data.frame' or 'data.frame')
    if (!is.null(data)) {
        ## Set defaults and POSIXt check
        ## <timestamp> is '"t"' if missing
        if (missing(timestamp))
            timestamp <- "t"
        ## <burst> is '"id"' if missing
        if (missing(burst))
            burst <- "id"
        ## $timestamp must be a POSIXt
        if (!inherits(data[[timestamp]], "POSIXt"))
            stop("The '\"timestamp\"' column must be a 'POSIXt'.")
        ## Check that the arguments <timestamp>, <burst>, <error> are
        ## actual columns from <data>
        arg_names <- c(timestamp, burst, error)
        if (!all(arg_names %in% names(data)))
            stop(paste0("\"", paste0(arg_names[which(!(arg_names %in% names(data)))], collapse = "\", \""), "\" need to be column(s) of 'data'"))
        ## If it is a 'sf/data.frame', set defaults, and check that it
        ## contains a <sfc_POINT>
        if (inherits(data, "sf")) {
            ## <coords> is '"geometry"' if missing
            if (missing(coords))
                coords <- "geometry"
            ## Check that <coords> is an actual column of <data>
            if (!(coords %in% names(data)))
                stop(paste0(coords, " needs to be a column of 'data'"))
            if (!inherits(data[[coords]], "sfc_POINT"))
                stop("'sf' column must be of class \"sfc_POINT\".")
        }
        ## Otherwise make sure that it is a 'data.frame', set
        ## defaults, and convert the DF to 'sf'
        else {
            ## Check that it is nevertheless a 'data.frame'
            if (!inherits(data, "data.frame"))
                stop("Data format not accepted. Check the documentation")
            ## <coords> is 'c("x", "y", "z")' if missing
            if (missing(coords))
                coords <- c("x", "y", "z")
            ## Check that <coord> is an actual column of <data>
            if (!all(coords %in% names(data)))
                stop(paste0("\"", paste0(coords[which(!(coords %in% names(data)))], collapse = "\", \""), "\" need to be column(s) of 'data'"))
            ## Create the "geometry" column (needs to handle cases
            ## where $geometry already exists)
            if (missing(crs)) {
                crs <- 4326
                warning("No CRS provided. Assuming 'longlat' by default (EPSG:4326).")
            }
            ## Note that 'dim' is automatically "XY" if only
            ## two dimensions, and "XYZ" if three dimensions
            ## (not necessary to specify)
            data <- sf::st_as_sf(data, coords = coords, remove = FALSE, na.fail = FALSE, crs = crs)
        }
    }
    ## If <data> is not NULL, vector mode. <coords> can be either a
    ## <data.frame> with two or three columns (coordinates) or a
    ## <sfc_POINT> column
    else {
        ## <data.frame> of coordinates
        if (inherits(coords, "data.frame")) {
            ## <timestamp> must be a POSIXt of length nrow(<coords>)
            if (!(inherits(timestamp, "POSIXt") & length(timestamp) == nrow(coords)))
                stop("With vector inputs, 'timestamp' must be a 'POSIXt' with a length equal to the number of rows of 'coords'.")
            ## <burst> must be a vector of length nrow(<coords>)
            if (length(burst) != nrow(coords))
                stop("With vector inputs, 'burst' must be a vector with a length equal to the number of rows of 'coords'.")
            ## <error> must be a vector of length nrow(<coords>)
            if (!is.null(error))
                if (length(error) != nrow(coords))
                    stop("With vector inputs, 'error' must be a vector with a length equal to the number of rows of 'coords'.")
            if (!(ncol(coords) %in% 2:3))
                stop("A <data.frame> of spatial coordinates must have 2 or 3 dimensions.")
            data <- sf::st_as_sf(data.frame(coords, t = timestamp, burst = burst), coords = names(coords), remove = FALSE, na.fail = FALSE, crs = crs)
        }
        else if (inherits(coords, "sfc_POINT")) {
            ## <timestamp> must be a POSIXt of length n
            if (!(inherits(timestamp, "POSIXt") & length(timestamp) == length(coords)))
                stop("With vector inputs, 'timestamp' must be a 'POSIXt' with the same length as 'coords'.")
            ## <burst> must be a vector of length n
            if (length(burst) != length(coords))
                stop("With vector inputs, 'burst' must be a vector with the same length as 'coords'.")
            ## <error> must be a vector of length n
            if (!is.null(error))
                if (length(error) != length(coords))
                    stop("With vector inputs, 'error' must be a vector with the same length as 'coords'.")
            data <- data.frame(geometry = coords, t = timestamp, burst = burst)
        }
        else
            stop("Data format not accepted. Check the documentation")
        ## Add the error term if it exists
        if (!is.null(error))
            data[["error"]] <- error
        ## And we set the variables names for timestamp, burst, and
        ## error in the function environment
        timestamp <- "t"
        burst <- "burst"
        error <- "error"
    }

    ## Remove duplicates (burst×timestamp)
    # data <- remove_time_dup(data)

    ## Order data by (burst×timestamp)
    # data <- order_traj(data)

    ## Build 'sftrack' object
    new_sftrack(data = data, burst = burst, timestamp = timestamp, error = error)
}

new_sftrack <- function(data, burst, timestamp, error) {
    ## Prepare burst column
    # data <- make_burst(data)

    ## Does <data> inherits from '"tbl"'?
    if (inherits(data, "tbl"))
        tbl <- c("tbl_df", "tbl")
    else tbl <- NULL

    structure(data,
        burst = burst,
        timestamp = timestamp,
        error = error,
        class = c("sftrack", "sf", tbl, "data.frame")
    )
}



#'  data(raccoon_data)
#'  burstz <- list( id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#' my_track <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),
#'   error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#'   burst =burstz)

new_sftrack.ori <-
  function(data = data.frame(),
    proj4 = NA,
    time = NA,
    burst = NULL,
    error = NA,
    coords = c('x','y','z'),
    tz = NULL,
    active_burst = 'id'
  ) {

    # Make multi burst
    mb <- make_multi_burst(burst=burst, active_burst = active_burst)
    time_tj <- new_time_tj(time,id=burst$id,tz=tz)
    if(is.na(error)){error <- rep(NA, nrow(data))}
    # Order data frame for later
    torder <- do.call(order,append(burst[active_burst], list(time)))
    # id label should be the first mentioned in the burst
    active_burst <- c('id',active_burst[active_burst!='id'])
    new_data <- data.frame(
      traj_id = NA,
      data,
      time = time_tj,
      burst = mb,
      error = new_error_tj(error)
    )
    new_data <- new_data[torder,]
    traj_id <- seq_len(nrow(new_data))
    new_data$traj_id = traj_id

    structure(
      sf::st_as_sf(
        new_data,
        coords = coords,
        dim = 'XYZ',
        na.fail = FALSE
      ),
      projection = proj4,
      active_burst = active_burst,
      class = c("sftrack", "sf",'data.frame')
    )
  }

#' @export
print.sftrack <- function(x,...){
  x <- as.data.frame(x)
  cat('This is an sftrack object\n')
  cat(paste0('proj : ',attr(x,'projection'),'\n'))
  cat(paste0('unique ids : ', paste(unique(sapply(x$burst, function(x) x$id)),collapse=', '), '\n'))
  cat(paste0('bursts : total = ', length(x$burst[[1]]),' | active burst = ',paste0(attr(x, 'active_burst'),collapse=', '), '\n'))
  n <- ifelse(nrow(x)>10,10,nrow(x))
  row_l <- length(!colnames(x)%in%c('time','burst','error','geometry'))
  p <- ifelse(row_l>6,6,row_l)
  cat(paste("First", n, "features w/",p+4, "truncated columns:\n"))
  if(ncol(x)>10){
  y <- cbind(x[1:n,colnames(x)[1:p]],
    data.frame('...' = rep('...',n)),
    x[1:n,c('time','burst','error','geometry')])
  } else y <- x
  print.data.frame(y, ...)
}
# ################################################
# # Test bed
# library(sf)

## Test data set
# df1 <- read.csv('/home/matt/Documents/sftrack/data_raccoon.csv')
# colnames(df1)[colnames(df1)=='latitude'] <- 'y'
# colnames(df1)[colnames(df1)=='longitude'] <- 'x'
#df1$m <- as.numeric(as.POSIXlt(df1$acquisition_time))
#df1$z <- df1$height
# # sf1 <- new_sftrack(df1[,], proj4 = 4236, time = df1$acquisition_time, time_column = 'acquisition_time')
# # sf1

#' @export step2track
step2track <-    function(my_step){

  geometry <- my_step$geometry

  new_geom <- lapply(geometry, function(x){
    if(c('GEOMETRYCOLLECTION')%in%class(x)){
      return(st_point(unlist(x)[1:3], dim = 'XYZ'))
    }
    if(c('LINESTRING')%in%class(x)){
      return(st_point(x[c(1,3,5)], dim = 'XYZ'))
    }
  })
  my_step$geometry <- st_sfc(new_geom)

  structure(
    data_sf1 <- sf::st_sf(
      my_step,
      sf_column_name='geometry'
    ),
    active_burst = attr(my_step, 'active_burst'),
    projection = attr(my_step, 'projection'),
    class = c("sftrack", 'sf','data.frame')
  )
}
