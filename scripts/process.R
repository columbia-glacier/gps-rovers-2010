library(magrittr)

# ---- Functions ----

read_edgee <- function(file) {
  # Read GrafNav file
  cols <- file %>%
    readLines(n = 21) %>%
    extract(21) %>%
    strsplit(split = "[ ]+") %>%
    unlist()
  edgee <- file %>%
    read.table(skip = 22, stringsAsFactors = FALSE) %>%
    set_names(cols)
  # Keep duplicates (by time) with highest quality (lowest Q)
  is_duplicate <- edgee[, c("Date", "GPSTime")] %>%
    {duplicated(.) | duplicated(., fromLast = TRUE)}
  deduplicates <- edgee[is_duplicate, ] %>%
   dplyr::group_by(Date, GPSTime) %>%
    dplyr::do({
      if (nrow(.) > 1) {
        ind <- which(.$Q == min(.$Q))[1]
        .[ind, ]
      } else {
        .
      }
    })
  dplyr::bind_rows(
    edgee[!is_duplicate, ],
    deduplicates
  ) %>%
    dplyr::arrange(Date, GPSTime)
}

# ---- Parsers ----

edgee_parsers <- list(
  track = function(track) {
    track %>%
      dpkg::set_field(description = "Track identifier (rover_id.track#)")
  },
  t = function(Date, GPSTime) {
    date <- format(strptime(Date, "%m/%d/%Y"), "%Y-%m-%d")
    time <- format(strptime(GPSTime, "%H:%M:%S"), "%H:%M:%S")
    as.POSIXct(paste(date, time), tz = "UTC")
  },
  x = function(Easting) {
    Easting %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Easting (WGS84 UTM Zone 6N, EPSG:32606)")
  },
  y = function(Northing) {
    Northing %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Northing (WGS84 UTM Zone 6N, EPSG:32606)")
  },
  elevation = function(`H-Ell`) {
    `H-Ell` %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Elevation (height above the WGS84 ellipsoid)")
  },
  xy_sd = function(SDHoriz) {
    SDHoriz %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Standard deviation in x-y plane")
  },
  elevation_sd = function(SDHeigh) {
    SDHeigh %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Standard deviation in elevation")
  },
  quality = function(Q) {
    Q %>%
      dpkg::set_field(description = "GrafNav position quality metric (smaller number = higher quality)")
  }
)

# ---- NETRS ----

# NOTE: Data manually pulled from columbia_netrs_2010.m (get_data).
netrs <- c(
  n1 = "sources/columbia_netrs_2010_n1.csv",
  n2 = "sources/columbia_netrs_2010_n2.csv",
  n3 = "sources/columbia_netrs_2010_n3.csv"
) %>%
  lapply(read.csv) %>%
  dplyr::bind_rows(.id = "track") %>%
  dplyr::transmute(
    # Split tracks at dig-out on day 137 (Ian Howat, email: 2010-10-29)
    track = track %>%
      paste0(ifelse(t <= 137, ".1", ".2")) %>%
      dpkg::set_field(description = "Track identifier (rover_id.track#)"),
    # Convert day number (2010) to ISO 8601 date
    # NOTE: Checked against columbia_edgee_2010.m (dayofyear).
    t = as.Date(t, origin = as.Date("2009-12-31")) %>%
      dpkg::set_field(description = "Date (time and zone unknown)"),
    x = x %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Easting (WGS84 UTM Zone 6N, EPSG:32606)"),
    y = y %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Northing (WGS84 UTM Zone 6N, EPSG:32606)"),
    elevation = z %>%
      units::set_units("m") %>%
      dpkg::set_field(description = "Elevation (height above the WGS84 ellipsoid)")
  )

# ---- EDGEE ----

edgee <- c(
  s1r1 = "sources/S1R1 new.txt",
  s1r2 = "sources/S1R2 new.txt",
  s1r3 = "sources/S1R3 new.txt",
  s2r1 = "sources/S2R1 new.txt",
  s2r2 = "sources/S2R2 new.txt",
  s2r3 = "sources/S2R3 new.txt"
) %>%
  lapply(read_edgee) %>%
  dplyr::bind_rows(.id = "track") %>%
  cgr::parse_table(edgee_parsers) %>%
  dplyr::arrange(track, t)

# Split tracks at relocations
# NOTE: Indice breaks adjusted from those in columbia_edgee_2010.m (columbia_edgee_2010). Assumes rows sorted and unique in time.
breaks <- list(
  s2r1 = c(2217),
  s2r2 = c(3441, 3756),
  s2r3 = c(1919, 2195)
)
for (i in seq_along(breaks)) {
  ind <- edgee$track == names(breaks)[i]
  intervals <- findInterval(seq_len(sum(ind)), breaks[[i]]) + 1
  edgee$track[ind] %<>%
    paste(intervals, sep = ".")
}

# Remove extreme outliers
outliers <- list(
  s2r2.1 = as.POSIXct(c("2010-05-14 03:14:00"), tz = "UTC"),
  s2r3.3 = as.POSIXct(c("2010-05-17 19:26:30"), tz = "UTC")
)
for (i in seq_along(outliers)) {
  edgee %<>%
    dplyr::filter(!(track == names(outliers[i]) & t %in% outliers[[i]]))
}

# ---- Data Package ----

dp <- list(
  netrs = netrs %>%
    dpkg::set_resource(
      name = "netrs",
      title = "Upper Glacier Rover Positions (NETRS)",
      description = "Daily positions of three rovers above Divider Mountain, processed using PPP from NETRS data. Positions for each rover are split into two tracks due to the dig-out on 2010-05-17.",
      path = "data/netrs.csv",
      profile = "tabular-data-resource"
    ),
  edgee = edgee %>%
    dpkg::set_resource(
      name = "edgee",
      title = "Lower Glacier Rover Positions (EDGEE)",
      description = "30-second positions for three rovers below Divider Mountain, processed from EDGEE data using GrafNav. Positions for some rovers are split into multiple tracks due to relocations.",
      path = "data/edgee.csv",
      profile = "tabular-data-resource"
    )
) %>%
  dpkg::set_package(
    name = "gps-rovers-2010",
    title = "GPS Rover Positions (2010)",
    description = "Daily to 30-second positions of six GPS rovers placed on the glacier surface in 2010.",
    version = "0.1.0",
    contributors = list(
      dpkg::contributor("Ethan Welty", role = "author", email = "ethan.welty@gmail.com"),
      dpkg::contributor("Ian Howat", role = "Coordinated the research"),
      dpkg::contributor("Shad O'Neel", role = "Assisted the research"),
      dpkg::contributor("Alberto Behar", role = "Designed and built the GPS rovers"),
      dpkg::contributor("Julie Markus", role = "Processed the NETRS positions"),
      dpkg::contributor("Phaedra Tinder", role = "Processed the EDGEE positions")
    ),
    sources = list(
      dpkg::source("Original data, scripts, and documentation", path = "sources/")
    )
  )

dpkg::write_package(dp)