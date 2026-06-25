## AI functions for calculating plankton net volume sampled:

#General Oceanics:

k_m_per_rev <- 268733 / 999999

net_diameter_m <- 1
net_area_m2 <- pi * (net_diameter_m / 2)^2  # ≈ 0.785398

go_volume_m3 <- function(start, end,
                         k_m_per_rev = 268733 / 999999,
                         net_area_m2 = pi * (1 / 2)^2) {
  revs <- end - start
  distance_m <- revs * k_m_per_rev
  distance_m * net_area_m2
}

# Example:
# start <- 12345
# end   <- 13567
# vol_m3 <- go_volume_m3(start, end)
# vol_m3


# HydroBios

hydrobios_volume_m3 <- function(start, end, 
                                net_diameter_m,
                                pitch_m_per_rev = 0.3) {
  revs <- end - start
  net_area_m2 <- pi * (net_diameter_m / 2)^2
  revs * pitch_m_per_rev * net_area_m2
}

# Example for 1 m ring net
# hydrobios_volume_m3(start = 1234, end = 1450, net_diameter_m = 1)