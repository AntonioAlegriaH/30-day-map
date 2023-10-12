# nyc street data

nyc_street_sf <- sf::read_sf("resources/NYC Street Centerline (CSCL)/geo_export_f6b5391a-2181-494b-93de-964e7f6c49bf.shp")
pryr::object_size(nyc_street_sf)
nyc_street_simp <- rmapshaper::ms_simplify(nyc_street_sf)
pryr::object_size(nyc_street_simp)
