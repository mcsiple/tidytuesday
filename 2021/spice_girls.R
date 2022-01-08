# Spice girls


# Color palettes and themes -----------------------------------------------
# Palettes from Spice Girls album covers, made on coolors.co
spice_pal <- c("EF4035","FD7D34","9A351D","88D200","f9ea9a","EDB2CB","FFFFFF","BE525F","CDE009","A9973E")
spiceworld_pal <- c("B0A7B4", "FF2332", "07047C", "FF1085", "B6E6D1", "E8F650", "00A8D1", "FFB463")


# Data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-12-14')
studio_album_tracks <- tuesdata$studio_album_tracks
lyrics <- tuesdata$lyrics