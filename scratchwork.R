st <- filter(z1, statecode == 'ID') %>% arrange(year)

pc <- filter(eia_gps, state == 'ID')$plant_id %>% unique() %>% sort()

c1 <- filter(con1, plant_id %in% pc)
c2 <- filter(con2, plant_id %in% pc)
c3 <- filter(con3, plant_id %in% pc)
c4 <- filter(con4, plant_id %in% pc)
c5 <- filter(con5, plant_id %in% pc)

cx <- do.call(rbind, mget(str_c('c', 1:5)))

y <- filter(cx, year == 2013)
z <- filter(co2, year == 2013)
