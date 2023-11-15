library(tidyverse)
library(maps)
library(nycflights13)

#5.65
View(band_members)
band_instruments
View(band_instruments2)

left_join(band_instruments, band_members)
left_join(band_members, band_instruments2 |>
            select(name=artist,plays))

inner_join(band_instruments, band_members)

#5.66
left_join(band_members, band_instruments2, by=c("name"="artist"))

#5.67
ggplot(data=mpg, aes(displ,hwy))

#5.86
ggplot(data=mpg, aes(displ,hwy))+
  geom_point(aes(col=drv))+
  geom_smooth(col='black')+
  geom_smooth(aes(col=drv))

#5.69
ggplot(data=mpg, aes(class))+
  geom_bar(aes(fill=drv))

#5.70

#5.71
#Creates a df of the different routes that aircraft
#flew and how many aircraft flew that route over the year

usa <- map_data("usa")
ggplot() +
  geom_polygon(aes(x = long, y = lat), data = usa, fill = "darkgreen")

#5.72
max(usa$long)
min(usa$long)

routes <- flights |> 
  group_by(dest) |> 
  summarise(n = n())

airports2 <- left_join(airports, routes, by = c("faa" = "dest"))

airports2$n[is.na(airports2$n)] <- 0

airports2_usa <- airports2 |> 
  filter(lon > min(usa$long) & lon < max(usa$long))

#5.73
ggplot() +
  geom_polygon(aes(x = long, y = lat), data = usa, fill = "darkgreen")+
  geom_point(aes(x = lon, y = lat),data=airports2_usa)

#5.74


ggplot() +
  geom_polygon(aes(x = long, y = lat), data = usa, fill = "darkgreen")+
  geom_point(aes(x = lon, y = lat, size=(0.5)*log(n+1)),data=airports2_usa)

