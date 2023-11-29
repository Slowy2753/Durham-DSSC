library("ukpolice")
library("leaflet")
library("tidyverse")
library("sf")

forces <- ukc_forces()

dpc <- ukc_neighbourhoods("durham")
dpc_boundary <- ukc_neighbourhood_boundary("durham", "DHAM1")

dpc_boundary <- dpc_boundary |>
  mutate(latitude=as.numeric(latitude),
         longitude=as.numeric(longitude))

for (i in 1:length(dpc$id)){
  assign(paste0("Boundary_",dpc$id[i]),
         ukc_neighbourhood_boundary("durham", dpc$id[i])|>
           mutate(latitude=as.numeric(latitude),
                  longitude=as.numeric(longitude)))
}



leaflet() |> 
  addTiles() |> 
  addPolygons(lng = Boundary_BISH1$longitude, lat = Boundary_BISH1$latitude, label = "BISH1") |>
  addPolygons(lng = Boundary_BISH2$longitude, lat = Boundary_BISH2$latitude, label = "BISH2") |>
  addPolygons(lng = Boundary_BISH3$longitude, lat = Boundary_BISH3$latitude, label = "BISH3") |>
  addPolygons(lng = Boundary_BISH4$longitude, lat = Boundary_BISH4$latitude, label = "BISH4") |>
  
  addPolygons(lng = Boundary_CLS1$longitude, lat = Boundary_CLS1$latitude, label = "CLS1") |>
  addPolygons(lng = Boundary_CLS2$longitude, lat = Boundary_CLS2$latitude, label = "CLS2") |>
  addPolygons(lng = Boundary_CLS3$longitude, lat = Boundary_CLS3$latitude, label = "CLS3") |>
  addPolygons(lng = Boundary_CLS4$longitude, lat = Boundary_CLS4$latitude, label = "CLS4") |>
  addPolygons(lng = Boundary_CLS5$longitude, lat = Boundary_CLS5$latitude, label = "CLS5") |>
  addPolygons(lng = Boundary_CLS6$longitude, lat = Boundary_CLS6$latitude, label = "CLS6") |>
  
  addPolygons(lng = Boundary_CON1$longitude, lat = Boundary_CON1$latitude, label = "CON1") |>
  addPolygons(lng = Boundary_CON2$longitude, lat = Boundary_CON2$latitude, label = "CON2") |>
  addPolygons(lng = Boundary_CON3$longitude, lat = Boundary_CON3$latitude, label = "CON3") |>
  
  addPolygons(lng = Boundary_DALE1$longitude, lat = Boundary_DALE1$latitude, label = "DALE1") |>
  addPolygons(lng = Boundary_DALE2$longitude, lat = Boundary_DALE2$latitude, label = "DALE2") |>
  addPolygons(lng = Boundary_DALE3$longitude, lat = Boundary_DALE3$latitude, label = "DALE3") |>
  addPolygons(lng = Boundary_DALE4$longitude, lat = Boundary_DALE4$latitude, label = "DALE4") |>
  addPolygons(lng = Boundary_DALE5$longitude, lat = Boundary_DALE5$latitude, label = "DALE5") |>
  addPolygons(lng = Boundary_DALE6$longitude, lat = Boundary_DALE6$latitude, label = "DALE6") |>
  
  addPolygons(lng = Boundary_DAR1$longitude, lat = Boundary_DAR1$latitude, label = "DAR1") |>
  addPolygons(lng = Boundary_DAR2$longitude, lat = Boundary_DAR2$latitude, label = "DAR2") |>
  addPolygons(lng = Boundary_DAR3$longitude, lat = Boundary_DAR3$latitude, label = "DAR3") |>
  addPolygons(lng = Boundary_DAR4$longitude, lat = Boundary_DAR4$latitude, label = "DAR4") |>
  addPolygons(lng = Boundary_DAR5$longitude, lat = Boundary_DAR5$latitude, label = "DAR5") |>
  addPolygons(lng = Boundary_DAR6$longitude, lat = Boundary_DAR6$latitude, label = "DAR6") |>
  addPolygons(lng = Boundary_DAR7$longitude, lat = Boundary_DAR7$latitude, label = "DAR7") |>
  
  
  addPolygons(lng = Boundary_DHAM1$longitude, lat = Boundary_DHAM1$latitude, label = "DHAM1") |>
  addPolygons(lng = Boundary_DHAM2$longitude, lat = Boundary_DHAM2$latitude, label = "DHAM2") |>
  addPolygons(lng = Boundary_DHAM3$longitude, lat = Boundary_DHAM3$latitude, label = "DHAM3") |>
  addPolygons(lng = Boundary_DHAM4$longitude, lat = Boundary_DHAM4$latitude, label = "DHAM4") |>
  addPolygons(lng = Boundary_DHAM5$longitude, lat = Boundary_DHAM5$latitude, label = "DHAM5") |>
  addPolygons(lng = Boundary_DHAM6$longitude, lat = Boundary_DHAM6$latitude, label = "DHAM6") |>
  addPolygons(lng = Boundary_DHAM7$longitude, lat = Boundary_DHAM7$latitude, label = "DHAM7") |>
  addPolygons(lng = Boundary_DHAM8$longitude, lat = Boundary_DHAM8$latitude, label = "DHAM8") |>
  addPolygons(lng = Boundary_DHAM9$longitude, lat = Boundary_DHAM9$latitude, label = "DHAM9") |>
  addPolygons(lng = Boundary_DHAM10$longitude, lat = Boundary_DHAM10$latitude, label = "DHAM10") |>
  addPolygons(lng = Boundary_DHAM11$longitude, lat = Boundary_DHAM11$latitude, label = "DHAM11") |>
  addPolygons(lng = Boundary_DHAM12$longitude, lat = Boundary_DHAM12$latitude, label = "DHAM12") |>

  addPolygons(lng = Boundary_NEW1$longitude, lat = Boundary_NEW1$latitude, label = "NEW1") |>
  addPolygons(lng = Boundary_NEW2$longitude, lat = Boundary_NEW2$latitude, label = "NEW2") |>
  addPolygons(lng = Boundary_NEW3$longitude, lat = Boundary_NEW3$latitude, label = "NEW3") |>
  addPolygons(lng = Boundary_NEW4$longitude, lat = Boundary_NEW4$latitude, label = "NEW4") |>
  addPolygons(lng = Boundary_NEW5$longitude, lat = Boundary_NEW5$latitude, label = "NEW5") |>
  addPolygons(lng = Boundary_NEW6$longitude, lat = Boundary_NEW6$latitude, label = "NEW6") |>
  addPolygons(lng = Boundary_NEW7$longitude, lat = Boundary_NEW7$latitude, label = "NEW7") |>
  addPolygons(lng = Boundary_NEW8$longitude, lat = Boundary_NEW8$latitude, label = "NEW8") |>
  addPolygons(lng = Boundary_NEW9$longitude, lat = Boundary_NEW9$latitude, label = "NEW9") |>
  
  addPolygons(lng = Boundary_PLE1$longitude, lat = Boundary_PLE1$latitude, label = "PLE1") |>
  addPolygons(lng = Boundary_PLE2$longitude, lat = Boundary_PLE2$latitude, label = "PLE2") |>
  addPolygons(lng = Boundary_PLE3$longitude, lat = Boundary_PLE3$latitude, label = "PLE3") |>
  addPolygons(lng = Boundary_PLE4$longitude, lat = Boundary_PLE4$latitude, label = "PLE4") |>
  addPolygons(lng = Boundary_PLE5$longitude, lat = Boundary_PLE5$latitude, label = "PLE5") |>
  addPolygons(lng = Boundary_PLE6$longitude, lat = Boundary_PLE6$latitude, label = "PLE6") |>
  addPolygons(lng = Boundary_PLE7$longitude, lat = Boundary_PLE7$latitude, label = "PLE7") |>
  
  addPolygons(lng = Boundary_SEA1$longitude, lat = Boundary_SEA1$latitude, label = "SEA1") |>
  addPolygons(lng = Boundary_SEA2$longitude, lat = Boundary_SEA2$latitude, label = "SEA2") |>
  addPolygons(lng = Boundary_SEA3$longitude, lat = Boundary_SEA3$latitude, label = "SEA3") |>
  addPolygons(lng = Boundary_SEA4$longitude, lat = Boundary_SEA4$latitude, label = "SEA4") |>
  addPolygons(lng = Boundary_SEA5$longitude, lat = Boundary_SEA5$latitude, label = "SEA5") |>
  addPolygons(lng = Boundary_SEA6$longitude, lat = Boundary_SEA6$latitude, label = "SEA6") |>

  addPolygons(lng = Boundary_SPEN1$longitude, lat = Boundary_SPEN1$latitude, label = "SPEN1") |>
  addPolygons(lng = Boundary_SPEN2$longitude, lat = Boundary_SPEN2$latitude, label = "SPEN2") |>
  addPolygons(lng = Boundary_SPEN3$longitude, lat = Boundary_SPEN3$latitude, label = "SPEN3") |>
  addPolygons(lng = Boundary_SPEN4$longitude, lat = Boundary_SPEN4$latitude, label = "SPEN4") |>
  addPolygons(lng = Boundary_SPEN5$longitude, lat = Boundary_SPEN5$latitude, label = "SPEN5") |>
  addPolygons(lng = Boundary_SPEN6$longitude, lat = Boundary_SPEN6$latitude, label = "SPEN6") |>
  addPolygons(lng = Boundary_SPEN7$longitude, lat = Boundary_SPEN7$latitude, label = "SPEN7") |>
  addPolygons(lng = Boundary_SPEN8$longitude, lat = Boundary_SPEN8$latitude, label = "SPEN8") |>
  
  addPolygons(lng = Boundary_STAN1$longitude, lat = Boundary_STAN1$latitude, label = "STAN1") |>
  addPolygons(lng = Boundary_STAN2$longitude, lat = Boundary_STAN2$latitude, label = "STAN2") |>
  addPolygons(lng = Boundary_STAN3$longitude, lat = Boundary_STAN3$latitude, label = "STAN3") 
  

Boundary_DHAM1_R <- Boundary_DHAM1 |> 
  select(lat = latitude,
         lng = longitude)

Boundary_DHAM1_R <- Boundary_DHAM1_R[round(seq(1, nrow(Boundary_DHAM1_R), length.out = 100)), ]

crimes <- ukc_crime_poly(Boundary_DHAM1_R,"2023-7")

crimes <- crimes |>
  mutate(latitude=as.numeric(latitude),
         longitude=as.numeric(longitude))

leaflet() |> 
  addTiles() |> 
  addPolygons(lng = Boundary_DHAM1$longitude, lat = Boundary_DHAM1$latitude, label = "DHAM1") |>
  addCircles(lng=crimes$longitude, lat=crimes$latitude, label = crimes$category, col='red')
