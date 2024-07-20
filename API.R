library(plumber)


#Info endpoint
#* @get /info
function(){
  info <- paste("Taylor Cesarski", 
                        "https://tcesarski.github.io/ST558FinalProject/",
                        sep = " ")
  return(info)
}


#Find natural log of a number
#* @param num Number to find ln of
#* @get /ln
function(num){
  log(as.numeric(num))
}

#http://localhost:PORT/ln?num=40

#Find multiple of two numbers
#* @param num1 1st number
#* @param num2 2nd number
#* @get /mult
function(num1, num2){
  as.numeric(num1)*as.numeric(num2)
}

#http://localhost:PORT/mult?num1=10&num2=10

#* Plot histogram of iris data
#* @png
#* @param type base or GGally
#* @get /plotiris
function(type="base"){
  if(type == "GGally"){
    a<- ggpairs(iris)
    print(a)
  } else {
    pairs(iris)
  }
}

#http://localhost:PORT/plotiris?type=GGally


#* Plotting widget
#* @serializer htmlwidget
#* @param lat latitude
#* @param lng longitude
#* @get /map
function(lng = 174.768, lat = -36.852){
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(as.numeric(lng), as.numeric(lat))
  m  # Print the map
}

#query with http://localhost:PORT/map?lng=174&la