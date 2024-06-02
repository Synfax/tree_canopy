mp <- function(x) {
  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(data = x)
}

mapthisline <- function(x) {
  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolylines(data = x)
}

st_parallel <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  
  split_results <- split(sf_df, split_vector) %>%
    #parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
    parallel::parLapply(cl = cl, ., function(x) sf_func(x, ...))
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2){
    output_class <- output_class[2]
  }
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix"){
    result <- do.call("rbind", split_results)
    names(result) <- NULL
  } else if (output_class == "sfc") {
    result <- do.call("c", split_results)
    result <- sf_func(result) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions. 
  } else if (output_class %in% c('list', 'sgbp') ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else if (output_class == "data.frame" ){
    result <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  # Return result
  return(result)
}

mapCoverage = function(sf, variable, palette = "Greens", title = "Tree Coverage (%)") {
  
  treePal <- colorNumeric(palette = palette, domain = variable)
  
  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(data = sf,
                fillColor = ~treePal(variable),
                fillOpacity = 0.5,
                opacity = 0) %>%
    addLegend(position = "bottomright",
              pal = treePal,
              values = variable,
              title = title) 
}
