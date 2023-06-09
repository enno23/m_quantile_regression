# External documentation -------------------------------------------------------

#' Visualizes regional disaggregrated estimates on a map
#'
#' Function \code{map_plot} creates spatial visualizations of the estimates
#' obtained by small area estimation methods or direct estimation.
#'
#' @param object an object of type emdi, containing the estimates to be 
#' visualized.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean" "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count", 
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty" or 
#' "Inequality". Defaults to "all". Note, additional custom indicators can be 
#' defined as argument for estimation approaches (see also \code{\link{direct}}
#' and  \code{\link{ebp}}) and do not appear in groups of indicators even 
#' though these might belong to one of the groups.  
#' @param MSE optional logical. If \code{TRUE}, the MSE is also visualized. 
#' Defaults to \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the CV is also visualized. 
#' Defaults to \code{FALSE}.
#' @param map_obj a \code{SpatialPolygonsDataFrame} object as defined by the
#' \code{sp} package on which the data should be visualized.
#' @param map_dom_id a character string containing the name of a variable in
#' \code{map_obj} that indicates the domains. 
#' @param map_tab a \code{data.frame} object with two columns that match the 
#' domain variable from the census data set (first column) with the domain 
#' variable in the map_obj (second column). This should only be used if the IDs 
#' in both objects differ.
#' @param col a \code{vector} of length 2 defining the lowest and highest 
#' color in the plots.
#' @param scale_points a structure defining the lowest, the mid and the highest 
#' value of the colorscale. If a numeric vector of length two is given, this scale
#' will be used for every plot. Alternatively a list defining colors for each 
#' plot seperatly may be given. Please see the examples for this. 
#' @param return_data if set to \code{TRUE} a fortified data frame including the 
#' map data as well as the chosen indicators is returned. Customized can easily 
#' be obtained from this data frame via the package \code{ggmap}. 
#' Defaults to \code{FALSE}.
#' @return Creates the map plots demanded.
#' @seealso \code{\link{ebp}}, \code{\link{emdiObject}},
#' \code{\link[maptools]{readShapePoly}}
#' @examples 
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#' 
#' # generate emdi object with additional indicators; here via function ebp()
#' emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + 
#' self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
#' fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#' pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#' threshold = 11064.82, transformation = "box.cox", L = 50, MSE = TRUE, B = 50, 
#' custom_indicator = list( my_max = function(y, threshold){max(y)},
#' my_min = function(y, threshold){min(y)}), na.rm = TRUE, cpus = 1)
#' 
#' # Load shape file
#' load_shapeaustria()
#' 
#' # Create mapping table such that variables that indicate domains correspond
#' # in population data and shape file
#' mapping_table <- data.frame(unique(eusilcA_pop$district), 
#' unique(shape_austria_dis$NAME_2))
#'
#' # Example 1: Create map plot for mean indicator - point and MSE estimates 
#' # but no CV
#' map_plot(object = emdi_model, MSE = TRUE, CV = FALSE, 
#' map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2", 
#' map_tab = mapping_table)
#' 
#' # Example 2:
#' # Now we are creating plots for the Mean and the Median with both their precision
#' # measures, while forcing coloring ranges of both indicators to be the same
#' 
#' # First, define appropriate scales for each indicator and measure and save it 
#' # in a nested list as seen below
#' 
#' my_scale_points <- list(Mean   = list(ind = c(0, 75000),
#'                                     MSE = c(45000, 15000000),
#'                                     CV  = c(0, 0.5)
#'                                    ),
#'                         Median = list(ind = c(0, 75000),
#'                                       MSE = c(45000, 15000000),
#'                                       CV  = c(0, 0.5)
#'                                    )
#'                        )
#'  # When done so, this list may be used as an argument to map_plot 
#'  
#'  map_plot(object = emdi_model, MSE = TRUE, CV = TRUE, 
#'           map_obj = shape_austria_dis, indicator = c("Mean", "Median"), 
#'           map_dom_id = "NAME_2", map_tab = mapping_table, 
#'           scale_points = my_scale_points)
#'  
#'  # Example 3:
#'  # In the simple case, that all plots shall use the same color range,
#'  # the procedure from example 2 may be abbreviated to:
#'  map_plot(object = emdi_model, MSE = FALSE, CV = FALSE, 
#'           map_obj = shape_austria_dis, indicator = c("Mean", "Median"), 
#'           map_dom_id = "NAME_2", map_tab = mapping_table, 
#'           scale_points = c(0, 75000))
#' }
#' @export
#' @import rgeos maptools reshape2 

map_plot <- function(object,
                     indicator = "all",
                     MSE = FALSE,
                     CV = FALSE,
                     map_obj = NULL,
                     map_dom_id = NULL,
                     map_tab = NULL,
                     col = c("white", "red4"),
                     scale_points = NULL,
                     return_data = FALSE){

  
  if (is.null(map_obj))
  {
    message("No Map Object has been provided. An artificial polygone is used for
            visualization. For real visualization give an object of 
            class SpatialPolygonsDataFrame to the argument map_obj.")
    map_pseudo(object = object , indicator = indicator, panelplot = FALSE, MSE = MSE,
               CV = CV)
  }
  else if (!inherits(map_obj, "SpatialPolygonsDataFrame") || attr(class(map_obj),
                                                               "package") != "sp") {
    stop("map_obj needs to be of class SpatialPolygonsDataFrame from the sp package.")
  }
  else {

    
    mapplot_check(object = object, map_dom_id = map_dom_id, map_obj = map_obj, 
                  map_tab = map_tab, col = col, return_data = return_data)
    
    plot_real(object,
              indicator = indicator,
              MSE = MSE,
              CV = CV,
              map_obj = map_obj,
              map_dom_id = map_dom_id,
              map_tab = map_tab,
              col = col,
              scale_points = scale_points,
              return_data = return_data
    )
}
}

map_pseudo <- function(object, indicator, panelplot, MSE, CV)
{
  x <- y <- id <- value <- NULL #avoid note due to usage in ggplot
  values <-  estimators(object = object, indicator = indicator,
                                 MSE = MSE, CV = CV)$ind

  indicator <- colnames(values)[-1]

  tplot <- get_polygone(values = values)

  if (panelplot) {
    ggplot(tplot, aes(x = x, y = y)) + geom_polygon(aes(group = id, fill = value))
    + facet_wrap( ~ variable, ncol = ceiling(sqrt(length(unique(tplot$variable)))))
  } else {
    for (ind in indicator) {
      print(ggplot(tplot[tplot$variable == ind,], aes(x = x, y = y)) + 
              ggtitle(paste0(ind)) + geom_polygon(aes(group = id, fill = value)) )
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
}

plot_real <- function(object,
                      indicator = "all",
                      MSE = FALSE,
                      CV = FALSE,
                      map_obj = NULL,
                      map_dom_id = NULL,
                      map_tab = NULL,
                      col = col,
                      scale_points = NULL,
                      return_data = FALSE
) {
  

  

  long <- lat <- group <- NULL
  
  
  
  map_data <- estimators(object = object, indicator = indicator,
                                  MSE = MSE, CV = CV)$ind

  if (!is.null(map_tab)) {
    map_data <- merge(x = map_data, y = map_tab,  
                      by.x = "Domain", by.y = names(map_tab)[1])
    matcher <- match(map_obj@data[map_dom_id][,1], map_data[,names(map_tab)[2]])
    
    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop("Domains of map_tab and map_obj do not match. Check map_tab. 
             See also help(map_plot).")
      } else {
        warnings("Not all domains of map_tab and map_obj could be matched.
                  Check map_tab. See also help(map_plot).")
      }
    }
    map_data <- map_data[matcher,]
    map_data <- map_data[,!colnames(map_data) %in% c("Domain", 
                                                     map_dom_id, 
                                                     names(map_tab)), drop = F]
  } else {
    matcher <- match(map_obj@data[map_dom_id][,1], map_data[,"Domain"])

    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop("Domain of emdi object and map object do not match. 
             Try using map_tab. See also help(map_plot).")
      } else {
        warnings("Not all domains of emdi object and map object could be matched.
                 Try using map_tab. See also help(map_plot).")
      }
    }
    map_data <- map_data[matcher,]
  }

  map_obj@data[colnames(map_data)] <- map_data

  map_obj.fort <- fortify(map_obj, region = map_dom_id)
  map_obj.fort <- merge(map_obj.fort, map_obj@data, by.x = "id", by.y = map_dom_id)

  indicator <- colnames(map_data)
  for (ind in indicator) {
    map_obj.fort[ind][,1][!is.finite(map_obj.fort[ind][,1])] <- NA
    scale_point <- get_scale_points(map_obj.fort[ind][,1], ind, scale_points)
    
  if(MSE==T) midp=0.05  
  if(MSE==F) midp=0.5  
    
    
    if(MSE==T) limi=0.1  
    if(MSE==F) limi=1  
   plot <-  ggplot(map_obj.fort, aes(long, lat, group = group, fill = map_obj.fort[ind][,1])) +
            geom_polygon(color = "grey") + coord_equal() + labs(x = "", y = gsub(pattern = "_",replacement = " ",x = ind), fill = ind) +
          #  ggtitle(gsub(pattern = "_",replacement = " ",x = ind)) +
            scale_fill_gradient2(low = "green", mid = "red",
                                 high = "black", midpoint = midp, space = "rgb",
                                na.value = "grey50", guide = "colourbar", limits=c(0,limi)) +
            #scale_colour_gradientn(colours = terrain.colors(10), midpoint = 0.5, space = "rgb",               na.value = "grey50", guide = "colourbar")
            theme(axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_blank(), 
                  legend.title = element_blank())
    if (!ind == tail(indicator,1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
      return(plot)
  
}

get_polygone <- function(values)
{
  if (is.null(dim(values)))
  {
    values = as.data.frame(values)
  }
  n <- nrow(values)
  cols <- ceiling(sqrt(n))
  n <- cols^2

  values["id"] <- 1:nrow(values)

  poly <- data.frame(id = rep(1:n, each = 4),
                     ordering = seq_len((n*4)),
                     x = c(0,1,1,0) + rep(0:(cols - 1), each = (cols * 4)),
                     y = rep(c(0,0,1,1) + rep(0:(cols - 1), each = 4),cols)
  )

  combo <- merge(poly, values, by = "id", all = TRUE, sort = FALSE)
  melt(combo[order(combo$ordering),], id.vars = c("id","x","y","ordering"))
}

get_scale_points <- function(y, ind, scale_points){
  result <- NULL
  if (!is.null(scale_points)) {
    if (inherits(scale_points, "numeric") && length(scale_points == 2)) {
      result <- scale_points
    } else {
      splt <- strsplit(ind, "_\\s*(?=[^_]+$)", perl = TRUE)[[1]]
      indicator_name <- splt[1]
      if (length(splt) == 2) {
        measure <- splt[2]
      } else {
        measure <- "ind"
      }
      if (indicator_name %in% names(scale_points)) {
        pointset <- scale_points[[indicator_name]]
        try(result <- pointset[[measure]])
      }
      if (is.null(result) || length(result) != 2)
      {
        warnings("scale_points is of no appropriate form, default values will 
                 be used. See the second and third example in 
                 help(map_plot) for details.")
        result <- NULL
      }
    }
  }
  if (is.null(result)) {
    rg <- range(y, na.rm = T)
    result <- rg 
  }
  return(result)
}


