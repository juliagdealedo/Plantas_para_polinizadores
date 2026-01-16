crear_ficha <- function(data = data,
                                   path = NULL,
                                   filename = NULL,
                                   title = NULL,
                                   subtitle = NULL,
                                   qr = NULL,
                                   family.column = NULL,
                                   taxon.column = NULL,
                                   species.column = NULL,
                                   genus.column = NULL,
                                   icon4.column = NULL,
                                   vernaculo.column = NULL,
                                   icon1.column = NULL,
                                   icon2.column = NULL,
                                   icon3.column = NULL,
                                   suelo.column = NULL,
                                   color.column = NULL,
                                   floracion.column = NULL,
                                   imagen.column = NULL,
                                   habito.column = NULL,
                                   clima.column = NULL,
                                   lpic = NULL,
                                   rpic = NULL,
                                   fpic = NULL,
                                   mpic=NULL,
                                   credito.column = NULL,
                                   keep.files = FALSE,
                                   template = NULL
){

  ## Check arguments

  if (is.null(data)) {
    stop("Please provide a data.frame or tibble.")
  }
  if ((!(all(class(data) == "data.frame"))) & any(class(data) == "data.frame")) {data <- as.data.frame(data)}
  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}
  data <- fill_NAs_df(data)

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  if (is.null(filename)) {
    message("No file name provided")
    filename <- "Herbarium"
  }


  if (any(apply(data, 1, nchar) > 150)) {
    message("Too long texts may give undesired results. Please consider shortening long fields.")
  }


  if (is.null(title)) {
    message("No title provided")
    title <- ""
  }

  if (is.null(subtitle)) {
    message("No subtitle provided")
    subtitle <- ""
  }


  ## QR code

  if (!is.null(qr)) {
    stopifnot(is.character(qr))
    # If qr is not a column in data, use same qr for all items
    if (!(qr %in% colnames(data))) {
      data$qr <- qr   # recycling to all rows in data
      qr <- "qr"    # used later for selecting column
    }
  }
  if (is.null(qr)) {
    message("No qr provided")
    data$qr <- ""
    qr <- "qr"    # used later for selecting column
  }




  if (!is.null(family.column)) {
    check_column_in_df(data, family.column)
    data[,family.column] <- toupper(data[,family.column])
  }



  ## Check columns are in data or create empty characters if NULL

  family.column           <- check_column_or_create_empty_char(data, family.column)
  taxon.column            <- check_column_or_create_empty_char(data, taxon.column)
  species.column            <- check_column_or_create_empty_char(data, species.column)
  genus.column            <- check_column_or_create_empty_char(data, genus.column)
  icon4.column           <- check_column_or_create_empty_char(data, icon4.column)
  vernaculo.column        <- check_column_or_create_empty_char(data, vernaculo.column)
  icon1.column             <- check_column_or_create_empty_char(data, icon1.column)
  icon2.column          <- check_column_or_create_empty_char(data, icon2.column)
  icon3.column         <- check_column_or_create_empty_char(data, icon3.column)
  suelo.column          <- check_column_or_create_empty_char(data, suelo.column)
  color.column             <- check_column_or_create_empty_char(data, color.column)
  floracion.column        <- check_column_or_create_empty_char(data, floracion.column)
  imagen.column        <- check_column_or_create_empty_char(data, imagen.column)
  habito.column           <- check_column_or_create_empty_char(data, habito.column)
  clima.column           <- check_column_or_create_empty_char(data, clima.column)
  credito.column            <- check_column_or_create_empty_char(data, credito.column)


  arguments <- c(family.column           ,
                 taxon.column            ,
                 species.column,
                 genus.column,
                 icon4.column           ,
                 vernaculo.column        ,
                 icon1.column             ,
                 icon2.column          ,
                 icon3.column         ,
                 suelo.column          ,
                 color.column             ,
                 floracion.column        ,
                 imagen.column        ,
                 habito.column           ,
                 clima.column           ,
                 credito.column            )
  arguments <- arguments[arguments != ""]

  data <- check_latex_columns(data, arguments)





  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }



  #### Defining Rmd template to use ####

  if (is.null(template)) { # use pkg default
    file.copy(
      from = system.file("rmarkdown/templates/herbarium/skeleton/skeleton.Rmd", package = "labeleR"),
      to = file.path(folder, "herbarium.Rmd"),
      overwrite = TRUE
    )
  }

  if (!is.null(template)) {
    stopifnot(file.exists(template))
    if (template != file.path(folder, "herbarium.Rmd")) {
      file.copy(
        from = template,
        to = file.path(folder, "herbarium.Rmd"),
        overwrite = TRUE
      )
    }
  }

  #### Logos ####

   use_image(lpic, name = "lpic", folder = folder)
   use_image(rpic, name = "rpic", folder = folder)
   use_image(fpic, name = "fpic", folder = folder)
   use_image(mpic, name = "mpic", folder = folder)
   
# # 
#   #### Mapas #####
# 
#   species <- data[,taxon.column]
# 
# 
#   for(sp in species){
# 
#     sp <- gsub(" ", "_", sp)
#     sp <- gsub("\\.", "",sp)
# 
#     if(file.exists(paste0("maps/results/europe/", sp, ".png"))){next}else{
#       use_image("maps/results/europe/blank.png", name=paste0(sp), "maps/results/europe")
#     }
#   }
# 
#   for(sp in species){
# 
#     sp <- gsub(" ", "_", sp)
# 
#     if(file.exists(paste0("maps/results/iberia/", sp, ".png"))){next}else{
#       use_image("maps/results/iberia/blank.png", name=paste0(sp), "maps/results/iberia")
#     }
#   }
# 
#   fs::dir_copy("maps",folder)

  #
  # mapeurope <- paste0("maps/results/europe/", species[1] , ".png") #se especifica el archivo del nombre del mapa
  # if (! file.exists(mapeurope )){mapeurope <- NULL} #si el mapa no existe, que saque un blanco
  # use_image(mapeurope, "mapeurope.png", folder = folder) #crea la imagen en el dir temporal
  #
  # mapiberia <- paste0("maps/results/iberia/", species[1] , ".png")
  # if (! file.exists(mapiberia )){mapiberia <- NULL}
  # use_image(mapiberia, "mapiberia.png", folder = folder)





  #### Render #####

  data <- as.data.frame(data) ## to exploit drop = TRUE when selecting cols below
   for (i in 1:nrow(data)) {
    out.name <- ""
    out.name <- paste0(data[i, family.column],"_",gsub(" ", "_",data[i, taxon.column]))
    output_file <- paste0(out.name,'.pdf')

    bl.char <- "~"


  rmarkdown::render(
    input = file.path(folder, "herbarium.Rmd"),
    output_dir = path,
    output_file = output_file,
    params = list(
      qr.i = data[i, qr],
      title              = if (title                   == "") {bl.char} else {title},
      subtitle           = if (subtitle                == "") {bl.char} else {subtitle},
      family.i           = if (family.column           == "") {bl.char} else {data[i,family.column]},
      taxon.i            = if (taxon.column            == "") {bl.char} else {data[i,taxon.column]},
      species.i            = if (species.column            == "") {bl.char} else {data[i,species.column]},
      genus.i            = if (genus.column            == "") {bl.char} else {data[i,genus.column]},
      icon4.i           = if (icon4.column           == "") {bl.char} else {data[i,icon4.column]},
      vernaculo.i        = if (vernaculo.column        == "") {bl.char} else {data[i,vernaculo.column]},
      icon1.i             = if (icon1.column             == "") {bl.char} else {data[i,icon1.column]},
      icon2.i          = if (icon2.column          == "") {bl.char} else {data[i,icon2.column]},
      icon3.i         = if (icon3.column         == "") {bl.char} else {data[i,icon3.column]},
      suelo.i          = if (suelo.column          == "") {bl.char} else {data[i,suelo.column]},
      color.i             = if (color.column             == "") {bl.char} else {data[i,color.column]},
      floracion.i        = if (floracion.column        == "") {bl.char} else {data[i,floracion.column]},
      imagen.i        = if (imagen.column        == "") {bl.char} else {data[i,imagen.column]},
      habito.i           = if (habito.column           == "") {bl.char} else {data[i,habito.column]},
      clima.i           = if (clima.column           == "") {bl.char} else {data[i,clima.column]},
      credito.i            = if (credito.column            == "") {bl.char} else {data[i,credito.column]}
    )
  )

}

}



#### Change special LaTeX symbols

check_latex<- function(df = NULL, column=NULL){
  df[[column]] <- gsub("&", "\\\\&", df[[column]])
  df[[column]] <- gsub("%", "\\\\%", df[[column]])
  # df[,column] <- gsub("$", "\\$", df[,column])#added to all texts at the end
  df[[column]] <- gsub("#", "\\\\#", df[[column]])
  df[[column]] <- gsub("_", "\\\\_", df[[column]]) #document names get truncated
  # df[,column] <- gsub("{", "\\{", df[,column]) #document names get truncated
  df[[column]] <- gsub("}", "\\\\}", df[[column]])
  df[[column]] <- gsub("~", "-"  , df[[column]]) #unable to render with \\~
  df[[column]] <- gsub("\\^", "\\\\^"  , df[[column]])#name truncated and accent to the next letter
  return(df[,column, drop=FALSE])
}

check_latex_columns <- function(df= NULL, columns= NULL){
  for (column in columns) {
    df[,column] <- check_latex(df, column)
  }
  return(df)
}

na2blank <- function(x){
  x[is.na(x)] <- "~"
  return(x)}

fill_NAs_df <- function(df = NULL){
  cols <- colnames(df)
  rows <- rownames(df)
  mat <- matrix(apply(df, 2, na2blank), nrow = length(rows), ncol=length(cols))
  df <- as.data.frame(mat)
  colnames(df) <- cols
  rownames(df) <- rows

  return(df)
}


#### Check columns in data

check_column_in_df <- function(df = NULL, column = NULL) {

  stopifnot(is.character(column))
  if (!(column) %in% colnames(df)) {
    stop("Column '", column ,
         " is not a column in your 'data' object. Please select from \n",
         paste0("- ", colnames(df), sep = "\n"))
  }
}


check_column_or_create_empty_char <- function(df = NULL, column = NULL) {

  if (!is.null(column)) {
    check_column_in_df(df, column)
    out <- column
  } else {
    out <- ""
  }

  out

}


#### Function to use logos/images provided by the user as PNG files,
## or create blank small images if not provided (NULL)

use_image <- function(image = NULL, name = NULL, folder = NULL) {

  ## If logos provided
  if (!is.null(image)) {
    if (!file.exists(image)) {
      stop(image, " file not found")
    } else {
      file.copy(from = image,
                to = file.path(folder, paste0(name, ".png")),
                overwrite = TRUE)
    }
  }

  ## If logos not provided
  if (is.null(image)) {  # create blank image
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(graphics::par(oldpar)))

    grDevices::png(file.path(folder, paste0(name, ".png")), 150, 150, "px")
    graphics::par(bg = "transparent")
    graphics::plot.new()
    grDevices::dev.off()
  }

}
