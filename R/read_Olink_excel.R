# -----------------------------------------------------------------------------#
#' Read Olink Excel
#' 
#' This parse an output file in Excel format from Olink NPX manager
#'
#' @param file a Excel file from the manager
#' 
#' @return a list of two tibbles (or data frames). One is for NPX data and the
#'   other for binders
#'   
#' @author Mun-Gwan Hong <\email{mun-gwan.hong@scilifelab.se}>
#' @seealso
#' \code{\link{read_excel}}
#' @importFrom readxl read_excel
#' @export
# -----------------------------------------------------------------------------#
# created  : 2018-09-12 by Mun-Gwan
# modified : 
#   2018-12-18 by Mun-Gwan : change output to a list
#   2020-01-09 by Mun-Gwan : change output to a list of two tibbles
# -----------------------------------------------------------------------------#

read_Olink_excel <- function(file) {
  rd0 <- suppressMessages(  # avoid 'New names: * `` -> ...1 * `` -> ...2  ...
    readxl::read_excel(
      path= file,
      col_names= F,
      col_types= "text"
    )
  )
  
  #  The first words in every line
  first_word <- pull(rd0, 1)

  ##  find starting rows of various information
  i_panel <- which(first_word == "Panel")
  i_olinkid <- which(first_word == "OlinkID")
  i_lod <- which(first_word == "LOD")
  
  ##  Indices for Olink NPX data
  i_data <- c((i_olinkid + 1):(i_lod - 1))
  i_data <- i_data[!is.na(first_word[i_data])]  # in case of any NA
  
  j_data <- slice(rd0, i_olinkid) %>% 
    unlist(use.names= F) %>% {
      which(!is.na(.))
    }
  
  ##  Header - About binders
  rd_h <- rd0[c(i_panel:i_olinkid, i_lod:(i_lod + 1)), j_data] %>% 
    t() %>% 
    `colnames<-`(unlist(.[1, ])) %>%   # title
    `[`(-1, ) %>% 
    as_tibble %>% 
    separate(Panel, c("Panel", "Version"), sep= "\\(") %>% 
    rename(`LLOD prop.` = "Missing Data freq.") %>% 
    mutate(
      Version = sub("\\)$", "", Version),
      LOD = as.numeric(LOD),
      `LLOD prop.` = as.numeric(`LLOD prop.`)
    ) %>% 
    select(OlinkID, Assay, LOD, `LLOD prop.`, `Uniprot ID`, everything())
  
  stopifnot(anyDuplicated(rd_h$OlinkID) == 0)
  
  ##  Numeric NPX Data
  rd_X <- rd0[i_data, j_data] %>% 
    `names<-`(unlist(rd0[i_olinkid, j_data])) %>% 
    mutate_at(-1, as.numeric) %>% 
    rename(id = "OlinkID")
  
  
  ##  Plate ID
  j_plate <- which(unlist(rd0[i_panel + 1, ], use.names= F) == "Plate ID")
  rd_plate <- rd0[i_data, j_plate] %>% 
    `names<-`(paste(unlist(slice(rd0, i_panel)[j_plate]), "Plate", sep= "."))
  
  ##  QC warning
  j_warn <- which(unlist(rd0[i_panel + 1, ], use.names= F) == "QC Warning")
  rd_warning <- rd0[i_data, j_warn] %>% 
    `names<-`(paste(unlist(slice(rd0, i_panel)[j_plate]), "QC_Warning", sep= "."))
  
  
  stopifnot((dim(rd_X)[2L] - 1) == nrow(rd_h))

  list(
    npx = rd_X,
    binder = rd_h,
    plate = rd_plate, 
    warning = rd_warning
  )
}

