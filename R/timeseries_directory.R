
#' Download timeseries directory entries
#'
#' @param catno character vector. Catalogues to lookup
#'
#' @return data.table
#' @export

ts_directory <- function(catno = c("6202.0", "6291.0.55.001")){
  # Get the first page of each catalogue lookup to determine number of pages
  urls <- paste0("https://abs.gov.au/servlet/TSSearchServlet?catno=", catno)
  first_page <- lapply(urls, read_xml)
  n_pages <- lapply(first_page, \(x){
    xml2::xml_double(xml_find_all(x, "./NumPages"))
  }
  )

  # Download every page
  all_pages <- mapply(
    \(url, n_page) paste0(url, "&pg=", seq_len(n_page)),
    url = urls,
    n_page = n_pages
  )
  all_pages <- unlist(all_pages, use.names = FALSE)
  all_pages <- parallel::mclapply(
    all_pages,
    xml2::read_xml,
    mc.cores = ifelse(
      Sys.info()["sysname"] == "Windows",
      1L,
      parallel::detectCores()
    )
  )

  # Convert to data frames
  to_dt <- function(xml){
    col_names <- xml2::xml_name(
      xml2::xml_children(
        xml2::xml_find_first(xml, "./Series")
      )
    )
    out <- data.table::as.data.table(
      lapply(col_names, \(x) xml2::xml_text(
        xml_find_all(xml, xpath = paste0("//", x))
      )
      )
    )
    names(out) <- col_names
    out
  }

  all_pages <- lapply(all_pages, to_dt)


  # Return
  return(data.table::rbindlist(all_pages, fill = TRUE))
}
