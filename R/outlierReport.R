#' Examine outliers in a data set using IRQ method
#'
#' `outlierReport` returns either an Excel report or a list which
#' contain information about any mild and severe outliers found in
#' the data. Classification based on being either 1.5x or 3.0x
#' outside the IRQ range.
#'
#' @details
#' If 'var_vec is NULL, the output will include all of the numeric
#' variables in the 'dat'. This might be overkill for your application.
#'
#' If 'label' is provided, the output table will have columns for
#' 'Mild_Outliers' and 'Severe_Outliers' with whatever is contained
#' in 'label'. This provides a way to track if the same row
#' is deemed an outlier on multiple numeric columns.
#'
#' 'file_path' must end in '.xlsx'. It's possible to point the file
#' at an existing sub-directory of the working directory by using something
#' like './reports/Outlier_Report_v1.xlsx'.
#'
#' If 'output_excel' is TRUE, the function will try to write an Excel
#' file report. Otherwise wit will return a list. The list has the same
#' data from that would be printed in the Excel file, and as well as
#' a list of the outlier values of each variable.
#'
#'
#' @param dat a data.frame
#'
#' An error will occur if 'output_excel' is set to TRUE AND 'file_path' is not specified.
#'
#'
#' @param dat a data.frame
#' @param var_vec NULL or a character vector of colnames to be included
#' @param labels NULL or a string that names a colname that holds row-wise ids.
#' @param file_path NULL or a string that names the output file.
#' @param output_excel a logical.

#' @return an .xlsx file or a list
#' @examples
#'
#'  # return a list
#'  res <- outlierReport(dat = airquality, output_excel = FALSE)
#'
#'  # return an Excel file
#'  outlierReport(dat = airquality, file_path = "OutilerTable.xlsx" ,output_excel = TRUE)
#'
#' @import openxlsx
#
#' @export
outlierReport <- function(dat, var_vec = NULL,  labels = NULL,
                          file_path = NULL, output_excel = TRUE){

  # function
  is_outlier <- function(x){

    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    irq_val <- q3 - q1

    m_upr <- q3 + 1.5 * irq_val
    m_lwr <- q1 - 1.5 * irq_val
    s_upr <- q3 + 3 * irq_val
    s_lwr <- q1 - 3 * irq_val

    m_outliers <- x[x > m_upr & x <= s_upr | x < m_lwr & x >= s_lwr ]
    s_outliers <- x[x > s_upr | x < s_lwr]
    out <- list(mild = x %in% m_outliers, severe =  x %in% s_outliers)

    return(out)

  }

  # select data
  if(!is.null(var_vec)){

    d <- dat[, var_vec]

  } else {

    var_vec <- names(dat)
    d <- dat

  }

  # remove non numeric variables, if necessary
  is_numeric <- sapply(d, is.numeric)

  if(!all(is_numeric)){

    mess <- paste0(paste(names(d)[!is_numeric], collapse = ", "),
                   " is/are not numeric. Will be excluded from analysis.")
    message(mess)

    d <- d[is_numeric]
    var_vec <- names(d)

  }

  # evaluate outlier-ness
  out_lst <- list()

  for(i in seq_along(var_vec)){

    x <- dat[[var_vec[i]]]
    if(!is.null(labels)) { names(x) <- dat[[labels]]    }
    x <- na.omit(x)
    tmp <- is_outlier(x = x)
    m_outs <- x[tmp$mild]
    s_outs <- x[tmp$severe]

    if(length(m_outs) > 1){

      out_lst[[i]]  <- list(mild = m_outs, severe = s_outs)

    } else {

      out_lst[[i]]  <- NA

    }

  }

  names(out_lst) <- var_vec

  out_lst <- out_lst[ sapply(out_lst, is.list) ]

  # build table
  res_lst <- list()

  for(i in seq_along(out_lst)){

    tmp <- out_lst[[i]]

    Mild_Outliers <- paste(names(tmp$mild), collapse = ", ")
    Mild_Values <- paste(tmp$mild, collapse = ", ")
    Severe_Outliers <- paste(names(tmp$severe), collapse = ", ")
    Severe_Values <- paste(tmp$severe, collapse = ", ")

    string_vec <- c(Variable = names(out_lst)[i],
                    Mild_Outliers = Mild_Outliers,
                    Mild_Values = Mild_Values,
                    Severe_Outliers = Severe_Outliers,
                    Severe_Values = Severe_Values)

    res_lst[[i]] <- string_vec

  }

  res_tbl <- as.data.frame(do.call("rbind", res_lst))

  if(is.null(labels)) { res_tbl <- res_tbl[, !names(res_tbl) %in% c("Mild_Outliers", "Severe_Outliers") ]}

  if(!output_excel){

    return(list(out_table = res_tbl, out_list = out_lst))

  } else if(!is.null(file_path)) {

    # test file_name
    hasCorrectFileExtension <- grepl(".xlsx$", file_path)
    stopifnot(hasCorrectFileExtension)

    # formatting
    left_align <- createStyle(halign = "left", wrapText = TRUE, valign = "top")
    left_align_bold <- createStyle(halign = "left", textDecoration = "bold")

    # create a workbook
    wb <- createWorkbook()
    sheet <- "Outlier_Table"
    addWorksheet(wb, sheet)

    # write data to worksheet
    writeData(wb = wb, sheet = sheet, x = res_tbl, headerStyle = left_align_bold)
    addStyle(wb = wb, sheet = sheet, left_align,
             rows = 2:(nrow(res_tbl)+1), cols = 1:(ncol(res_tbl)),
             gridExpand = TRUE, stack = TRUE )
    freezePane(wb = wb, sheet = sheet, firstRow = TRUE)
    if(is.null(labels)) wdths <- c(25, 25, 25) else wdths <- c(25, 50, 25, 50, 25)
    setColWidths(wb = wb, sheet = sheet, cols = 1:5, widths = wdths)

    # save file
    saveWorkbook(wb, file_path, overwrite = TRUE)
    message(paste0("Saved to ", file_path))

  } else {

    stop("Please provide a valid file name to 'file_path'.")

  }

}


