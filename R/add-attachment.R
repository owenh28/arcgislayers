#' Add Feature Service Attachments
#'
#' Add attachments to features in feature layers.
#'
#' @details
#'
#' If any requests fail, the requests are added as as the `errors` attribute to the resultant `data.frame`.
#' @param path a vecetor of the same length as `feature_id` indicating where the attachment exists.
#' @inheritParams arc_open
#' @param feature_ids a vector of object IDs that corresponds to the features that will receive attachments.
#' @param file_paths a vector of file paths that point to the files to upload as attachments.
#' @param file_names a vector of names that the attachments will use if use_basename is FALSE.
#' @param use_basename a logical determining whether the base file name in the file path will be used as the attachment name.
#' @returns a `data.frame` with 2 columns returning the status of the update.
#' @references See [API documentation](https://developers.arcgis.com/rest/services-reference/enterprise/add-attachment/#request-parameters) for more.
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#' library(arcgisutils)
#'
#' # authenticate
#' set_arc_token(auth_user())
#'
#' # open a feature service
#' feature_layer <- arc_open("your-item-id") |>
#'   # layer ID of the feature service
#'   get_layer(0)
#'
#' # create a list of features to update
#' features <- c(1,2,3)
#'
#' # create a list of files to upload as attachments
#' attachment_files <- c("path/to/file1.png", "path/to/file2.png", "path/to/file3.png")
#'
#' # add the attachment files to the features in the feature layer
#' add_response <- add_attachments(feature_layer, features, attachment_files, use_basename=TRUE)
#' }
#' }

add_attachments <- function(
x,
feature_ids,
file_paths,
file_names,
use_basename=TRUE,
.progress = TRUE,
token=arc_token()
){
  obj_check_layer(x)

  if (
    !rlang::is_character(feature_ids) && !rlang::is_integer(feature_ids)
  ) {
    cli::cli_abort("{.arg feature_ids} must be a character or integer vector")
  }
   if (anyNA(feature_id)) {
    cli::cli_abort("{.arg feature_ids} must not contain missing values")
  }
  if (anyNA(file_paths)) {
    cli::cli_abort("{.arg file_paths} must not contain missing values")
  }
  n_filepaths <- length(file_paths)
  n_features <- length(feature_ids)
  n_filenames <- file_names
  if (n_features != n_filepaths) {
    cli::cli_abort(
      "{.arg feature_ids} and {.arg file_paths} must be the same length"
    )
  }
  if (!use_basename && n_filenames != n_filepaths){
    cli::cli_abort(
      "{.arg file_names} and {.arg file_paths} must be the same length when {.arg use_basename} is FALSE"
    )
  }

  url <- x[["url"]]

  if(use_basename){
    reqs <- purr::pmap(list(feature_ids, file_paths), \(feat, path){
      file <- curl::form_file(path, name=basename(path))
      arc_base_req(
        url,
        path=c(feat, "addAttachment"),
        token=token,
        query = c(f="json")
      ) |> httr2::req_body_multipart(
        attachment=file,
      )
    })
  }else{
    reqs <- purr::pmap(list(feature_ids, file_paths, file_names), \(feat, path, name){
      file <- curl::form_file(path, name=name)
      arc_base_req(
        url,
        path=c(feat, "addAttachment"),
        token=token,
        query = c(f="json")
      ) |> httr2::req_body_multipart(
        attachment=file,
      )
    })
  }


  resps <- httr2::req_perform_parallel(
    reqs,
    max_active=3,
    progress = .progress,
    on_error = "continue"
  )
  all_resps_body <- lapply(
    httr2::resps_successes(resps),
    function(.x) {
      r <- httr2::resp_body_string(.x)
      cnd <- catch_error(r)

      if (rlang::is_condition(cnd)) {
        cnd$call <- rlang::caller_call(2)
        print(cnd)
        return(NULL)
      }
      as.data.frame(compact(RcppSimdJson::fparse(r)[[1]]))
    }
  )

  res <- rbind_results(all_resps_body)

  errors <- httr2::resps_failures(resps)
  n_errs <- length(errors)
  if (n_errs > 0) {
    cli::cli_warn(
      "{n_errs} occured. Error responses are stored in the `errors` attribute"
    )
    attr(res, "errors") <- errors
  }

  res
}