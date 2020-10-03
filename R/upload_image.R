#' Upload issue images to GitHub
#'
#' Resizes an image to a maximum width, uploads both files to GitHub,
#' then generates a link to the resized file in markdown format.
#'
#' @md
#' @param file path to image which is to be uploaded
#' @param caption caption to insert into markdown link
#' @param width maximum width of resized image
#' @param issue current issue being edited (will be inferred from draft.md if needed)
#' @param image_repo local path to rweekly/image repository
#' @param push should the data be pushed to the repo immediately? default: TRUE
#'
#' @details `file` will be resized to a maximum width of `width` pixels.
#'
#' @importFrom git2r discover_repository add commit push
#' @importFrom stringr str_match str_remove
#' @importFrom httr GET content
#' @importFrom magick image_read image_resize image_write
#' @importFrom tools file_ext
#'
#' @return the markdown link (invisibly) after being printed to the console.
#' @export
upload_image <- function(file = NULL,
                         caption = NULL,
                         width = "600",
                         issue = NULL,
                         image_repo = NULL,
                         push = TRUE) {

  if (is.null(file)) stop("`file` is a required argument")
  if (is.null(image_repo)) stop("`image_repo` is a required argument")
  if (is.null(git2r::discover_repository(image_repo))) stop("Unable to use `image_repo` as a git repo")
  if (is.null(width)) stop("`width` is a required argument")
  if (is.null(caption)) {
    add_caption <- askYesNo("No caption added - would you like to add one?")
    caption <- if (!is.na(add_caption) && add_caption && interactive()) {
      readline("Enter caption for this image... ")
    } else {
      ""
    }
  }
  if (is.null(issue)) {
    # download draft.md
    draft <- httr::GET("https://raw.githubusercontent.com/rweekly/rweekly.org/gh-pages/draft.md") %>%
      httr::content("text")
    # obtain issue number
    issue <- stringr::str_match(draft, "Release Date: (.+)")[,2]
  }
  issue_dir <- file.path(image_repo, issue)
  if (!dir.exists(issue_dir)) {
    message("Creating issue folder in image_repo: ", issue_dir)
    dir.create(issue_dir)
    git2r::add(repo = image_repo, path = issue_dir)
  }

  # clean width
  width <- stringr::str_remove(width, "px$")

  # copy original to image_repo
  repo_file <- file.path(image_repo, issue, basename(file))
  if (file.exists(repo_file)) stop(repo_file, " already exists in repo. Delete that first")
  file.copy(file, repo_file)

  # convert image to max width (as a copy)
  fileext <- tools::file_ext(file)
  filename <- stringr::str_remove(basename(file), paste0(".", fileext))
  filename_resized <- paste0(file.path(image_repo, issue, filename), "_", width, ".", fileext)
  if (file.exists(filename_resized)) stop(filename_resized, " already exists in repo. Delete that first")
  im <- magick::image_read(file)
  message("Resizing image...")
  im_resized <- magick::image_resize(im, geometry = paste0(width, "x"))
  message("Done")
  magick::image_write(im_resized, path = filename_resized)

  # commit images
  git2r::add(repo = image_repo, path = file.path(issue, basename(repo_file)))
  git2r::add(repo = image_repo, path = file.path(issue, basename(filename_resized)))
  git2r::commit(repo = image_repo, message = paste0("[auto] images for ", issue))
  if (push) {
    git2r::push(image_repo)
  }

  # links for issue
  link <- paste0("![", caption, "](https://raw.githubusercontent.com/rweekly/image/master/", issue, "/", basename(filename_resized), ")")
  message("Copy this markdown link into the image:")
  cat(link)

  return(invisible(link))
}

#' @export
#' @importFrom git2r push
push <- git2r::push
