# Files on an SMB share. The relative path under config$path is the relative
# path on the share in config$share. Every smbclient call goes through
# share_smbclient(), so a test can replace that one function.

# The one runner. Kerberos only: no credential goes into the argument vector.
share_smbclient <- function(commands) {
  return(processx::run(
    "smbclient",
    c(config$share, "--use-kerberos=required", "-N", "-c", commands),
    env = c("current", TZ = "UTC"),
    error_on_status = FALSE
  ))
}

share_enabled <- function() {
  return(isTRUE(nzchar(config$share)))
}

# NT status codes that mean "the remote path does not exist".
share_missing_codes <- c(
  "NT_STATUS_NO_SUCH_FILE",
  "NT_STATUS_OBJECT_NAME_NOT_FOUND",
  "NT_STATUS_OBJECT_PATH_NOT_FOUND"
)

# Resolves the arguments of path() in the frame `envir`, which is the frame of
# the caller of path_put(), path_get() or path_ls(). path() evaluates glue in
# its parent frame, and do.call(envir = ) makes that frame `envir`.
share_target <- function(dots, create_dir, trailing_slash, auto, envir) {
  local <- do.call(
    path,
    c(
      dots,
      list(
        create_dir = create_dir,
        trailing_slash = trailing_slash,
        auto = auto
      )
    ),
    envir = envir
  )
  file <- sub("/+$", "", local)
  root <- config$path
  if (!isTRUE(nzchar(root))) {
    stop(
      sprintf("'%s' is not under cs9::config$path, which is empty.", local),
      call. = FALSE
    )
  }
  prefix <- paste0(root, "/")
  if (!startsWith(file, prefix) || nchar(file) <= nchar(prefix)) {
    stop(
      sprintf("'%s' is not under cs9::config$path '%s'.", local, root),
      call. = FALSE
    )
  }
  parts <- strsplit(substring(file, nchar(prefix) + 1), "/", fixed = TRUE)[[1]]
  parts <- parts[parts != ""]
  share_check_parts(parts, local)
  return(list(
    local = local,
    file = file,
    parts = parts,
    remote = paste(parts, collapse = "\\")
  ))
}

share_check_parts <- function(parts, what) {
  if (any(parts %in% c(".", ".."))) {
    stop(
      sprintf("'%s' has a path component '.' or '..'.", what),
      call. = FALSE
    )
  }
  return(invisible(parts))
}

# The checks that matter only for smbclient. They run after the share check, so
# a session without a share accepts every name that path() accepts.
share_check_names <- function(target) {
  if (grepl("[\";[:cntrl:]]", target$file)) {
    stop(
      sprintf(
        "'%s' contains a double quote, a semicolon or a control character.",
        target$local
      ),
      call. = FALSE
    )
  }
  share_check_name_parts(target$parts, target$local)
  return(invisible(target))
}

share_check_name_parts <- function(parts, what) {
  if (any(grepl("[\"\\\\*?;[:cntrl:]]", parts))) {
    stop(
      sprintf(
        paste(
          "'%s' has a name that contains one of",
          "\" \\ * ? ; or a control character."
        ),
        what
      ),
      call. = FALSE
    )
  }
  return(invisible(parts))
}

# Double quotes around one path argument, for the command parser of smbclient.
share_quote <- function(x) {
  return(paste0("\"", x, "\""))
}

# Runs `commands` and stops, naming `what`, on any NT_STATUS line and on any
# non-zero exit status. With mkdir = TRUE, NT_STATUS_OBJECT_NAME_COLLISION is
# no error, and neither is the non-zero exit status that smbclient gives for it.
# With `missing` set, a not-found status stops with that message instead.
share_run <- function(commands, what, mkdir = FALSE, missing = NULL) {
  res <- tryCatch(
    share_smbclient(commands),
    error = function(e) {
      stop(
        sprintf(
          "smbclient did not run for '%s': %s",
          what,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  out <- unlist(strsplit(c(res$stdout, res$stderr), "\r?\n"))
  out <- out[nzchar(trimws(out))]
  codes <- unlist(regmatches(out, gregexpr("NT_STATUS_[A-Z_]+", out)))
  collision <- codes == "NT_STATUS_OBJECT_NAME_COLLISION"
  bad <- if (mkdir) codes[!collision] else codes
  failed <- length(bad) > 0 ||
    (!identical(as.integer(res$status), 0L) && !(mkdir && any(collision)))
  if (!failed) {
    return(res)
  }
  if (
    !is.null(missing) && length(bad) > 0 && all(bad %in% share_missing_codes)
  ) {
    stop(missing, call. = FALSE)
  }
  stop(
    sprintf(
      "smbclient failed for '%s' (exit status %s):\n%s",
      what,
      res$status,
      paste(trimws(out), collapse = "\n")
    ),
    call. = FALSE
  )
}

# Parses the output of the smbclient command `ls`, produced under TZ=UTC:
#   "  name with spaces                  A    12345  Tue Nov 10 13:38:24 2026"
# The name can contain spaces, so the pattern reads from the right: date, size,
# attributes, and the rest is the name. The entries "." and ".." are dropped.
share_parse_ls <- function(stdout) {
  lines <- unlist(strsplit(stdout, "\r?\n"))
  pattern <- paste0(
    "^\\s*(\\S(?:.*\\S)?)\\s+([VDAHSNRdtsrcone]+)\\s+([0-9]+)\\s+",
    "[A-Z][a-z]{2}\\s+([A-Z][a-z]{2})\\s+([0-9]{1,2})\\s+",
    "([0-9]{2}:[0-9]{2}:[0-9]{2})\\s+([0-9]{4})\\s*$"
  )
  lines <- lines[grepl(pattern, lines, perl = TRUE)]
  m <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  field <- function(i) vapply(m, function(x) x[i + 1], character(1))
  time <- sprintf(
    "%s-%02d-%02d %s",
    field(7),
    match(field(4), month.abb),
    as.integer(field(5)),
    field(6)
  )
  retval <- data.table::data.table(
    name = field(1),
    type = c("file", "directory")[grepl("D", field(2), fixed = TRUE) + 1],
    size = as.numeric(field(3)),
    modification_time = as.POSIXct(
      time,
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%S"
    )
  )
  return(retval[!retval$name %in% c(".", "..")])
}

share_list <- function(mask, what, missing = NULL) {
  res <- share_run(paste("ls", share_quote(mask)), what, missing = missing)
  return(share_parse_ls(res$stdout))
}

share_hex <- function() {
  # tempfile() draws its name outside the R random number generator, so the
  # caller's .Random.seed stays the same.
  return(basename(tempfile("")))
}

# Deletes a temporary remote name after a failure. The result is not checked:
# the error that called it is the one the caller needs.
share_discard <- function(remote) {
  try(share_smbclient(paste("del", share_quote(remote))), silent = TRUE)
  return(invisible(NULL))
}

share_mkdirs <- function(parts, what) {
  if (length(parts) == 0) {
    return(invisible(NULL))
  }
  folders <- vapply(
    seq_along(parts),
    function(i) paste(parts[seq_len(i)], collapse = "\\"),
    character(1)
  )
  share_run(
    paste0("mkdir ", share_quote(folders), collapse = "; "),
    what,
    mkdir = TRUE
  )
  return(invisible(NULL))
}

#' Upload a file under CS9_PATH to the same path on the share
#'
#' @description
#' `path_put()` takes the arguments of [path()] and uploads the file at that
#' path to the same relative path on the SMB share in `cs9::config$share`. It
#' returns the local path invisibly.
#'
#' @details
#' The share comes from the environment variable `CS9_SHARE`, for example
#' `//server/share`. Each call runs `smbclient` with
#' `--use-kerberos=required -N`, so the session needs a Kerberos ticket.
#'
#' The upload has four steps:
#'
#' 1. `mkdir` for each remote folder above the file, one by one. A folder that
#'    exists already is no error.
#' 2. `put` to the temporary name `<name>.partial-<pid>-<hex>` in the same
#'    remote folder.
#' 3. `ls` of the temporary name. The size on the share MUST be the local size.
#' 4. `rename ... -f`, which replaces the final name.
#'
#' When step 2 or step 3 fails, `path_put()` sends no `rename` and deletes
#' nothing under the final name. It tries to delete the temporary name, and it
#' stops with an error that contains the local path.
#'
#' `path_put()` always stops in these two cases, also without a share:
#'
#' - The path is not under `cs9::config$path`, or `cs9::config$path` is empty.
#' - A path component is `.` or `..`.
#'
#' When `cs9::config$share` is empty, `path_put()` then returns the path. It
#' uploads nothing and does not check the local file. With a share, it also
#' stops before it runs `smbclient` in these cases:
#'
#' - A name contains `"`, `\`, `*`, `?`, `;` or a control character.
#' - The local file does not exist.
#'
#' @inheritParams path
#' @return The local path from [path()], invisibly.
#' @seealso [path_get()], [path_ls()]
#' @examples
#' \dontrun{
#' saveRDS(mtcars, cs9::path("output", "weather", "results.rds"))
#' cs9::path_put("output", "weather", "results.rds")
#' }
#' @export
path_put <- function(
  ...,
  create_dir = FALSE,
  trailing_slash = FALSE,
  auto = cs9::config$is_auto
) {
  target <- share_target(
    list(...),
    create_dir,
    trailing_slash,
    auto,
    parent.frame()
  )
  if (!share_enabled()) {
    return(invisible(target$local))
  }
  share_check_names(target)
  if (!fs::file_exists(target$file) || fs::dir_exists(target$file)) {
    stop(
      sprintf(
        "'%s' is not a file, so path_put() cannot upload it.",
        target$file
      ),
      call. = FALSE
    )
  }
  n <- length(target$parts)
  share_mkdirs(target$parts[-n], target$file)
  temp_name <- paste0(
    target$parts[n],
    ".partial-",
    Sys.getpid(),
    "-",
    share_hex()
  )
  temp <- paste(c(target$parts[-n], temp_name), collapse = "\\")
  tryCatch(
    {
      share_run(
        paste("put", share_quote(target$file), share_quote(temp)),
        target$file
      )
      share_check_size(temp, temp_name, target$file)
    },
    error = function(e) {
      share_discard(temp)
      stop(e)
    }
  )
  share_run(
    paste("rename", share_quote(temp), share_quote(target$remote), "-f"),
    target$file
  )
  return(invisible(target$local))
}

share_check_size <- function(temp, temp_name, file) {
  size_local <- as.numeric(fs::file_size(file))
  listed <- share_list(temp, file)
  hit <- listed[listed$name == temp_name & listed$type == "file"]
  ok <- nrow(hit) == 1 && isTRUE(hit$size == size_local)
  if (!ok) {
    stop(
      sprintf(
        paste(
          "path_put() uploaded '%s', but the share holds %s bytes under the",
          "temporary name, not %s. The final name is unchanged."
        ),
        file,
        if (nrow(hit) == 1) hit$size else "no",
        size_local
      ),
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

#' Download a file or a folder under CS9_PATH from the same path on the share
#'
#' @description
#' `path_get()` takes the arguments of [path()] and downloads that path from
#' the SMB share in `cs9::config$share`, a file or a folder with all its
#' subfolders. It returns the local path when each remote file exists locally
#' with the remote size.
#'
#' @details
#' `path_get()` first lists the path on the share. A folder is listed again,
#' folder by folder, to find every file below it. `path_get()` creates the
#' local folders, also the empty ones, and sends one `get` per file. It then
#' compares the size of each local file with the size in that listing.
#'
#' `path_get()` stops, with an error that contains the local path, in these
#' cases:
#'
#' - The remote path does not exist.
#' - A local file is missing, or its size is not the remote size.
#' - Any `smbclient` call reports an `NT_STATUS` code or a non-zero exit
#'   status.
#'
#' The checks of the path and of the names in [path_put()] apply here too,
#' before any `smbclient` call. When `cs9::config$share` is empty, only the
#' checks of the path apply. `path_get()` then downloads nothing, does not check
#' the local path, and returns the path.
#'
#' @inheritParams path
#' @return The local path from [path()].
#' @seealso [path_put()], [path_ls()]
#' @examples
#' \dontrun{
#' folder <- cs9::path_get("input", "weather")
#' }
#' @export
path_get <- function(
  ...,
  create_dir = FALSE,
  trailing_slash = FALSE,
  auto = cs9::config$is_auto
) {
  target <- share_target(
    list(...),
    create_dir,
    trailing_slash,
    auto,
    parent.frame()
  )
  if (!share_enabled()) {
    return(target$local)
  }
  share_check_names(target)
  missing <- sprintf("'%s' does not exist on the share.", target$file)
  listed <- share_list(target$remote, target$file, missing = missing)
  # The share compares names without regard to case, and so does this.
  hit <- listed[
    tolower(listed$name) == tolower(target$parts[length(target$parts)])
  ]
  if (nrow(hit) != 1) {
    stop(missing, call. = FALSE)
  }
  if (hit$type == "directory") {
    entries <- share_walk(target$remote, target$file)
    fs::dir_create(c(target$file, entries$local[entries$type == "directory"]))
    files <- entries[entries$type == "file"]
  } else {
    files <- data.table::data.table(
      remote = target$remote,
      local = target$file,
      size = hit$size
    )
    fs::dir_create(fs::path_dir(target$file))
  }
  commands <- paste(
    "get",
    share_quote(files$remote),
    share_quote(files$local)
  )
  for (chunk in share_chunks(commands)) {
    share_run(paste(chunk, collapse = "; "), target$file)
  }
  share_check_local(files, target$file)
  return(target$local)
}

# Lists a remote folder and every folder below it.
share_walk <- function(remote, local) {
  listed <- share_list(paste0(remote, "\\*"), local)
  share_check_name_parts(listed$name, local)
  entries <- data.table::data.table(
    remote = paste0(remote, "\\", listed$name),
    local = paste0(local, "/", listed$name),
    type = listed$type,
    size = listed$size
  )
  below <- lapply(
    which(entries$type == "directory"),
    function(i) share_walk(entries$remote[i], entries$local[i])
  )
  return(data.table::rbindlist(c(list(entries), below)))
}

# Splits commands into groups well below the 128 KiB limit of one argument.
share_chunks <- function(commands, max_bytes = 30000) {
  if (length(commands) == 0) {
    return(list())
  }
  bytes <- cumsum(nchar(commands, type = "bytes") + 2)
  return(unname(split(commands, (bytes - 1) %/% max_bytes)))
}

share_check_local <- function(files, what) {
  exists <- fs::file_exists(files$local)
  size <- rep(NA_real_, nrow(files))
  size[exists] <- as.numeric(fs::file_size(files$local[exists]))
  bad <- files$local[is.na(size) | size != files$size]
  if (length(bad) > 0) {
    stop(
      sprintf(
        paste(
          "path_get() did not download '%s' whole. These files are missing or",
          "have another size than on the share:\n%s"
        ),
        what,
        paste(bad, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

#' List the entries of a folder on the share
#'
#' @description
#' `path_ls()` takes the arguments of [path()] and lists the entries directly
#' in that folder on the SMB share in `cs9::config$share`. When
#' `cs9::config$share` is empty, it lists the local folder with
#' [fs::dir_info()].
#'
#' @details
#' The entries keep the order of `smbclient ls`, and `.` and `..` are not
#' entries. The checks of the path and of the names in [path_put()] apply here
#' too, before any `smbclient` call. Without a share, only the checks of the
#' path apply. A folder that does not exist on the share is an error that
#' contains the local path.
#'
#' @inheritParams path
#' @return A `data.table` with one row per entry and four columns:
#'
#' - `path`: the local path of the entry, under `cs9::config$path`.
#' - `type`: `"file"` or `"directory"`. Without a share, the type from
#'   [fs::dir_info()], as text.
#' - `size`: the size in bytes, as a number.
#' - `modification_time`: a `POSIXct` in the time zone UTC.
#' @seealso [path_put()], [path_get()]
#' @examples
#' \dontrun{
#' cs9::path_ls("input", "weather")
#' }
#' @export
path_ls <- function(
  ...,
  create_dir = FALSE,
  trailing_slash = FALSE,
  auto = cs9::config$is_auto
) {
  target <- share_target(
    list(...),
    create_dir,
    trailing_slash,
    auto,
    parent.frame()
  )
  if (!share_enabled()) {
    info <- fs::dir_info(target$local)
    modification_time <- info$modification_time
    attr(modification_time, "tzone") <- "UTC"
    return(data.table::data.table(
      path = as.character(info$path),
      type = as.character(info$type),
      size = as.numeric(info$size),
      modification_time = modification_time
    ))
  }
  share_check_names(target)
  missing <- sprintf("'%s' does not exist on the share.", target$file)
  listed <- share_list(
    paste0(target$remote, "\\*"),
    target$file,
    missing = missing
  )
  return(data.table::data.table(
    path = paste0(target$file, "/", listed$name),
    type = listed$type,
    size = listed$size,
    modification_time = listed$modification_time
  ))
}
