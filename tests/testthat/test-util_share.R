# Tests of path_put(), path_get() and path_ls(). No test reaches the network or
# runs smbclient: share_smbclient() is replaced by a fake share in memory, which
# records each command and answers in the output format of smbclient.

# Sets config$path to a new temporary folder and config$share to `share`.
local_share_root <- function(share = "//server/share", env = parent.frame()) {
  root <- as.character(fs::path(tempfile("cs9-share-")))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  old_path <- config$path
  old_share <- config$share
  config$path <- root
  config$share <- share
  withr::defer(
    {
      config$path <- old_path
      config$share <- old_share
      unlink(root, recursive = TRUE)
    },
    envir = env
  )
  root
}

write_local <- function(file, text) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(text), file)
  file
}

# Splits one smbclient command into tokens as smbclient does: whitespace
# separates tokens, and a double quote starts or ends a quoted part.
fake_tokens <- function(line) {
  tokens <- character()
  current <- ""
  quoted <- FALSE
  started <- FALSE
  for (ch in strsplit(line, "")[[1]]) {
    if (ch == "\"") {
      quoted <- !quoted
      started <- TRUE
    } else if (!quoted && ch %in% c(" ", "\t", "\r", "\n")) {
      if (started) {
        tokens <- c(tokens, current)
      }
      current <- ""
      started <- FALSE
    } else {
      current <- paste0(current, ch)
      started <- TRUE
    }
  }
  if (started) {
    tokens <- c(tokens, current)
  }
  tokens
}

# A share in memory. `files` maps a remote path to its content as text.
# put_status: NULL for a good put, or the exit status of a put that reports
# NT_STATUS_ACCESS_DENIED. put_drop: bytes the share loses on each put.
# get_drop: bytes that each get loses. mkdir_status: NULL, or a status code
# that each mkdir of a new folder reports.
fake_share <- function(
  files = list(),
  dirs = character(),
  put_status = NULL,
  put_drop = 0,
  get_drop = 0,
  mkdir_status = NULL
) {
  state <- new.env()
  state$files <- lapply(files, charToRaw)
  state$dirs <- dirs
  state$log <- list()
  state$calls <- character()

  norm <- function(x) sub("^\\\\+", "", x)
  parent <- function(x) {
    p <- sub("\\\\[^\\\\]*$", "", x)
    if (identical(p, x)) "" else p
  }
  has_dir <- function(x) x == "" || x %in% state$dirs
  exists <- function(x) x %in% names(state$files) || x %in% state$dirs
  line <- function(name, attr, size) {
    sprintf(
      "  %-30s%7.7s %8.0f  %s",
      name,
      attr,
      size,
      "Tue Nov 10 13:38:24 2026"
    )
  }
  answer <- function(out, status = 0L) list(out = out, status = status)

  cmd_mkdir <- function(x) {
    x <- norm(x)
    if (exists(x)) {
      return(answer(
        sprintf(
          "NT_STATUS_OBJECT_NAME_COLLISION making remote directory \\%s",
          x
        ),
        1L
      ))
    }
    if (!is.null(mkdir_status)) {
      return(answer(
        sprintf("%s making remote directory \\%s", mkdir_status, x),
        1L
      ))
    }
    if (!has_dir(parent(x))) {
      return(answer(
        sprintf(
          "NT_STATUS_OBJECT_PATH_NOT_FOUND making remote directory \\%s",
          x
        ),
        1L
      ))
    }
    state$dirs <- c(state$dirs, x)
    answer(character())
  }
  cmd_put <- function(local, remote) {
    remote <- norm(remote)
    if (!is.null(put_status)) {
      return(answer(
        sprintf("NT_STATUS_ACCESS_DENIED opening remote file \\%s", remote),
        put_status
      ))
    }
    if (!file.exists(local)) {
      return(answer(sprintf("%s does not exist", local), 1L))
    }
    if (!has_dir(parent(remote))) {
      return(answer(
        sprintf(
          "NT_STATUS_OBJECT_PATH_NOT_FOUND opening remote file \\%s",
          remote
        ),
        1L
      ))
    }
    bytes <- readBin(local, "raw", file.size(local))
    state$files[[remote]] <- bytes[seq_len(max(0, length(bytes) - put_drop))]
    answer(sprintf(
      "putting file %s as \\%s (1.0 kb/s) (average 1.0 kb/s)",
      local,
      remote
    ))
  }
  cmd_ls <- function(mask) {
    mask <- norm(mask)
    blocks <- "\t\t5119 blocks of size 4096. 1234 blocks available"
    if (endsWith(mask, "\\*")) {
      folder <- sub("\\\\\\*$", "", mask)
      if (!has_dir(folder)) {
        return(answer(
          sprintf("NT_STATUS_OBJECT_NAME_NOT_FOUND listing \\%s", mask),
          1L
        ))
      }
      f <- names(state$files)[vapply(names(state$files), parent, "") == folder]
      d <- state$dirs[vapply(state$dirs, parent, "") == folder]
      base <- function(x) sub("^.*\\\\", "", x)
      out <- c(
        line(".", "D", 0),
        line("..", "D", 0),
        vapply(d, function(x) line(base(x), "D", 0), ""),
        vapply(f, function(x) line(base(x), "A", length(state$files[[x]])), ""),
        "",
        blocks
      )
      return(answer(unname(out)))
    }
    name <- sub("^.*\\\\", "", mask)
    if (mask %in% names(state$files)) {
      return(answer(c(
        line(name, "A", length(state$files[[mask]])),
        "",
        blocks
      )))
    }
    if (mask %in% state$dirs) {
      return(answer(c(line(name, "D", 0), "", blocks)))
    }
    answer(sprintf("NT_STATUS_NO_SUCH_FILE listing \\%s", mask), 1L)
  }
  cmd_rename <- function(from, to, flag = NA) {
    from <- norm(from)
    to <- norm(to)
    if (!from %in% names(state$files)) {
      return(answer(
        sprintf("NT_STATUS_OBJECT_NAME_NOT_FOUND renaming files \\%s", from),
        1L
      ))
    }
    if (exists(to) && !identical(flag, "-f")) {
      return(answer(
        sprintf("NT_STATUS_OBJECT_NAME_COLLISION renaming files \\%s", to),
        1L
      ))
    }
    state$files[[to]] <- state$files[[from]]
    state$files[[from]] <- NULL
    answer(character())
  }
  cmd_del <- function(x) {
    x <- norm(x)
    if (!x %in% names(state$files)) {
      return(answer(
        sprintf("NT_STATUS_NO_SUCH_FILE deleting remote file \\%s", x),
        1L
      ))
    }
    state$files[[x]] <- NULL
    answer(character())
  }
  cmd_get <- function(remote, local) {
    remote <- norm(remote)
    if (!remote %in% names(state$files)) {
      return(answer(
        sprintf(
          "NT_STATUS_OBJECT_NAME_NOT_FOUND opening remote file \\%s",
          remote
        ),
        1L
      ))
    }
    bytes <- state$files[[remote]]
    writeBin(bytes[seq_len(max(0, length(bytes) - get_drop))], local)
    answer(sprintf(
      "getting file \\%s of size %d as %s",
      remote,
      length(bytes),
      local
    ))
  }

  state$run <- function(commands) {
    state$calls <- c(state$calls, commands)
    out <- character()
    status <- 0L
    for (one in strsplit(commands, ";", fixed = TRUE)[[1]]) {
      tok <- fake_tokens(one)
      if (length(tok) == 0) {
        next
      }
      state$log[[length(state$log) + 1]] <- tok
      r <- switch(
        tok[1],
        mkdir = cmd_mkdir(tok[2]),
        put = cmd_put(tok[2], tok[3]),
        ls = cmd_ls(tok[2]),
        rename = cmd_rename(tok[2], tok[3], tok[4]),
        del = cmd_del(tok[2]),
        get = cmd_get(tok[2], tok[3]),
        answer(sprintf("%s: command not found", tok[1]), 1L)
      )
      out <- c(out, r$out)
      status <- r$status
    }
    list(status = status, stdout = paste(out, collapse = "\n"), stderr = "")
  }
  state$verbs <- function() vapply(state$log, function(x) x[1], "")
  state$content <- function(x) {
    if (is.null(state$files[[x]])) {
      NA_character_
    } else {
      rawToChar(state$files[[x]])
    }
  }
  state
}

test_that("share_smbclient() runs smbclient with Kerberos, the share and the commands only", {
  local_share_root(share = "//server/share")
  seen <- NULL
  local_mocked_bindings(
    run = function(...) {
      seen <<- list(...)
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )
  share_smbclient("ls \"a\\*\"")
  expect_equal(seen[[1]], "smbclient")
  expect_equal(
    seen[[2]],
    c("//server/share", "--use-kerberos=required", "-N", "-c", "ls \"a\\*\"")
  )
  expect_equal(seen$env, c("current", TZ = "UTC"))
  expect_false(seen$error_on_status)
})

test_that("set_env_vars() reads CS9_SHARE into config$share, and unset is empty", {
  old <- mget(c("path", "share", "is_auto", "dbconfigs"), envir = config)
  withr::defer(list2env(old, envir = config))

  withr::local_envvar(CS9_SHARE = "//server/share")
  set_env_vars()
  expect_equal(config$share, "//server/share")

  withr::local_envvar(CS9_SHARE = NA)
  set_env_vars()
  expect_equal(config$share, "")
})

test_that("ls output in the format of smbclient parses from the right", {
  stdout <- paste(
    c(
      "  .                                   D        0  Tue Nov 10 11:19:05 2026",
      "  ..                                  D        0  Tue Nov 10 11:19:05 2026",
      "    norsyss_vekerapport                 D        0  Tue Nov 10 11:19:05 2026",
      "  name   A   12345  Tue Nov 10 13:38:24 2026",
      "  uke 45 rapport A.xlsx               A   987654  Tue Nov  3 09:05:01 2026",
      "  epistatusmøte                       DH       0  Mon Nov  2 23:59:59 2026",
      "",
      "\t\t5119 blocks of size 4096. 1234 blocks available"
    ),
    collapse = "\n"
  )
  x <- share_parse_ls(stdout)
  expect_equal(
    x$name,
    c("norsyss_vekerapport", "name", "uke 45 rapport A.xlsx", "epistatusmøte")
  )
  expect_equal(x$type, c("directory", "file", "file", "directory"))
  expect_equal(x$size, c(0, 12345, 987654, 0))
  expect_equal(
    x$modification_time,
    as.POSIXct(
      c(
        "2026-11-10 11:19:05",
        "2026-11-10 13:38:24",
        "2026-11-03 09:05:01",
        "2026-11-02 23:59:59"
      ),
      tz = "UTC"
    )
  )
  expect_equal(attr(x$modification_time, "tzone"), "UTC")

  empty <- share_parse_ls("")
  expect_equal(nrow(empty), 0)
  expect_type(empty$type, "character")
  expect_type(empty$size, "double")
  expect_s3_class(empty$modification_time, "POSIXct")
})

test_that("path_put() puts a temporary name, checks its size, then renames it over the final name", {
  root <- local_share_root()
  write_local(file.path(root, "output", "report.txt"), "new content")
  fake <- fake_share(
    files = list("output\\report.txt" = "old"),
    dirs = "output"
  )
  local_mocked_bindings(share_smbclient = fake$run)

  res <- tryCatch(
    path_put("output", "report.txt", auto = TRUE),
    error = function(e) e
  )
  verbs <- fake$verbs()
  expect_equal(verbs, c("mkdir", "put", "ls", "rename"))
  expect_lt(match("put", verbs), match("ls", verbs))
  expect_lt(match("ls", verbs), match("rename", verbs))

  temp <- fake$log[[2]][3]
  expect_match(
    temp,
    paste0("^output\\\\report\\.txt\\.partial-", Sys.getpid(), "-[0-9a-f]+$")
  )
  expect_equal(fake$log[[3]][2], temp)
  expect_equal(fake$log[[4]], c("rename", temp, "output\\report.txt", "-f"))
  expect_equal(res, paste0(root, "/output/report.txt"))
  expect_equal(fake$content("output\\report.txt"), "new content")
  expect_equal(sort(names(fake$files)), "output\\report.txt")
})

test_that("path_put() returns the local path invisibly", {
  root <- local_share_root()
  write_local(file.path(root, "a.txt"), "x")
  fake <- fake_share()
  local_mocked_bindings(share_smbclient = fake$run)
  expect_invisible(path_put("a.txt", auto = TRUE))
  expect_equal(fake$verbs(), c("put", "ls", "rename"))
  expect_equal(fake$content("a.txt"), "x")
})

test_that("path_put() stops on a size mismatch, naming the file, and issues no rename", {
  root <- local_share_root()
  local <- write_local(file.path(root, "output", "report.txt"), "new content")
  fake <- fake_share(dirs = "output", put_drop = 1)
  local_mocked_bindings(share_smbclient = fake$run)

  expect_error(
    path_put("output", "report.txt", auto = TRUE),
    paste0("'", local, "'.*holds 10 bytes.*not 11"),
    fixed = FALSE
  )
  expect_false("rename" %in% fake$verbs())
  expect_false("output\\report.txt" %in% names(fake$files))
})

test_that("path_put() stops on a failed put that reports NT_STATUS with exit status 0", {
  root <- local_share_root()
  local <- write_local(file.path(root, "output", "report.txt"), "new content")
  fake <- fake_share(dirs = "output", put_status = 0L)
  local_mocked_bindings(share_smbclient = fake$run)

  err <- expect_error(path_put("output", "report.txt", auto = TRUE))
  expect_match(conditionMessage(err), "NT_STATUS_ACCESS_DENIED", fixed = TRUE)
  expect_match(conditionMessage(err), local, fixed = TRUE)
  expect_equal(fake$verbs(), c("mkdir", "put", "del"))
})

test_that("path_put() stops on a non-zero exit status without NT_STATUS", {
  root <- local_share_root()
  local <- write_local(file.path(root, "report.txt"), "new content")
  local_mocked_bindings(
    share_smbclient = function(commands) {
      list(status = 1L, stdout = "session setup failed", stderr = "")
    }
  )
  err <- expect_error(path_put("report.txt", auto = TRUE))
  expect_match(conditionMessage(err), "exit status 1", fixed = TRUE)
  expect_match(conditionMessage(err), local, fixed = TRUE)
})

test_that("path_put() keeps the previous final file when the put or the size check fails", {
  root <- local_share_root()
  write_local(file.path(root, "output", "report.txt"), "new content")
  final <- "output\\report.txt"

  old <- stats::setNames(list("old"), final)
  for (fake in list(
    fake_share(files = old, dirs = "output", put_status = 1L),
    fake_share(files = old, dirs = "output", put_drop = 1)
  )) {
    local_mocked_bindings(share_smbclient = fake$run)
    expect_error(path_put("output", "report.txt", auto = TRUE))
    expect_equal(fake$content(final), "old")
    expect_false("rename" %in% fake$verbs())
    targets <- vapply(fake$log, function(x) x[2], "")
    expect_false(any(fake$verbs() == "del" & targets == final))
    expect_equal(names(fake$files), final)
  }
})

test_that("path_put() quotes names with non-ASCII letters and spaces", {
  root <- local_share_root()
  write_local(
    file.path(root, "epistatusmøte", "uke 45", "rapport uke 45.txt"),
    "innhold"
  )
  fake <- fake_share()
  local_mocked_bindings(share_smbclient = fake$run)

  res <- tryCatch(
    path_put("epistatusmøte", "uke 45", "rapport uke 45.txt", auto = TRUE),
    error = conditionMessage
  )
  expect_equal(res, paste0(root, "/epistatusmøte/uke 45/rapport uke 45.txt"))
  expect_equal(fake$dirs, c("epistatusmøte", "epistatusmøte\\uke 45"))
  expect_equal(
    fake$content("epistatusmøte\\uke 45\\rapport uke 45.txt"),
    "innhold"
  )
  expect_match(fake$calls[2], "^put \"[^\"]+\" \"[^\"]+\"$")
})

test_that("path_put() creates remote folders one by one and tolerates a collision", {
  root <- local_share_root()
  write_local(file.path(root, "output", "a", "b", "f.txt"), "x")
  fake <- fake_share(dirs = "output")
  local_mocked_bindings(share_smbclient = fake$run)

  path_put("output", "a", "b", "f.txt", auto = TRUE)
  mkdirs <- fake$log[fake$verbs() == "mkdir"]
  expect_equal(
    vapply(mkdirs, function(x) x[2], ""),
    c("output", "output\\a", "output\\a\\b")
  )
  expect_equal(fake$content("output\\a\\b\\f.txt"), "x")

  # Every folder exists, so every mkdir collides and smbclient exits with 1.
  fake2 <- fake_share(dirs = c("output", "output\\a", "output\\a\\b"))
  local_mocked_bindings(share_smbclient = fake2$run)
  expect_no_error(path_put("output", "a", "b", "f.txt", auto = TRUE))
  expect_equal(fake2$content("output\\a\\b\\f.txt"), "x")
})

test_that("path_put() stops on a mkdir NT_STATUS other than a collision", {
  root <- local_share_root()
  local <- write_local(file.path(root, "output", "f.txt"), "x")
  fake <- fake_share(mkdir_status = "NT_STATUS_ACCESS_DENIED")
  local_mocked_bindings(share_smbclient = fake$run)

  err <- expect_error(path_put("output", "f.txt", auto = TRUE))
  expect_match(conditionMessage(err), "NT_STATUS_ACCESS_DENIED", fixed = TRUE)
  expect_match(conditionMessage(err), local, fixed = TRUE)
  expect_equal(fake$verbs(), "mkdir")
})

test_that("path_put() stops naming the file when the local file does not exist", {
  root <- local_share_root()
  calls <- 0
  local_mocked_bindings(share_smbclient = function(commands) {
    calls <<- calls + 1
    list(status = 0L, stdout = "", stderr = "")
  })
  expect_error(
    path_put("output", "missing.txt", auto = TRUE),
    paste0(root, "/output/missing.txt"),
    fixed = TRUE
  )
  expect_equal(calls, 0)
})

test_that("the three functions call no runner and return the path when config$share is empty", {
  root <- local_share_root(share = "")
  write_local(file.path(root, "output", "f.txt"), "x")
  calls <- 0
  local_mocked_bindings(share_smbclient = function(commands) {
    calls <<- calls + 1
    list(status = 0L, stdout = "", stderr = "")
  })

  put <- tryCatch(
    path_put("output", "f.txt", auto = TRUE),
    error = conditionMessage
  )
  get <- tryCatch(
    path_get("output", "f.txt", auto = TRUE),
    error = conditionMessage
  )
  get_interactive <- tryCatch(
    path_get("input", "x", auto = FALSE),
    error = conditionMessage
  )
  listed <- tryCatch(path_ls("output", auto = TRUE), error = conditionMessage)
  expect_equal(calls, 0)
  expect_equal(put, paste0(root, "/output/f.txt"))
  expect_equal(get, paste0(root, "/output/f.txt"))
  expect_equal(get_interactive, paste0(root, "/input/_interactive/x"))
  expect_s3_class(listed, "data.table")
  expect_invisible(path_put("output", "f.txt", auto = TRUE))

  # Without a share, path_put() does not check the local file, and the names
  # need no quoting for smbclient.
  missing <- tryCatch(
    path_put("output", "missing.txt", auto = TRUE),
    error = conditionMessage
  )
  expect_equal(missing, paste0(root, "/output/missing.txt"))
  expect_invisible(path_put("output", "missing.txt", auto = TRUE))
  odd <- tryCatch(
    path_get("output", "a;b*.txt", auto = TRUE),
    error = conditionMessage
  )
  expect_equal(odd, paste0(root, "/output/a;b*.txt"))
  expect_equal(calls, 0)

  # The checks of rule 3 come first, also without a share.
  expect_error(
    path_put("output", "..", "missing.txt", auto = TRUE),
    "has a path component '.' or '..'",
    fixed = TRUE
  )
})

test_that("path_ls() without a share returns fs::dir_info() reduced to four columns", {
  root <- local_share_root(share = "")
  write_local(file.path(root, "output", "f.txt"), "abc")
  dir.create(file.path(root, "output", "sub"))
  local_mocked_bindings(share_smbclient = function(commands) {
    stop("runner called")
  })

  x <- path_ls("output", auto = TRUE)
  info <- fs::dir_info(path("output", auto = TRUE))
  expect_s3_class(x, "data.table")
  expect_equal(names(x), c("path", "type", "size", "modification_time"))
  expect_equal(x$path, as.character(info$path))
  expect_equal(x$type, as.character(info$type))
  expect_setequal(x$type, c("file", "directory"))
  expect_type(x$size, "double")
  expect_equal(x$size, as.numeric(info$size))
  expect_equal(
    as.numeric(x$modification_time),
    as.numeric(info$modification_time)
  )
  expect_equal(attr(x$modification_time, "tzone"), "UTC")
})

test_that("the three functions evaluate glue in the caller's frame", {
  root <- local_share_root()
  glue_week <- "2026-45"
  local <- write_local(file.path(root, "output", "report_2026-45.txt"), "x")
  fake <- fake_share(dirs = "output")
  local_mocked_bindings(share_smbclient = fake$run)

  # Each call runs inside tryCatch(), so glue that is evaluated in the wrong
  # frame gives an error text, and the equality assertion rejects that text.
  expected <- path("output", "report_{glue_week}.txt", auto = TRUE)
  expect_equal(expected, local)
  put <- tryCatch(
    path_put("output", "report_{glue_week}.txt", auto = TRUE),
    error = conditionMessage
  )
  expect_equal(put, expected)
  expect_equal(fake$content("output\\report_2026-45.txt"), "x")

  unlink(local)
  get <- tryCatch(
    path_get("output", "report_{glue_week}.txt", auto = TRUE),
    error = conditionMessage
  )
  expect_equal(get, expected)
  expect_true(file.exists(local))

  glue_folder <- "output"
  listed <- tryCatch(
    path_ls("{glue_folder}", auto = TRUE)$path,
    error = conditionMessage
  )
  expect_equal(listed, expected)

  # A caller one level down sees its own variable, not the test's one.
  caller <- function() {
    glue_week <- "2026-46"
    tryCatch(
      path_get("output", "report_{glue_week}.txt", auto = TRUE),
      error = conditionMessage
    )
  }
  expect_equal(
    caller(),
    paste0(
      "'",
      root,
      "/output/report_2026-46.txt' does not exist on the share."
    )
  )
})

test_that("a path with a component . or .., or outside config$path, stops before any runner call", {
  root <- local_share_root()
  write_local(file.path(root, "output", "f.txt"), "x")
  calls <- 0
  local_mocked_bindings(share_smbclient = function(commands) {
    calls <<- calls + 1
    list(status = 0L, stdout = "", stderr = "")
  })

  # output/a/../f.txt stays under config$path, so only the check of each
  # component can stop it.
  error_message <- function(expr) {
    tryCatch(
      {
        force(expr)
        "no error"
      },
      error = conditionMessage
    )
  }
  messages <- c(
    error_message(path_put("output", "a", "..", "f.txt", auto = TRUE)),
    error_message(path_get("output", "a", "..", "f.txt", auto = TRUE)),
    error_message(path_ls("output", "a", "..", auto = TRUE)),
    error_message(path_get("..", "..", "etc", auto = TRUE)),
    error_message(path_get("output", ".", "f.txt", auto = TRUE))
  )
  expect_equal(calls, 0)
  expect_match(messages, "has a path component '.' or '..'", fixed = TRUE)

  # path() always starts with config$path, so a path outside it needs a
  # replaced path().
  local_mocked_bindings(path = function(...) "/elsewhere/f.txt")
  expect_error(
    path_get("f.txt", auto = TRUE),
    "'/elsewhere/f.txt' is not under cs9::config$path",
    fixed = TRUE
  )
  config$path <- ""
  expect_error(path_get("output", auto = TRUE), "config\\$path, which is empty")
  expect_equal(calls, 0)
})

test_that("a name with a double quote, a backslash, a wildcard or a semicolon stops before any runner call", {
  root <- local_share_root()
  calls <- 0
  local_mocked_bindings(share_smbclient = function(commands) {
    calls <<- calls + 1
    list(status = 0L, stdout = "", stderr = "")
  })
  for (name in c("a\"b.txt", "a*.txt", "a?.txt", "a;b.txt")) {
    expect_error(path_get("output", name, auto = TRUE), root, fixed = TRUE)
  }
  expect_equal(calls, 0)
  # fs::path() turns a backslash into a slash on Windows, so check the
  # names directly.
  expect_error(
    share_check_name_parts(c("output", "a\\b.txt"), "x"),
    "a name that contains"
  )
  expect_error(share_check_name_parts("a\"b", "x"), "a name that contains")
  expect_no_error(share_check_name_parts(c("epistatusmøte", "uke 45"), "x"))
  # With a share, a missing local file is an error before any runner call.
  expect_error(
    path_put("output", "missing.txt", auto = TRUE),
    paste0(root, "/output/missing.txt"),
    fixed = TRUE
  )
  expect_equal(calls, 0)
})

test_that("path_get() downloads a folder recursively and checks every size", {
  root <- local_share_root()
  fake <- fake_share(
    files = list(
      "input\\weather\\a.csv" = "1,2,3",
      "input\\weather\\uke 45\\b ø.csv" = "4,5",
      "input\\weather\\uke 45\\deep\\c.txt" = "c"
    ),
    dirs = c(
      "input",
      "input\\weather",
      "input\\weather\\uke 45",
      "input\\weather\\uke 45\\deep",
      "input\\weather\\empty"
    )
  )
  local_mocked_bindings(share_smbclient = fake$run)

  res <- path_get("input", "weather", auto = TRUE)
  expect_equal(res, paste0(root, "/input/weather"))
  expect_equal(readLines(file.path(res, "a.csv"), warn = FALSE), "1,2,3")
  expect_equal(
    readLines(file.path(res, "uke 45", "b ø.csv"), warn = FALSE),
    "4,5"
  )
  expect_equal(
    readLines(file.path(res, "uke 45", "deep", "c.txt"), warn = FALSE),
    "c"
  )
  expect_true(dir.exists(file.path(res, "empty")))
  expect_equal(sum(fake$verbs() == "get"), 3)
})

test_that("path_get() downloads one file", {
  root <- local_share_root()
  fake <- fake_share(
    files = list("input\\a b.csv" = "1,2,3"),
    dirs = "input"
  )
  local_mocked_bindings(share_smbclient = fake$run)

  res <- path_get("input", "a b.csv", auto = TRUE)
  expect_equal(res, paste0(root, "/input/a b.csv"))
  expect_equal(readLines(res, warn = FALSE), "1,2,3")
  expect_equal(fake$verbs(), c("ls", "get"))
})

test_that("path_get() stops naming the path when a downloaded size differs", {
  root <- local_share_root()
  fake <- fake_share(
    files = list("input\\weather\\a.csv" = "1,2,3"),
    dirs = c("input", "input\\weather"),
    get_drop = 1
  )
  local_mocked_bindings(share_smbclient = fake$run)

  err <- expect_error(path_get("input", "weather", auto = TRUE))
  expect_match(
    conditionMessage(err),
    paste0(root, "/input/weather"),
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    paste0(root, "/input/weather/a.csv"),
    fixed = TRUE
  )
})

test_that("path_get() stops naming the path when the remote path does not exist", {
  root <- local_share_root()
  fake <- fake_share(dirs = "input")
  local_mocked_bindings(share_smbclient = fake$run)

  expect_error(
    path_get("input", "nothing.csv", auto = TRUE),
    paste0("'", root, "/input/nothing.csv' does not exist on the share."),
    fixed = TRUE
  )
  expect_equal(fake$verbs(), "ls")
})

test_that("path_ls() lists the entries of a share folder with typed columns", {
  root <- local_share_root()
  fake <- fake_share(
    files = list("output\\uke 45 rapport.xlsx" = "12345"),
    dirs = c("output", "output\\epistatusmøte")
  )
  local_mocked_bindings(share_smbclient = fake$run)

  x <- path_ls("output", auto = TRUE)
  expect_s3_class(x, "data.table")
  expect_equal(names(x), c("path", "type", "size", "modification_time"))
  expect_equal(
    x$path,
    paste0(root, "/output/", c("epistatusmøte", "uke 45 rapport.xlsx"))
  )
  expect_equal(x$type, c("directory", "file"))
  expect_equal(x$size, c(0, 5))
  expect_equal(
    x$modification_time,
    as.POSIXct(rep("2026-11-10 13:38:24", 2), tz = "UTC")
  )
  expect_equal(fake$log[[1]], c("ls", "output\\*"))

  expect_error(
    path_ls("missing", auto = TRUE),
    paste0("'", root, "/missing' does not exist on the share."),
    fixed = TRUE
  )
})
