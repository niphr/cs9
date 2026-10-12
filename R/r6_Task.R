Task <- R6::R6Class(
  "Task",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    permission = NULL,
    plans = list(),
    tables = list(),
    cores = 1,
    upsert_at_end_of_each_plan = FALSE,
    insert_at_end_of_each_plan = FALSE,
    name = NULL,
    name_grouping = NULL,
    name_action = NULL,
    name_variant = NULL,
    update_plans_fn = NULL,
    action_before_fn = NULL,
    action_after_fn = NULL,
    ss = "unspecified",
    implementation_version = "unspecified",
    initialize = function(
      name_grouping = NULL,
      name_action = NULL,
      name_variant = NULL,
      permission = NULL,
      plans = NULL,
      update_plans_fn = NULL,
      tables,
      cores = 1,
      upsert_at_end_of_each_plan = FALSE,
      insert_at_end_of_each_plan = FALSE,
      action_before_fn = NULL,
      action_after_fn = NULL,
      fixed_plans_fn = NULL
    ) {
      private$fixed_plans_fn <- fixed_plans_fn
      self$name <- paste0(
        c(name_grouping, name_action, name_variant),
        collapse = "_"
      )
      self$name_grouping <- name_grouping
      self$name_action <- name_action
      self$name_variant <- name_variant
      self$permission <- permission
      self$plans <- plans
      self$update_plans_fn <- update_plans_fn
      self$tables <- tables
      self$cores <- cores
      self$upsert_at_end_of_each_plan <- upsert_at_end_of_each_plan
      self$insert_at_end_of_each_plan <- insert_at_end_of_each_plan
      self$action_before_fn <- action_before_fn
      return(self$action_after_fn <- action_after_fn)
    },
    insert_first_last_analysis = function() {
      if (is.null(self$plans)) {
        return()
      }
      if (length(self$plans) == 0) {
        return()
      }
      if (is.null(self$plans[[1]]$analyses)) {
        return()
      }
      if (length(self$plans[[1]]$analyses) == 0) {
        return()
      }
      if (is.null(self$plans[[1]]$analyses[[1]]$argset)) {
        return()
      }
      if (!is.null(self$plans[[1]]$analyses[[1]]$argset$first_analysis)) {
        return()
      }

      for (i in seq_along(self$plans)) {
        for (j in seq_along(self$plans[[i]]$analyses)) {
          self$plans[[i]]$set_use_foreach(FALSE)
          self$plans[[i]]$analyses[[j]]$argset$index_plan <- i
          self$plans[[i]]$analyses[[j]]$argset$index_analysis <- j

          if (i == 1 && j == 1) {
            self$plans[[i]]$analyses[[j]]$argset$first_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[j]]$argset$first_analysis <- FALSE
          }

          if (
            i == length(self$plans) && j == length(self$plans[[i]]$analyses)
          ) {
            self$plans[[i]]$analyses[[j]]$argset$last_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[j]]$argset$last_analysis <- FALSE
          }

          if (j == 1) {
            self$plans[[i]]$analyses[[
              j
            ]]$argset$within_plan_first_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[
              j
            ]]$argset$within_plan_first_analysis <- FALSE
          }

          if (j == length(self$plans[[i]]$analyses)) {
            self$plans[[i]]$analyses[[
              j
            ]]$argset$within_plan_last_analysis <- TRUE
          } else {
            self$plans[[i]]$analyses[[
              j
            ]]$argset$within_plan_last_analysis <- FALSE
          }
        }
      }
      return(invisible(NULL))
    },
    # A task with a plan analysis builds its plans with update_plans_fn.
    # get_task() and the shortcut_get_*() methods build them once and then
    # reuse them, so the plan indexes stay the same between interactive calls.
    # SurveillanceSystem_v9$run_task() passes replan = TRUE, because the data
    # that the plan analysis reads can change between two runs in one session.
    # A task with fixed plans builds them again with fixed_plans_fn when
    # replan = TRUE, so that argset$today is the date of the run and not the
    # date of registration. update_plans_fn stays NULL for that task, because
    # callers read a non-NULL update_plans_fn as "this task has a plan
    # analysis".
    update_plans = function(replan = FALSE) {
      if (!is.null(self$update_plans_fn) && (replan || !private$plans_built)) {
        message(glue::glue("Updating plans..."))
        self$plans <- self$update_plans_fn()
        private$plans_built <- TRUE
      } else if (replan && !is.null(private$fixed_plans_fn)) {
        self$plans <- private$fixed_plans_fn()
      }
      return(self$insert_first_last_analysis())
    },
    num_plans = function() {
      return(length(self$plans))
    },
    num_analyses = function() {
      retval <- 0
      for (i in seq_along(plans)) {
        retval <- retval + plans[[i]]$x_length()
      }
      return(retval)
    },
    # `run_key` names the run. A run with a key stages its writes and
    # publishes them after the last plan. A second call with the same key
    # resumes the first: it skips each plan that the first call finished,
    # except plan 1, and publishes once. NULL stages nothing: the run writes
    # its tables directly, as before 26.10.14. A run with a key always builds
    # its plans again, so every attempt compares its plan list with the list
    # of the first attempt. Without a key, `replan = TRUE` builds the
    # plans again, after the run has its date. SurveillanceSystem_v9$run_task()
    # passes it.
    run = function(cores = self$cores, run_key = NULL, replan = FALSE) {
      # `cores` MAY be a function with no arguments. It is called here, so it
      # reads its settings when the task runs. A runner that sets
      # `self$cores <- 1` replaces the function.
      if (is.function(cores)) {
        cores <- cores()
      }

      status <- "failed"
      start_datetime <- lubridate::now()
      on.exit({
        if (status == "failed") {
          ram_max_used_mb <- 0
        }
        update_config_tasks_stats(
          ss = self$ss,
          task = self$name,
          implementation_version = self$implementation_version,
          cores_n = cores,
          plans_n = self$num_plans(),
          analyses_n = self$num_analyses(),
          start_datetime = start_datetime,
          stop_datetime = lubridate::now(),
          ram_all_cores_mb = round(ram_max_used_mb, 1),
          ram_per_core_mb = round(ram_max_used_mb / cores, 1),
          status = status
        )
      })

      # task <- tm_get_task("analysis_norsyss_qp_gastro")
      message(glue::glue("task: {self$name}"))
      if (!is.null(self$permission)) {
        if (!self$permission$has_permission()) {
          return(NULL)
        }
      }

      upsert_at_end_of_each_plan <- self$upsert_at_end_of_each_plan

      # A run with a run_key stages every write to the task's own tables, and
      # publishes them after the last plan. See R/run_state.R.
      run <- run_begin(self$name, self$tables, run_key)
      on.exit(run_end(run), add = TRUE)
      if (isTRUE(run$published)) {
        message(glue::glue(
          "Run '{run$run_key}' of task {self$name} is published. Nothing to do."
        ))
        ram_max_used_mb <- 0
        status <- "succeeded"
        return(invisible(NULL))
      }
      config$run <- run

      # A plan MAY read published tables only, so staged_tbl() stops here.
      # A keyed run always builds its plans again: see the comment above.
      config$planning <- TRUE
      self$update_plans(replan = replan || !is.null(run_key))
      config$planning <- FALSE

      update_config_log(
        ss = self$ss,
        task = self$name,
        message = glue::glue(
          "Running task={self$name} with plans={length(self$plans)} and analyses={self$num_analyses()}"
        )
      )
      if (self$num_analyses() == 0) {
        message("Quitting because there is nothing to do (0 analyses)")
        return()
      }

      # TRUE for each plan that this attempt runs. An attempt that resumes an
      # earlier one skips the plans it finished, except plan 1.
      to_run <- rep(TRUE, length(self$plans))
      private$plan_ids <- NULL
      if (!is.null(run)) {
        private$plan_ids <- run_plan_ids(
          self$plans,
          private$injected_fields(),
          self$name
        )
        config$run$first_plan <- private$plan_ids[1]
        to_run <- run_prepare(config$run, private$plan_ids, self$tables)
        if (!all(to_run)) {
          message(glue::glue(
            "Resuming run '{run$run_key}': {sum(!to_run)} of ",
            "{length(to_run)} plans are done and are skipped"
          ))
        }
      }

      if (cores == 1 || (length(self$plans) >= 2 && length(self$plans) <= 3)) {
        run_type <- "sequential"
        run_description <- "plans=sequential, argset=sequential"
        cores <- 1
      } else if (length(self$plans) == 1) {
        # in theory, the inner loop should be parallelized
        # but this is not implemented yet
        run_type <- "sequential"
        run_description <- "plans=sequential, argset=sequential"
        cores <- 1
      } else if (interactive()) {
        run_type <- "sequential"
        run_description <- "plans=sequential, argset=sequential"
        cores <- 1
      } else {
        run_type <- "parallel_plans"
        run_description <- "plans=multicore, analyses=sequential"
      }

      if (!is.null(self$action_before_fn)) {
        message("Running action_before_fn")
        self$action_before_fn()
      }

      message(glue::glue("{run_description} with cores={cores}"))

      a0 <- Sys.time()

      if (run_type == "sequential") {
        # not running in parallel
        progressr::with_progress(
          {
            pb <- progressr::progressor(steps = self$num_analyses())
            private$run_sequential(
              plans_index = which(to_run),
              tables = self$tables,
              upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
              insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
              pb = pb,
              cores = cores
            )
          },
          handlers = progressr_handler(),
          interval = 10,
          delay_stdout = FALSE,
          delay_conditions = ""
        )

        ram_max_used_mb <- gc(reset = FALSE)[, 6] |> sum()
      } else if (run_type == "parallel_plans") {
        # running in parallel

        message(
          "Running plans 1 and ",
          length(self$plans),
          " sequentially, and 2:",
          length(self$plans) - 1,
          " in parallel\n"
        )

        message("*****")
        message("*****")
        message(
          "***** Running plan 1 sequentially at ",
          lubridate::now(),
          " *****"
        )
        # pb <- progressr::progressor(steps = self$plans[[1]]$x_length())
        private$run_sequential(
          plans_index = 1,
          tables = self$tables,
          upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
          cores = cores
        )
        b0 <- Sys.time()
        message(
          "\nPlan 1 ran in ",
          round(as.numeric(difftime(b0, a0, units = "mins")), 1),
          " mins\n"
        )

        message("*****")
        message("*****")
        message(
          "***** Running plans 2:",
          (length(self$plans) - 1),
          " in parallel at ",
          lubridate::now(),
          " *****"
        )

        middle <- 2:(length(self$plans) - 1)
        middle <- middle[to_run[middle]]
        if (length(middle) > 0) {
          private$run_parallel_plans(
            plans_index = middle,
            tables = self$tables,
            upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
            insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
            cores = cores
          )
        }

        message("\n*****")
        message("*****")
        message(
          "***** Running plan ",
          length(self$plans),
          " sequentially at ",
          lubridate::now(),
          " *****"
        )
        a1 <- Sys.time()
        private$run_sequential(
          plans_index = which(to_run)[which(to_run) == length(self$plans)],
          tables = self$tables,
          upsert_at_end_of_each_plan = self$upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan = self$insert_at_end_of_each_plan,
          cores = cores
        )
        b1 <- Sys.time()
        message(
          "\nPlan ",
          length(self$plans),
          " ran in ",
          round(as.numeric(difftime(b1, a1, units = "mins")), 1),
          " mins"
        )

        ram_max_used_mb <- gc(reset = FALSE)[, 6] |> sum()
        ram_max_used_mb <- ram_max_used_mb * cores
      }

      b1 <- Sys.time()
      message(
        "Task ran in ",
        round(as.numeric(difftime(b1, a0, units = "mins")), 1),
        " mins\n"
      )

      data.table::setDTthreads()

      if (!is.null(self$action_after_fn)) {
        message("Running action_after_fn")
        if (!is.null(run)) {
          config$run$plan_id <- run_after_plans
        }
        self$action_after_fn()
      }

      if (!is.null(run)) {
        message(glue::glue(
          "Publishing run '{run$run_key}' of task {self$name}"
        ))
        run_publish(config$run, self$tables)
      }

      status <- "succeeded"
      if (!is.null(self$permission)) return(self$permission$revoke_permission())
    }
  ),
  private = list(
    plans_built = FALSE,
    fixed_plans_fn = NULL,
    # The id of each plan in the run in progress. See R/run_state.R.
    plan_ids = NULL,
    # A task from add_task() gets index, today and yesterday from cs9.
    injected_fields = function() {
      return(run_injected_fields(
        !is.null(self$update_plans_fn) || !is.null(private$fixed_plans_fn)
      ))
    },
    run_sequential = function(
      plans_index,
      tables,
      upsert_at_end_of_each_plan,
      insert_at_end_of_each_plan,
      pb = NULL,
      cores
    ) {
      # for (s in tables) s$connect()
      for (i in seq_along(self$plans[plans_index])) {
        if (!is.null(pb)) {
          self$plans[plans_index][[i]]$set_progressor(pb)
        }
        config$plan_attempt_index <- 1
        if (!is.null(config$run)) {
          config$run$plan_id <- private$plan_ids[plans_index[i]]
        }

        if (length(plans_index) == 1 && is.null(pb)) {
          verbose <- TRUE
        } else {
          verbose <- FALSE
        }

        data <- self$plans[plans_index][[i]]$get_data()
        hashes <- data$hash
        last_run_hashes <- get_last_run_data_hash_split_into_plnr_format(
          task = self$name,
          index_plan = plans_index[i],
          expected_element_tags = names(data$hash$current_elements)
        )
        data$hash$last_run <- last_run_hashes$last_run
        data$hash$last_run_elements <- last_run_hashes$last_run_elements

        retval <- self$plans[plans_index][[i]]$run_all_with_data(
          data = data,
          tables = tables
        )

        if (upsert_at_end_of_each_plan) {
          retval <- csutil::unnest_dfs_within_list_of_fully_named_lists(
            retval,
            returned_name_when_dfs_are_not_nested = "output",
            use.names = TRUE,
            fill = TRUE
          )
          for (df_name in names(retval)) {
            tables[[df_name]]$upsert_data(retval[[df_name]], verbose = verbose)
          }
        }
        if (insert_at_end_of_each_plan) {
          retval <- csutil::unnest_dfs_within_list_of_fully_named_lists(
            retval,
            returned_name_when_dfs_are_not_nested = "output",
            use.names = TRUE,
            fill = TRUE
          )
          for (df_name in names(retval)) {
            tables[[df_name]]$insert_data(retval[[df_name]], verbose = verbose)
          }
        }

        rm("retval")

        update_config_data_hash_for_each_plan(
          task = self$name,
          index_plan = plans_index[i],
          element_tag = names(hashes$current_elements),
          element_hash = unlist(hashes$current_elements),
          all_hash = hashes$current
        )
        rm("data")
        # The done mark comes after every write of the plan.
        if (!is.null(config$run)) {
          run_mark_plan_done(config$run$plan_id, tables)
          config$run$plan_id <- NULL
        }
      }
      for (s in tables) {
        s$disconnect()
      }
      for (s in config$tables) {
        s$disconnect()
      }
      return(invisible(NULL))
    },
    run_parallel_plans = function(
      plans_index,
      tables,
      upsert_at_end_of_each_plan,
      insert_at_end_of_each_plan,
      cores
    ) {
      y <- pbmcapply::pbmclapply(
        self$plans[plans_index],
        function(
          x,
          tables,
          upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan
        ) {
          config$in_parallel <- TRUE # this will stop TABLOCK from being used in in/upserts
          data.table::setDTthreads(1)
          x$set_verbose(FALSE)
          message(".")

          # this tries to set a different seed for each parallel run
          # so that there isn't a race condition in any of the parallel
          # processes
          set.seed(x$get_argset(1)$index)
          # for (s in tables) s$disconnect()

          # Each plan runs once. The scheduler retries the whole task.
          config$plan_attempt_index <- 1
          # The id from the argsets, as run_plan_ids() computed it.
          if (!is.null(config$run)) {
            config$run$plan_id <- run_plan_id(x, private$injected_fields())
          }

          catch_result <- tryCatch(
            {
              # for (s in tables) s$connect()
              data <- x$get_data()
              hashes <- data$hash
              last_run_hashes <- get_last_run_data_hash_split_into_plnr_format(
                task = self$name,
                index_plan = x$get_argset(1)$index_plan,
                expected_element_tags = names(data$hash$current_elements)
              )
              data$hash$last_run <- last_run_hashes$last_run
              data$hash$last_run_elements <- last_run_hashes$last_run_elements

              retval <- x$run_all_with_data(data = data, tables = tables)

              if (upsert_at_end_of_each_plan) {
                retval <- csutil::unnest_dfs_within_list_of_fully_named_lists(
                  retval,
                  returned_name_when_dfs_are_not_nested = "output",
                  use.names = TRUE,
                  fill = TRUE
                )
                for (df_name in names(retval)) {
                  tables[[df_name]]$upsert_data(
                    retval[[df_name]],
                    verbose = FALSE
                  )
                }
              }
              if (insert_at_end_of_each_plan) {
                retval <- csutil::unnest_dfs_within_list_of_fully_named_lists(
                  retval,
                  returned_name_when_dfs_are_not_nested = "output",
                  use.names = TRUE,
                  fill = TRUE
                )
                for (df_name in names(retval)) {
                  tables[[df_name]]$insert_data(
                    retval[[df_name]],
                    verbose = FALSE
                  )
                }
              }

              rm("retval")

              # this might break things!!!!!!
              update_config_data_hash_for_each_plan(
                task = self$name,
                index_plan = x$get_argset(1)$index_plan,
                element_tag = names(hashes$current_elements),
                element_hash = unlist(hashes$current_elements),
                all_hash = hashes$current
              )
              rm("data")
              if (!is.null(config$run)) {
                run_mark_plan_done(config$run$plan_id, tables)
              }

              return(list(
                error = FALSE,
                msg = "succeeded"
              ))
            },
            error = function(e) {
              return(list(
                error = TRUE,
                msg = paste0(
                  "Error in index ",
                  x$get_argset(1)$index,
                  ".\n********\n",
                  e$message,
                  "\n********\n"
                )
              ))
            }
          )
          # for (s in tables) s$disconnect()
          if (catch_result$error) {
            stop(catch_result$msg)
          }

          # ***************************** #
          # NEVER DELETE gc()             #
          # IT CAUSES 2x SPEEDUP          #
          # AND 10x MEMORY EFFICIENCY     #
          gc() #
          # ***************************** #
          return(1)
        },
        tables = tables,
        upsert_at_end_of_each_plan = upsert_at_end_of_each_plan,
        insert_at_end_of_each_plan = insert_at_end_of_each_plan,
        ignore.interactive = TRUE,
        mc.cores = cores,
        # Each plan gets its own fork. A prescheduled core stops at its first
        # error, which skipped the other plans on that core.
        mc.preschedule = FALSE,
        mc.style = "ETA",
        mc.substyle = 2
      )
      config$in_parallel <- FALSE # this will allow TABLOCK in in/upserts
      if (!is.null(config$run)) {
        config$run$plan_id <- NULL
      }

      # The error holds the message of every failed plan, in plan order.
      try_error_index <- vapply(
        y,
        function(x) inherits(x, "try-error"),
        logical(1)
      )
      if (any(try_error_index)) {
        stop(
          "Error running in parallel: ",
          sum(try_error_index),
          " of ",
          length(y),
          " parallel plans failed.\n",
          paste(
            vapply(y[try_error_index], function(x) x[1], character(1)),
            collapse = "\n"
          ),
          call. = FALSE
        )
      }
      # print(y)
    },
    # run_parallel = function(plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb){
    #   y <- foreach(x = self$plans[plans_index]) %dopar% {
    #     data.table::setDTthreads(1)
    #
    #     for (s in tables) s$connect()
    #     x$set_progressor(pb)
    #     retval <- x$run_all(tables = tables)
    #
    #     if (upsert_at_end_of_each_plan) {
    #       retval <- rbindlist(retval)
    #       tables$output$upsert_data(retval, verbose = F)
    #     }
    #
    #     if (insert_at_end_of_each_plan) {
    #       retval <- rbindlist(retval)
    #       tables$output$insert_data(retval, verbose = F)
    #     }
    #     rm("retval")
    #     for (s in tables) s$db_disconnect()
    #
    #     # ***************************** #
    #     # NEVER DELETE gc()             #
    #     # IT CAUSES 2x SPEEDUP          #
    #     # AND 10x MEMORY EFFICIENCY     #
    #     gc() #
    #     # ***************************** #
    #     1
    #   }
    # },
    run_parallel = function(
      plans_index,
      tables,
      upsert_at_end_of_each_plan,
      insert_at_end_of_each_plan,
      pb
    ) {
      # y <- future.apply::future_lapply(
      #   self$plans[plans_index],
      #   function(x, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, pb){
      #     data.table::setDTthreads(1)
      #
      #     for (s in tables) s$connect()
      #     x$set_progressor(pb)
      #     retval <- x$run_all(tables = tables)
      #
      #     if (upsert_at_end_of_each_plan) {
      #       retval <- rbindlist(retval)
      #       tables$output$upsert_data(retval, verbose = F)
      #     }
      #
      #     if (insert_at_end_of_each_plan) {
      #       retval <- rbindlist(retval)
      #       tables$output$insert_data(retval, verbose = F)
      #     }
      #     rm("retval")
      #     for (s in tables) s$db_disconnect()
      #
      #     # ***************************** #
      #     # NEVER DELETE gc()             #
      #     # IT CAUSES 2x SPEEDUP          #
      #     # AND 10x MEMORY EFFICIENCY     #
      #     gc() #
      #     # ***************************** #
      #     1
      #   },
      #   tables = tables,
      #   upsert_at_end_of_each_plan = upsert_at_end_of_each_plan,
      #   insert_at_end_of_each_plan = insert_at_end_of_each_plan,
      #   pb = pb
      # )
      return(
        y <- pbmcapply::pbmcmapply(
          function(
            x,
            tables,
            upsert_at_end_of_each_plan,
            insert_at_end_of_each_plan,
            pb
          ) {
            data.table::setDTthreads(1)

            # for (s in tables) s$connect()
            x$set_progressor(pb)
            retval <- x$run_all(tables = tables)

            if (upsert_at_end_of_each_plan) {
              retval <- rbindlist(retval)
              tables$output$upsert_data(retval, verbose = FALSE)
            }

            if (insert_at_end_of_each_plan) {
              retval <- rbindlist(retval)
              tables$output$insert_data(retval, verbose = FALSE)
            }
            rm("retval")
            # for (s in tables) s$db_disconnect()

            # ***************************** #
            # NEVER DELETE gc()             #
            # IT CAUSES 2x SPEEDUP          #
            # AND 10x MEMORY EFFICIENCY     #
            gc() #
            # ***************************** #
            return(1)
          },
          self$plans[plans_index],
          MoreArgs = list(
            tables,
            upsert_at_end_of_each_plan,
            insert_at_end_of_each_plan,
            pb
          ),
          ignore.interactive = TRUE,
          mc.cores = 2
        )
      )
      # y <- foreach(x = self$plans[plans_index]) %dopar% {
      #   data.table::setDTthreads(1)
      #
      #   for (s in tables) s$connect()
      #   x$set_progressor(pb)
      #   retval <- x$run_all(tables = tables)
      #
      #   if (upsert_at_end_of_each_plan) {
      #     retval <- rbindlist(retval)
      #     tables$output$upsert_data(retval, verbose = F)
      #   }
      #
      #   if (insert_at_end_of_each_plan) {
      #     retval <- rbindlist(retval)
      #     tables$output$insert_data(retval, verbose = F)
      #   }
      #   rm("retval")
      #   for (s in tables) s$db_disconnect()
      #
      #   # ***************************** #
      #   # NEVER DELETE gc()             #
      #   # IT CAUSES 2x SPEEDUP          #
      #   # AND 10x MEMORY EFFICIENCY     #
      #   gc() #
      #   # ***************************** #
      #   1
      # }
    }
  )
)
