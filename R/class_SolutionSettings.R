#' @include internal.R class_Weight.R class_Theme.R class_Include.R
NULL

#' Solution settings class
#'
#' Definition for the `SolutionSettings` class.
SolutionSettings <- R6::R6Class(
  "SolutionSettings",
  public = list(

    #' @field theme_ids `character` vector of identifiers for the themes.
    theme_ids = character(0),

    #' @field weight_ids `character` vector of identifiers for the weights.
    weight_ids = character(0),

    #' @field include_ids `character` vector of identifiers for the includes.
    include_ids = character(0),

    #' @field parameter_ids `character` vector of identifiers for the
    #'  parameters.
    parameter_ids = character(0),

    #' @field themes `list` of [Theme] objects.
    themes = NULL,

    #' @field weights `list` of [Weight] objects.
    weights = NULL,

    #' @field includes `list` of [Include] objects.
    includes = NULL,

    #' @field parameters `list` of [Parameter] objects.
    parameters = NULL,

    #' @description
    #' Create a `SolutionSettings` object.
    #' @param themes `list` of [Theme] objects.
    #' @param weights `list` of [Weight] objects.
    #' @param includes `list` of [Include] objects.
    #' @param parameters `list` of [Parameter] objects.
    #' @return A new `SolutionSettings` object.
    initialize = function(themes, weights, includes, parameters) {
      
      assertthat::assert_that(
        is.list(themes),
        is.list(weights),
        all_list_elements_inherit(themes, "Theme"),
        all_list_elements_inherit(weights, "Weight"),
        all_list_elements_inherit(includes, "Include"),
        all_list_elements_inherit(parameters, "Parameter")
      )
      self$themes <- themes
      self$weights <- weights
      self$includes <- includes
      self$parameters <- parameters
      self$theme_ids <- vapply(themes, `[[`, character(1), "id")
      self$weight_ids <- vapply(weights, `[[`, character(1), "id")
      self$include_ids <- vapply(includes, `[[`, character(1), "id")
      self$parameter_ids <- vapply(parameters, `[[`, character(1), "id")
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("SolutionSettings")
      # print themes
      if (length(self$themes) > 0) {
        message("  themes: ")
        for (x in vapply(self$themes, function(x) x$repr(), character(1))) {
          message("    ", gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  themes: none")
      }
      ## print weights
      if (length(self$weights) > 0) {
        message("  weights: ")
        for (x in vapply(self$weights, function(x) x$repr(), character(1))) {
          message("    ", gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  weights: none")
      }
      ## print includes
      if (length(self$includes) > 0) {
        message("  includes: ")
        for (x in vapply(self$includes, function(x) x$repr(), character(1))) {
          message("    ", gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  includes: none")
      }
      ## print parameters
      if (length(self$parameters) > 0) {
        message("  parameters: ")
        for (x in vapply(self$parameters, function(x) x$repr(), character(1))) {
          message("    ", gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  parameters: none")
      }
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      "SolutionSettings object"
    },

    #' @description
    #' Get a theme.
    #' @param value `character` theme identifier.
    #' @return [Theme] object.
    get_theme = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value)
      )
      assertthat::assert_that(
        value %in% self$theme_ids,
        msg = paste0("no theme with the id `", value, "`")
      )
      self$themes[[which(self$theme_ids == value)]]
    },

    #' @description
    #' Get a weight.
    #' @param value `character` weight identifier.
    #' @return [Weight] object.
    get_weight = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value)
      )
      assertthat::assert_that(
        value %in% self$weight_ids,
        msg = paste0("no weight with the id `", value, "`")
      )
      self$weights[[which(self$weight_ids == value)]]
    },

    #' @description
    #' Get an include.
    #' @param value `character` weight identifier.
    #' @return [Include] object.
    get_include = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value)
      )
      assertthat::assert_that(
        value %in% self$include_ids,
        msg = paste0("no include with the id `", value, "`")
      )
      self$includes[[which(self$include_ids == value)]]
    },

    #' @description
    #' Get an parameter.
    #' @param value `character` weight identifier.
    #' @return [Parameter] object.
    get_parameter = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value)
      )
      assertthat::assert_that(
        value %in% self$parameter_ids,
        msg = paste0("no parameter with the id `", value, "`")
      )
      self$parameters[[which(self$parameter_ids == value)]]
    },

    #' @description
    #' Get a setting for a weight, theme, include, or parameter.
    #' @param value `list` with new parameter information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` identifier for theme, weight, or include.}
    #' \item{setting}{`character` name of parameter.
    #'   Available options are: `"status"`, `"factor"`, `"value"`, or `"goal"`.}
    #' \item{type}{`character` indicating the type of setting.
    #'   Available options are: `"theme"`, `"weight"`, `"include"`,
    #'   `"parameter"`.}
    #' }
    get_setting = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "id"),
        assertthat::is.string(value$id),
        assertthat::has_name(value, "setting"),
        assertthat::is.string(value$setting),
        assertthat::has_name(value, "type"),
        assertthat::is.string(value$type),
        isTRUE(value$type %in% c("theme", "weight", "include", "parameter"))
      )
      if (identical(value$type, "theme")) {
        self$get_theme(value$id)$get_setting(value$setting)
      } else if (identical(value$type, "weight")) {
        self$get_weight(value$id)$get_setting(value$setting)
      } else if (identical(value$type, "include")) {
        self$get_include(value$id)$get_setting(value$setting)
      } else if (identical(value$type, "parameter")) {
        self$get_parameter(value$id)$get_setting(value$setting)
      }
    },

    #' @description
    #' Set a setting for a weight or theme.
    #' @param value `list` with new setting information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` identifier for theme, weight, or include.}
    #' \item{setting}{`character` name of parameter.
    #'   Available options are: `"status"`, `"factor"`, `"value"`, or `"goal"`.}
    #' \item{value}{`numeric` or `logical` value for new setting.}
    #' \item{type}{`character` indicating the type of setting.
    #'   Available options are: `"theme"`, `"weight"`, `"include"`,
    #'   `"parameter"`.}
    #' }
    set_setting = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "id"),
        assertthat::is.string(value$id),
        assertthat::has_name(value, "setting"),
        assertthat::is.string(value$setting),
        assertthat::has_name(value, "value"),
        assertthat::is.string(value$type),
        isTRUE(value$type %in% c("theme", "weight", "include", "parameter"))
      )
      if (identical(value$type, "theme")) {
        self$get_theme(value$id)$set_setting(value$setting, value$value)
      } else if (identical(value$type, "weight")) {
        self$get_weight(value$id)$set_setting(value$setting, value$value)
      } else if (identical(value$type, "include")) {
        self$get_include(value$id)$set_setting(value$setting, value$value)
      } else if (identical(value$type, "parameter")) {
        self$get_parameter(value$id)$set_setting(value$setting, value$value)
      }
    },

    #' @description
    #' Get data for displaying the theme in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        themes =
          lapply(
            self$themes, function(x) x$get_solution_settings_widget_data()
          ),
        weights =
          lapply(
            self$weights, function(x) x$get_solution_settings_widget_data()
          ),
        includes =
          lapply(
            self$includes, function(x) x$get_solution_settings_widget_data()
          ),
        parameters =
          lapply(
            self$parameters, function(x) x$get_widget_data()
          )
      )
    },

    #' @description
    #' Get theme settings for generating a prioritization.
    #' @return [tibble::tibble()] with data.
    get_theme_settings = function() {
      tibble::tibble(
        id =
          do.call(c, lapply(self$themes, function(x) x$get_feature_id())),
        name =
          do.call(c, lapply(self$themes, function(x) x$get_feature_name())),
        status =
          do.call(c, lapply(self$themes, function(x) x$get_feature_status())),
        goal =
          do.call(c, lapply(self$themes, function(x) x$get_feature_goal())),
        current =
          do.call(c, lapply(self$themes, function(x) x$get_feature_current())),
        limit =
          do.call(c, lapply(self$themes, function(x) x$get_feature_limit())),
        total =
          do.call(c, lapply(self$themes, function(x) x$get_feature_total()))
      )
    },

    #' @description
    #' Get weight settings for generating a prioritization.
    #' @return [tibble::tibble()] with data.
    get_weight_settings = function() {
      tibble::tibble(
        id = vapply(self$weights, `[[`, character(1), "id"),
        name = vapply(self$weights, `[[`, character(1), "name"),
        status = vapply(self$weights, `[[`, logical(1), "status"),
        factor = vapply(self$weights, `[[`, numeric(1), "factor")
      )
    },

    #' @description
    #' Get include settings for generating a prioritization.
    #' @return [tibble::tibble()] with data.
    get_include_settings = function() {
      tibble::tibble(
        id = vapply(self$includes, `[[`, character(1), "id"),
        name = vapply(self$includes, `[[`, character(1), "name"),
        status = vapply(self$includes, `[[`, logical(1), "status")
      )
    },

    #' @description
    #' Get parameter settings for generating a prioritization.
    #' @return [tibble::tibble()] with data.
    get_parameter_settings = function() {
      tibble::tibble(
        id = vapply(self$parameters, `[[`, character(1), "id"),
        name = vapply(self$parameters, `[[`, character(1), "name"),
        status = vapply(self$parameters, `[[`, logical(1), "status"),
        value = vapply(self$parameters, `[[`, numeric(1), "value")
      )
    },

    #' @description
    #' Get theme matrix data.
    #' @return [Matrix::sparseMatrix()] with data.
    get_theme_data = function() {
      v <- lapply(self$themes, function(x) {
        lapply(x$feature, `[[`, "variable")
      })
      fid <- lapply(self$themes, function(x) {
        lapply(x$feature, `[[`, "id")
      })
      fid <- base::unlist(fid, recursive = TRUE, use.names = FALSE)
      out <- extract_data_matrix(do.call(c, v))
      rownames(out) <- fid
      out
    },

    #' @description
    #' Get weight matrix data.
    #' @return [Matrix::sparseMatrix()] with data.
    get_weight_data = function() {
      if (length(self$weights) > 0) {
        # if weights are present, then create matrix using them
        v <- lapply(self$weights, `[[`, "variable")
        out <- extract_data_matrix(v)
        rownames(out) <- vapply(self$weights, `[[`, character(1), "id")
      } else {
        # if no weights are present, then create empty matrix
        n_pu <- length(
          self$themes[[1]]$feature[[1]]$variable$dataset$
            get_planning_unit_indices()
        )
        out <- Matrix::sparseMatrix(
          i = numeric(0), j = numeric(0), x = numeric(0), dims = c(0, n_pu)
        )
      }
      out
    },

    #' @description
    #' Get includes matrix data.
    #' @return [Matrix::sparseMatrix()] with data.
    get_include_data = function() {
      if (length(self$includes) > 0) {
        # if includes are present, then create matrix using them
        v <- lapply(self$includes, `[[`, "variable")
        out <- extract_data_matrix(v)
        rownames(out) <- vapply(self$includes, `[[`, character(1), "id")
      } else {
        # if no includes are present, then create empty matrix
        n_pu <- length(
          self$themes[[1]]$feature[[1]]$variable$dataset$
            get_planning_unit_indices()
        )
        out <- Matrix::sparseMatrix(
          i = numeric(0), j = numeric(0), x = numeric(0), dims = c(0, n_pu)
        )
      }
      out
    },

    #' @description
    #' Update the current amount held for each themes and weights automatically
    #' based on the include statuses.
    #' @param theme_data `[Matrix::sparseMatrix()] with theme data.
    #' Defaults to `self$get_theme_data()`.
    #' @param weight_data `[Matrix::sparseMatrix()] with weight data.
    #' Defaults to `self$get_weight_data()`.
    #' @param include_data `[Matrix::sparseMatrix()] with include data.
    #' Defaults to `self$get_include_data()`.
    update_current_held = function(theme_data = self$get_theme_data(),
                                   weight_data = self$get_weight_data(),
                                   include_data = self$get_include_data()) {
      # assert arguments are valid
      assertthat::assert_that(
        inherits(theme_data, "dgCMatrix"),
        inherits(weight_data, "dgCMatrix"),
        inherits(include_data, "dgCMatrix"),
        ncol(theme_data) == ncol(include_data),
        ncol(weight_data) == ncol(include_data),
        nrow(include_data) == length(self$includes),
        nrow(weight_data) == length(self$weights)
      )
      # calculate current status for each planning unit
      curr_status <- include_data
      if (nrow(include_data) > 0) {
        ## if includes are present, then use their data
        for (i in seq_len(nrow(curr_status))) {
          curr_status[i, ] <- curr_status[i, ] * self$includes[[i]]$status
        }
        curr_status <- as.numeric(colSums(curr_status > 0.5) > 0.5)
      } else {
        ## if no includes are present, then set place holder of zeros
        curr_status <- rep(0, ncol(theme_data))
      }

      # themes
      ## calculate current amount held for each feature as a proportion
      curr_feature_held <- calculate_coverage(curr_status, theme_data)
      ## update the current amount for each theme
      for (i in seq_along(self$themes)) {
        self$themes[[i]]$set_feature_current(
          unname(curr_feature_held[self$themes[[i]]$get_feature_id()])
        )
      }

      # weights
      if (nrow(weight_data) > 0) {
        ## calculate current amount held for each weight as a proportion
        curr_weight_held <- calculate_coverage(curr_status, weight_data)
        ## update the current amount for each weight
        for (i in seq_along(self$weights)) {
          self$weights[[i]]$set_current(
            unname(curr_weight_held[self$weights[[i]]$id])
          )
        }
      }

      # return self
      invisible(self)
    }
  )
)

#' New solution settings
#'
#' Create a new [SolutionSettings] object.
#'
#' @param themes `list` of [Theme] objects. D
#'
#' @param weights `list` of [Weight] objects.
#'
#' @param includes `list` of [Include] objects.
#'
#' @param parameters `list` of [Parameter] objects.
#'
#' @return A [SolutionSettings] object.
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#' v3 <- new_variable_from_auto(dataset = d, index = 3)
#' v4 <- new_variable_from_auto(dataset = d, index = 4)
#' v5 <- new_variable_from_auto(dataset = d, index = 5)
#'
#' # create a weight using a variable
#' w <- new_weight(
#'   name = "Human Footprint Index", variable = v1,
#'   factor = -90, status = FALSE, id = "W1"
#' )
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   goal = 0.2, status = FALSE, current = 0.5, id = "F1"
#' )
#' f2 <- new_feature(
#'   name = "Forests", variable = v3,
#'   goal = 0.3, status = FALSE, current = 0.9, id = "F2"
#' )
#' f3 <- new_feature(
#'   name = "Shrubs", variable = v4,
#'   goal = 0.6, status = TRUE, current = 0.4, id = "F3"
#' )
#'
#' # create themes using the features
#' t1 <- new_theme("Species", f1, id = "T1")
#' t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
#'
#' # create an included using a variable
#' i <- new_include(
#'   name = "Protected areas", variable = v5,
#'   status = FALSE, id = "I1"
#' )
#'
#' # create parameters
#' p1 <- new_parameter(name = "Spatial clustering")
#' p2 <- new_parameter(name = "Optimality gap")
#'
#' # create solution settings using the themes and weight
#' ss <- new_solution_settings(
#'   themes = list(t1, t2), weights = list(w), includes = list(i),
#'   parameters = list(p1, p2)
#' )
#'
#' # print object
#' print(ss)
#' @export
new_solution_settings <- function(themes, weights, includes, parameters) {
  SolutionSettings$new(
    themes = themes,
    weights = weights,
    includes = includes,
    parameters = parameters
  )
}
