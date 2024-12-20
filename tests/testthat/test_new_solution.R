context("new_solution")

test_that("initialization", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  v4 <- new_variable(
    d,
    index = 3, total = 90, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = -0.2,
    id = "FID1"
  )
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  i <- new_include(
    name = "Protected areas",
    variable = v4,
    visible = FALSE,
    status = FALSE,
    id = "FID1"
  )
  ir <- new_include_results(
    include = i,
    held = 0.78,
    id = "IID1"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  p <- new_parameter("budget", value = 12)
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    include_results = list(ir),
    id = "solution1"
  )
  # run tests
  expect_is(x, "Solution")
  print(x)
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v3)
  expect_identical(x$visible, FALSE)
  expect_equal(x$parameters, list(p))
  expect_identical(x$statistics, list(s1, s2))
  expect_identical(x$theme_results, list(thr))
  expect_identical(x$weight_results, list(wr))
  expect_identical(x$include_results, list(ir))
  expect_identical(x$id, "solution1")
  expect_is(x$get_summary_results_data(), "data.frame")
  expect_is(x$get_theme_results_data(), "data.frame")
  expect_is(x$get_weight_results_data(), "data.frame")
  expect_is(x$get_include_results_data(), "data.frame")
  expect_is(x$render_summary_results(), "datatables")
  expect_is(x$render_theme_results(), "datatables")
  expect_is(x$render_weight_results(), "datatables")
  expect_is(x$render_include_results(), "datatables")
})

test_that("initialization (no weights or includes)", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  p <- new_parameter("budget", value = 12)
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(),
    include_results = list(),
    id = "solution1"
  )
  # run tests
  expect_is(x, "Solution")
  print(x)
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v3)
  expect_identical(x$visible, FALSE)
  expect_equal(x$parameters, list(p))
  expect_identical(x$statistics, list(s1, s2))
  expect_identical(x$theme_results, list(thr))
  expect_identical(x$weight_results, list())
  expect_identical(x$include_results, list())
  expect_identical(x$id, "solution1")
  expect_is(x$get_theme_results_data(), "data.frame")
  expect_is(x$get_weight_results_data(), "data.frame")
  expect_is(x$get_include_results_data(), "data.frame")
  expect_is(x$render_theme_results(), "datatables")
  expect_is(x$render_weight_results(), "datatables")
  expect_is(x$render_include_results(), "datatables")
})

test_that("initialization (from Result object)", {
  skip_on_ci()
  skip_if_not_installed("RandomFields")  
  # create object
  ## create dataset
  RandomFields::RFoptions(seed = 200)
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  d <- new_dataset_from_auto(rd)
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = simulate_continuous_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = simulate_continuous_legend()
  )
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = simulate_continuous_legend()
  )
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = simulate_continuous_legend()
  )
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_include_legend()
  )
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = 90, status = TRUE, id = "W1"
  )
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
    status = FALSE, id = "I1"
  )
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    goal = 0.2, status = FALSE, current = 0.5, id = "F1"
  )
  f2 <- new_feature(
    name = "Forests", variable = v3,
    goal = 0.3, status = FALSE, current = 0.9, id = "F2"
  )
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    goal = 0.6, status = TRUE, current = 0.4, id = "F3"
  )
  ## create themes using the features
  t1 <- new_theme("Species", f1, id = "T1")
  t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", id = "P1")
  p2 <- new_parameter("Gap", id = "P2")
  ## create solution setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1, p2)
  )
  ## create result
  r <- min_set_result(
    id = "R1",
    area_data = d$get_planning_unit_areas(),
    boundary_data = d$get_boundary_data(),
    theme_data = ss$get_theme_data(),
    weight_data = ss$get_weight_data(),
    include_data = ss$get_include_data(),
    theme_settings = ss$get_theme_settings(),
    weight_settings = ss$get_weight_settings(),
    include_settings = ss$get_include_settings(),
    parameters = ss$parameters,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status
  )
  ## create object
  x <- new_solution_from_result(
    id = "S1",
    result = r,
    name = "sol",
    visible = TRUE,
    dataset = d,
    settings = ss,
    legend = new_manual_legend(
      values = c(0, 1),
      colors = c("#00FFFF00", "#112233FF"),
      labels = c("not selected", "selected")
    )
  )
  # run tests
  expect_is(x, "Solution")
})

test_that("initialization (from Result object), sf, no boundary", {
  skip_on_ci()
  skip_if_not_installed("RandomFields")  
  # create object
  ## create dataset
  spatial_data <- import_simple_vector_data()
  idx <- seq_len(nrow(spatial_data))
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    V3 = runif(length(idx)),
    V4 = runif(length(idx)),
    V5 = runif(length(idx)),
    `_index` = idx
  )
  # merge attribute data with spatial data
  sf_project <- merge(spatial_data, attribute_data, by.x = "id", by.y = "_index")
  # move id column to last position
  sf_project <- dplyr::relocate(sf_project, id, .after = last_col())
  # change id column name to _index
  names(sf_project)[names(sf_project) == "id"] <- "_index"
  
  # create object 
  d <- new_dataset_from_auto(
    x = sf_project,
    skip_bm = TRUE
  )
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = new_null_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = new_null_legend()
  )
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = new_null_legend()
  )
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = new_null_legend()
  )
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = new_null_legend()
  )
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = 90, status = TRUE, id = "W1"
  )
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
    status = FALSE, id = "I1"
  )
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    goal = 0.2, status = FALSE, current = 0.5, id = "F1"
  )
  f2 <- new_feature(
    name = "Forests", variable = v3,
    goal = 0.3, status = FALSE, current = 0.9, id = "F2"
  )
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    goal = 0.6, status = TRUE, current = 0.4, id = "F3"
  )
  ## create themes using the features
  t1 <- new_theme("Species", f1, id = "T1")
  t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", id = "P1")
  p2 <- new_parameter("Gap", id = "P2")
  ## create solution setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1, p2)
  )
  ## create result
  r <- min_set_result(
    id = "R1",
    area_data = d$get_planning_unit_areas(),
    boundary_data = d$get_boundary_data(),
    theme_data = ss$get_theme_data(),
    weight_data = ss$get_weight_data(),
    include_data = ss$get_include_data(),
    theme_settings = ss$get_theme_settings(),
    weight_settings = ss$get_weight_settings(),
    include_settings = ss$get_include_settings(),
    parameters = ss$parameters,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = 0
  )
  ## create object
  x <- new_solution_from_result(
    id = "S1",
    result = r,
    name = "sol",
    visible = TRUE,
    dataset = d,
    settings = ss,
    legend = new_manual_legend(
      values = c(0, 1),
      colors = c("#00FFFF00", "#112233FF"),
      labels = c("not selected", "selected")
    )
  )
  # run tests
  expect_equal(is.na(r$perimeter), TRUE)
  # reserve_sizes statistics should all be ""
  expect_equal(
    any(
      !stringi::stri_isempty(x$get_summary_results_data()[5:9,3][[1]])), FALSE
    )
  expect_is(x, "Solution")
})

test_that("get methods", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v2 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  f <- new_feature(
    name = "F1",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  p <- new_parameter("budget", value = 12)
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v2,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(),
    include_results = list(),
    id = "solution1"
  )
  # run tests
  expect_equal(x$get_visible(), FALSE)
})

test_that("set methods", {
  skip_if_not_installed("RandomFields")
  # create object
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v2 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  f <- new_feature(
    name = "F1",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  p <- new_parameter("budget", value = 12)
  x <- new_solution(
    name = "solution001",
    variable = v2,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(),
    include_results = list(),
    id = "solution1"
  )
  # run tests
  expect_equal(x$get_visible(), FALSE)
  x$set_visible(TRUE)
  expect_equal(x$get_visible(), TRUE)
})

test_that("widget methods", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  v4 <- new_variable(
    d,
    index = 3, total = 90, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )

  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = -0.2,
    id = "FID1"
  )
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  i <- new_include(
    name = "Protected areas",
    variable = v4,
    visible = FALSE,
    status = FALSE,
    id = "FID1"
  )
  ir <- new_include_results(
    include = i,
    held = 0.78,
    id = "IID1"
  )
  p <- new_parameter("budget", value = 12)
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x_nohide <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    include_results = list(ir),
    id = "solution1",
    hidden = FALSE
  )
  
  x_hide <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    include_results = list(ir),
    id = "solution1",
    hidden = TRUE
  )  
  
  # run tests
  ## solution results widget
  expect_identical(
    x_nohide$get_solution_results_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      parameters = list(p$get_widget_data()),
      statistics = list(s1$get_widget_data(), s2$get_widget_data()),
      theme_results = list(thr$get_widget_data()),
      weight_results = list(wr$get_widget_data()),
      include_results = list(ir$get_widget_data()),
      solution_color = scales::alpha(last(x_nohide$variable$legend$colors), 1)
    )
  )
  ## map manager widget
  expect_identical(
    x_nohide$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      visible = FALSE,
      legend = v3$legend$get_widget_data(),
      units = v3$units,
      type = "solution",
      hidden = FALSE
    )
  )
  expect_identical(
    x_hide$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      visible = FALSE,
      legend = v3$legend$get_widget_data(),
      units = v3$units,
      type = "solution",
      hidden = TRUE
    )
  )  
  
})


