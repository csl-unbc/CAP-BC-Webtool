context("simulate spatial data")

test_that("proportion (raster)", {
  skip_if_not_installed("RandomFields")
  # create data
  d <- import_simple_raster_data()
  x <- simulate_proportion_spatial_data(d, 2)
  # run tests
  expect_is(x, "Raster")
  expect_equal(raster::nlayers(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    raster::compareRaster(
      x, d,
      res = TRUE, tolerance = 1e-5, stopiffalse = FALSE
    )
  )
  expect_gte(min(raster::cellStats(x, "min")), 0)
  expect_lte(max(raster::cellStats(x, "max")), 1)
})

test_that("proportion (sf)", {
  skip_if_not_installed("RandomFields")
  # create data
  d <- import_simple_vector_data()
  x <- simulate_proportion_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
  expect_lte(max(as.matrix(sf::st_drop_geometry(x))), 1)
})

test_that("continuous (raster)", {
  skip_if_not_installed("RandomFields")
  # create data
  d <- import_simple_raster_data()
  x <- simulate_continuous_spatial_data(d, 2)
  # run tests
  expect_is(x, "Raster")
  expect_equal(raster::nlayers(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    raster::compareRaster(
      x, d,
      res = TRUE, tolerance = 1e-5, stopiffalse = FALSE
    )
  )
  expect_gte(min(raster::cellStats(x, "min")), 0)
})

test_that("continuous (sf)", {
  skip_if_not_installed("RandomFields")
  # create data
  d <- import_simple_vector_data()
  x <- simulate_continuous_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
})

test_that("categorical (raster)", {
  skip_if_not_installed("RandomFields")
  # create data
  d <- import_simple_raster_data()
  x <- simulate_categorical_spatial_data(d, 2)
  # run tests
  expect_is(x, "Raster")
  expect_equal(raster::nlayers(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    raster::compareRaster(
      x, d,
      res = TRUE, tolerance = 1e-5, stopiffalse = FALSE
    )
  )
  expect_gte(min(raster::cellStats(x, "min")), 0)
  expect_lte(max(raster::cellStats(x, "max")), 11)
  expect_lte(n_distinct(c(raster::values(x))), 11)
})

test_that("categorical (sf)", {
  skip_if_not_installed("RandomFields")  
  # create data
  RandomFields::RFoptions(seed = 500)
  d <- import_simple_vector_data()
  x <- simulate_categorical_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
  expect_lte(max(as.matrix(sf::st_drop_geometry(x))), 11)
  expect_lte(n_distinct(c(as.matrix(sf::st_drop_geometry(x)))), 11)
})
