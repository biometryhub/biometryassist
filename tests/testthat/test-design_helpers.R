# Tests for internal helper functions in design_helpers.R

test_that("get_design_info correctly identifies non-factorial designs", {
  # Test with a CRD design
  crd_design <- agricolae::design.crd(trt = c("A", "B", "C"), r = 3, seed = 42)
  result <- biometryassist:::get_design_info(crd_design)
  
  expect_type(result, "list")
  expect_named(result, c("type", "is_factorial", "base"))
  expect_equal(result$type, "crd")
  expect_false(result$is_factorial)
  expect_equal(result$base, "crd")
})

test_that("get_design_info correctly identifies RCBD designs", {
  # Test with an RCBD design
  rcbd_design <- agricolae::design.rcbd(trt = c("T1", "T2", "T3", "T4"), 
                                        r = 4, seed = 42)
  result <- biometryassist:::get_design_info(rcbd_design)
  
  expect_type(result, "list")
  expect_equal(result$type, "rcbd")
  expect_false(result$is_factorial)
  expect_equal(result$base, "rcbd")
})

test_that("get_design_info correctly identifies LSD designs", {
  # Test with a Latin Square design
  lsd_design <- agricolae::design.lsd(trt = c("V1", "V2", "V3", "V4"), 
                                      seed = 42)
  result <- biometryassist:::get_design_info(lsd_design)
  
  expect_type(result, "list")
  expect_equal(result$type, "lsd")
  expect_false(result$is_factorial)
  expect_equal(result$base, "lsd")
})

test_that("get_design_info correctly identifies factorial designs with CRD", {
  # Test with a factorial CRD design
  factorial_crd <- agricolae::design.ab(trt = c(2, 3), r = 2, 
                                        design = "crd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_crd)
  
  expect_type(result, "list")
  expect_named(result, c("type", "is_factorial", "base"))
  expect_equal(result$type, "factorial_crd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "crd")
})

test_that("get_design_info correctly identifies factorial designs with RCBD", {
  # Test with a factorial RCBD design
  factorial_rcbd <- agricolae::design.ab(trt = c(3, 2), r = 3, 
                                         design = "rcbd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_rcbd)
  
  expect_type(result, "list")
  expect_equal(result$type, "factorial_rcbd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "rcbd")
})

test_that("get_design_info correctly identifies factorial designs with LSD", {
  # Test with a factorial Latin Square design
  factorial_lsd <- agricolae::design.ab(trt = c(2, 2), r = 1, 
                                        design = "lsd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_lsd)
  
  expect_type(result, "list")
  expect_equal(result$type, "factorial_lsd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "lsd")
})

test_that("get_design_info handles 3-way factorial designs", {
  # Test with a 3-way factorial design
  factorial_3way <- agricolae::design.ab(trt = c(2, 2, 2), r = 2, 
                                         design = "crd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_3way)
  
  expect_type(result, "list")
  expect_equal(result$type, "factorial_crd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "crd")
})

test_that("get_design_info structure is consistent across design types", {
  # Create different design types and verify structure is always the same
  designs <- list(
    crd = agricolae::design.crd(trt = c("A", "B"), r = 2, seed = 42),
    rcbd = agricolae::design.rcbd(trt = c("A", "B"), r = 3, seed = 42),
    lsd = agricolae::design.lsd(trt = c("A", "B", "C"), seed = 42),
    factorial = agricolae::design.ab(trt = c(2, 2), r = 2, design = "crd", seed = 42)
  )
  
  for (design in designs) {
    result <- biometryassist:::get_design_info(design)
    
    # Check that all expected fields are present
    expect_named(result, c("type", "is_factorial", "base"))
    
    # Check types of fields
    expect_type(result$type, "character")
    expect_type(result$is_factorial, "logical")
    expect_type(result$base, "character")
    
    # Check that all fields have length 1
    expect_length(result$type, 1)
    expect_length(result$is_factorial, 1)
    expect_length(result$base, 1)
  }
})
