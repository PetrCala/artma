box::use(
  testthat[
    describe,
    expect_equal,
    expect_named,
    expect_null,
    expect_true,
    it,
    test_that
  ]
)

box::use(
  artma / methods / bma[bma_estimates],
  artma / methods / fma[fma_estimates],
  artma / modules / runtime_methods[new_estimates],
  artma / output / ma_table[build_ma_table, display_ma_table]
)

# The fixtures go through the methods' own estimates builders, so the table
# stays pinned to the mapping the methods actually produce.
make_bma_estimates <- function() {
  bma_estimates(data.frame(
    variable = c("Intercept", "T stat", "Precision", "Pcc"),
    pip = c(0.55, 1.0, 1.0, 0.97),
    post_mean = c(0.01, 0.43, -0.20, 0.13),
    post_sd = c(0.05, 0.016, 0.023, 0.019),
    cond_pos_sign = c(0.60, 1.0, 0.0, 1.0),
    stringsAsFactors = FALSE
  ))
}

make_fma_estimates <- function() {
  fma_estimates(data.frame(
    variable = c("Intercept", "T stat", "Precision", "Pcc"),
    coefficient = c(-3.674, 0.288, -5.794, 1.604),
    se = c(0.888, 0.011, 0.539, 0.247),
    p_value = c(0.001, 0.000, 0.000, 0.000),
    stringsAsFactors = FALSE
  ))
}

describe("build_ma_table", {
  it("returns NULL when both inputs are NULL", {
    result <- build_ma_table(bma_estimates = NULL, fma_estimates = NULL)
    expect_null(result)
  })

  it("returns NULL when both inputs are empty data frames", {
    result <- build_ma_table(bma_estimates = new_estimates(), fma_estimates = new_estimates())
    expect_null(result)
  })

  it("builds table with both BMA and FMA columns", {
    bma <- make_bma_estimates()
    fma <- make_fma_estimates()
    result <- build_ma_table(bma_estimates = bma, fma_estimates = fma)

    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 4)
    expected_cols <- c("Variable", "BMA P.Mean", "BMA SD", "BMA PIP", "FMA Coef", "FMA SE", "FMA p-val")
    expect_named(result, expected_cols)
  })

  it("reads the BMA posterior inclusion probability from the statistic column", {
    result <- build_ma_table(bma_estimates = make_bma_estimates())

    expect_equal(result[["BMA PIP"]], c(0.55, 1.0, 1.0, 0.97))
    expect_equal(result[["BMA P.Mean"]], c(0.01, 0.43, -0.20, 0.13))
  })

  it("builds table with BMA columns only", {
    bma <- make_bma_estimates()
    result <- build_ma_table(bma_estimates = bma, fma_estimates = NULL)

    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 4)
    expected_cols <- c("Variable", "BMA P.Mean", "BMA SD", "BMA PIP")
    expect_named(result, expected_cols)
  })

  it("builds table with FMA columns only", {
    fma <- make_fma_estimates()
    result <- build_ma_table(bma_estimates = NULL, fma_estimates = fma)

    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 4)
    expected_cols <- c("Variable", "FMA Coef", "FMA SE", "FMA p-val")
    expect_named(result, expected_cols)
  })

  it("places Intercept as the first row", {
    bma <- make_bma_estimates()
    # Shuffle so Intercept is not first
    bma <- bma[c(2, 3, 1, 4), ]
    result <- build_ma_table(bma_estimates = bma)

    expect_equal(result$Variable[1], "Intercept")
  })

  it("normalizes (Intercept) to Intercept", {
    bma <- make_bma_estimates()
    bma$term[1] <- "(Intercept)"
    result <- build_ma_table(bma_estimates = bma)

    expect_true("Intercept" %in% result$Variable)
    expect_equal(result$Variable[1], "Intercept")
  })

  it("rounds values to the specified number of decimals", {
    bma <- bma_estimates(data.frame(
      variable = c("Var1"),
      pip = c(0.123456),
      post_mean = c(0.654321),
      post_sd = c(0.111111),
      stringsAsFactors = FALSE
    ))
    result <- build_ma_table(bma_estimates = bma, round_to = 2)

    expect_equal(result[["BMA PIP"]], 0.12)
    expect_equal(result[["BMA P.Mean"]], 0.65)
    expect_equal(result[["BMA SD"]], 0.11)
  })

  it("handles variable sets that differ between BMA and FMA", {
    bma <- bma_estimates(data.frame(
      variable = c("Var A", "Var B"),
      pip = c(1.0, 0.8),
      post_mean = c(0.5, 0.3),
      post_sd = c(0.1, 0.2),
      stringsAsFactors = FALSE
    ))
    fma <- fma_estimates(data.frame(
      variable = c("Var B", "Var C"),
      coefficient = c(0.4, 0.6),
      se = c(0.05, 0.07),
      p_value = c(0.01, 0.03),
      stringsAsFactors = FALSE
    ))
    result <- build_ma_table(bma_estimates = bma, fma_estimates = fma)

    expect_equal(nrow(result), 3)
    expect_equal(result$Variable, c("Var A", "Var B", "Var C"))

    # Var A: BMA values present, FMA values NA
    expect_equal(result[["BMA P.Mean"]][1], 0.5)
    expect_true(is.na(result[["FMA Coef"]][1]))

    # Var B: both present
    expect_equal(result[["BMA P.Mean"]][2], 0.3)
    expect_equal(result[["FMA Coef"]][2], 0.4)

    # Var C: BMA values NA, FMA values present
    expect_true(is.na(result[["BMA P.Mean"]][3]))
    expect_equal(result[["FMA Coef"]][3], 0.6)
  })
})

describe("display_ma_table", {
  it("returns invisible NULL for NULL input", {
    result <- display_ma_table(NULL)
    expect_null(result)
  })

  it("returns invisible NULL for empty data frame", {
    result <- display_ma_table(data.frame())
    expect_null(result)
  })

  it("does not error for valid table at verbosity 1", {
    bma <- make_bma_estimates()
    table <- build_ma_table(bma_estimates = bma)
    result <- display_ma_table(table, verbosity = 1L)
    expect_true(is.data.frame(result))
  })
})
