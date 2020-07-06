
Nk <- 4
Nd <- 50
Nv <- 1000

alpha <- rgamma(Nk, 0.5)

test_that("generate zipf works", {
  beta <- generate_zipf(vocab_size = Nv, magnitude = 500, zipf_par = 1.1)

  expect_equal(length(beta), Nv)

})

beta <- generate_zipf(vocab_size = Nv, magnitude = 500, zipf_par = 1.1)

test_that("sampling parameters works", {
  pars <- sample_parameters(alpha, beta, Nd)

  expect_equal(nrow(pars$theta), Nd)

  expect_equal(ncol(pars$theta), Nk)

  expect_equal(nrow(pars$phi), Nk)

  expect_equal(ncol(pars$phi), Nv)

  # below ensures rows sum to 1
  expect_equal(sum(rowSums(pars$theta)), Nd)

  expect_equal(sum(rowSums(pars$phi)), Nk)

})

pars <- sample_parameters(alpha, beta, Nd)

doc_lengths <- rpois(Nd, 50)

test_that("sampling documents works", {
  dtm <- sample_documents(
    theta = pars$theta,
    phi = pars$phi,
    doc_lengths = doc_lengths,
    threads = 1
  )

  expect_equal(sum(dim(dtm)), Nd + Nv)
})


test_that("parallel sampling documents works", {
  dtm <- sample_documents(
    theta = pars$theta,
    phi = pars$phi,
    doc_lengths = doc_lengths,
    threads = 2
  )

  expect_equal(sum(dim(dtm)), Nd + Nv)
})
