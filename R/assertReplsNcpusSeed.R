assertReplsNcpusSeed = function(repls, ncpus, seed) {

  checkmate::assertIntegerish(repls, lower = 1, any.missing = FALSE,
                              len = 1)

  checkmate::assertIntegerish(ncpus, lower = 1, any.missing = TRUE,
                              len = 1)

  checkmate::assertIntegerish(seed, lower = 1, any.missing = FALSE,
                              len = 1)
}
