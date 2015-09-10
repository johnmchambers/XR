frj <- jsonlite::fromJSON; toj <- jsonlite::toJSON
ev <- InsideInterface()
i1 <- 1:5
i1j <- toj(i1)
i1a <- asRObject(frj(i1j), ev)
all.equal(i1, i1a)
nl1 <- list(a = 1:5, b = round(rnorm(3),2))
nl1a <- asRObject(frj(toj(nl1)), ev)
all.equal(nl1, nl1a)
## make an environment
nl2 <- nl1
keys <- c(".RClass", ".package", ".type")
nl2[keys] <- list("environment", "methods", "environment")
nl2a <- asRObject(frj(toj(nl2)), ev)
all.equal(as.environment(nl2), nl2a)
