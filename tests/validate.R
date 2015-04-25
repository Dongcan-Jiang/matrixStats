library(matrixStats)


# positives and negatives mixing.
tools::assertError(validate(1:-1, 1))
tools::assertError(validate(-1:1, 1))
tools::assertError(validate(as.integer(c(1, 0, 0, -4)), 2))
tools::assertError(validate(as.integer(c(-4, 0, 0, 1)), 6))


# negative indices
y <- validate(as.integer(c(-4, 0, -3, -1)), 5)
stopifnot(identical(y, as.integer(c(2, 5))))


# positive indices
y <- validate(as.integer(c(4, 4, 8, 2, 3)), 8)
stopifnot(identical(y, as.integer(c(4, 4, 8, 2, 3))))

validate(as.integer(c(4, 4, 8, 2, 3)), 9)
stopifnot(identical(y, as.integer(c(4, 4, 8, 2, 3))))


# positive out of ranges
tools::assertError(validate(as.integer(c(4, 4, 8, 2, 3)), 7))


# negative out of ranges
y <- validate(as.integer(c(-4, 0, -3, -1)), 3)
stopifnot(identical(y, as.integer(c(2))))


# negative indices exclude all
y <- validate(as.integer(c(-4, 0, -3, -1, -2, 0)), 3)
stopifnot(identical(y, integer(0)))


# idxs is single integer
y <- validate(as.integer(4), 5)
stopifnot(identical(y, as.integer(c(4))))

y <- validate(as.integer(-4), 5)
stopifnot(identical(y, as.integer(c(1, 2, 3, 5))))

y <- validate(as.integer(0), 5)
stopifnot(identical(y, integer(0)))


# idxs is empty
y <- validate(integer(0), 5)
stopifnot(identical(y, integer(0)))


# idxs is NULL
#y <- validate(NULL, 5)
#stopifnot(identical(y, 1:5))

#y <- validate(, 5)
#stopifnot(identical(y, 1:5))


# N is 0
#tools::assertError(validate(4:4, 0))

#y <- validate(-4:-4, 0)
#stopifnot(identical(y, integer(0)))

#y <- validate(0:0, 0)
#stopifnot(identical(y, integer(0)))


# NA in idxs
tools::assertError(validate(as.integer(c(NA, -2)), 2))

y <- validate(as.integer(c(NA, 0, 2)), 2)
stopifnot(identical(y, as.integer(c(NA, 2))))
