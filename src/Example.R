createMwc <- function () {
    output <- c(1:2048)
    result <- .C("createHsMwcS",
                 out_vec=as.integer(output),
                 out_len=as.integer(0))
    return(list(seed=result$out_vec, length=result$out_len))
}

nextMwc <- function(g, gLen) {
    output <- c(1:2048)
    result <- .C("randHsMwcS",
                 in_vec=as.integer(g),
                 in_len=as.integer(gLen),
                 rnd_int=as.integer(0),
                 out_vec=as.integer(output),
                 out_len=as.integer(0))
    return(list(seed=result$out_vec, length=result$out_len, rndInt=result$rnd_int))
}

dyn.load("/Users/dom/Library/Haskell/ghc-7.10.3/lib/test-via-r-0.1.0.0/bin/MwcTest.dylib")
.C("HsStart")

foo <- createMwc()
bar <- nextMwc(foo$seed, foo$length)
print(bar$rndInt)
baz <- nextMwc(bar$seed, bar$length)
print(baz$rndInt)
