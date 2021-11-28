print("This file was created within RStudio")

print("And now it lives on GitHub")

my_function <- function() {
    x <- rnorm(100)
    mean(x)
}

second <- function(x) {
    x <- x + rnorm(length(x))
}