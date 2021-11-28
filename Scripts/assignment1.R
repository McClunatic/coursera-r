pollutantmean <- function(directory, pollutant, id = 1:332) {
    dfs <- vector(mode = "list", length = length(id))
    for (value in id) {
        csv <- file.path(directory, sprintf("%03d.csv", value))
        dfs[[value]] <- read.csv(csv)
    }
    df <- do.call("rbind", dfs)
    mean(df[pollutant][!is.na(df[pollutant])])
}

complete <- function(directory, id = 1:332) {
    dfs <- vector(mode = "list", length = length(id))
    for (value in seq_along(id)) {
        csv <- file.path(directory, sprintf("%03d.csv", id[[value]]))
        df <- read.csv(csv)
        dfs[[value]] <- data.frame(
            id = id[[value]],
            nobs = sum(complete.cases(df))
        )
    }
    res <- do.call("rbind", dfs)
    colnames(res) <- c("id", "nobs")
    res
}