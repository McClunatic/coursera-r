pollutantmean <- function(directory, pollutant, id = 1:332) {
    dfs <- vector(mode = "list", length = length(id))
    for (value in id) {
        csv <- file.path(directory, sprintf("%03d.csv", value))
        dfs[[value]] <- read.csv(csv)
    }
    df <- do.call("rbind", dfs)
    mean(df[pollutant][!is.na(df[pollutant])])
}