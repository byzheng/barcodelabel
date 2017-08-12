# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   08:31 PM Thursday, 04 June 2015
# * Copyright: AS IS

# Generate barcode for a number

zint_barcode <- function(
    labels,
    height = 50,
    border = 10,
    zint = 'zint',
    is_draw = FALSE,
    is_show_text = TRUE) {
    file_name <- paste0(tempfile(), '.png')
    res <- list()
    for (i in seq(along = labels)) {
        cmd <- paste0(
            zint,
            ' -o ', file_name,
            ifelse(is_show_text, '', ' --notext'),
            ' -b 20 --height=',
            height, ' --border=', border,
            ' -d "', labels[i], '"')
        system(cmd)
        library(png)

        img <- readPNG(file_name)
        res[[as.character(labels[i])]] <- img
    }

    if (is_draw) {
        library(grid)
        grid.raster(res[[1]])
    }
    res
}
