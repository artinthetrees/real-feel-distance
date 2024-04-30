# https://cran.r-project.org/web/packages/osmdata/vignettes/query-split.html

split_bbox <- function (bbox, grid = 2, eps = 0.05) {
    xmin <- bbox ["x", "min"]
    ymin <- bbox ["y", "min"]
    dx <- (bbox ["x", "max"] - bbox ["x", "min"]) / grid
    dy <- (bbox ["y", "max"] - bbox ["y", "min"]) / grid

    bboxl <- list ()

    for (i in 1:grid) {
        for (j in 1:grid) {
            b <- matrix (c (
                xmin + ((i - 1 - eps) * dx),
                ymin + ((j - 1 - eps) * dy),
                xmin + ((i + eps) * dx),
                ymin + ((j + eps) * dy)
            ),
            nrow = 2,
            dimnames = dimnames (bbox)
            )

            bboxl <- append (bboxl, list (b))
        }
    }
    bboxl
}













