# When using this code, please cite:
# Legewie. 2018. Living on the Edge. Neighborhood Boundaries and the Spatial Dynamics of Violent Crime. Demography 55: 1957–1977.

#' Areal Wombling
#'
#' \code{areal_wombling} for algorithmic areal wombling (Lu and Carlin 2005: 268).
#'
#' By default, \code{censusr} downloads and recodes a selected set of variables.
#' These variables include 100-300 commonly used measures from the
#'
#' @param  sp Object of type \code{SpatialPolygonsDataFrame} (sf objects as converted)
#' @param  x Vector of variable names for which we want to calculate the boundary value.
#' @param threshold Threshold for the boundary membership value (BMV). If \code{threshold} is 
#'   /code{NA} (the default), /code{areal_wombling} uses fuzzy wombling. If \code{threshold} is 
#'   specified (any value between 0 and 1), /code{areal_wombling} uses crisp wombling
#'   using \code{threshold} to determine boundary membership.
#' @param dist Distance function. By default the absolute difference in the response variable.
#' @return    Object of class \code{SpatialLinesDataFrame} with SpatialLines 
#' @export

areal_wombling <- function(sp, x, threshold = NA, dist = function(x) abs(x[1] - x[2])) {
    # check `reshape2:::parse_formula` for p.white.cb + p.black.cb + p.hisp.cb + p.asian.cb ~ 1
    # Coerce simple feature geometries to corresponding Spatial* objects
    if (is(sp, "sf")) sp <- as(sp, "Spatial")
    # get borders as line segments in SpatialLinesDataFrame
    sl <- border_lines(sp)
    # boundary likelihood value (BLV) and boundary membership value (BMV) to data.frame
    dots_format <- function(s, suffix)
        s %>% setNames(paste0(x, suffix)) %>%
            as.list() %>% lapply(FUN = as.formula, env = environment())
    dots_blv <- dots_format(sprintf("~ dist(sp@data[['%s']][c(i, j)])[1]", x), suffix = "_blv")
    dots_bmv <- dots_format(sprintf("~ %s_blv/max(%s_blv, na.rm = TRUE)", x, x), suffix = "_bmv")
    if(!is.na(threshold))
        dots_bmv <- dots_format(sprintf("~ %s_blv > %s", x, threshold), suffix = "_bmv")
    sl@data <- sl@data %>%
        dplyr::group_by(i, j) %>%
        dplyr::mutate_(.dots = dots_blv) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_(.dots = dots_bmv) %>%
        as.data.frame()
    # return
    return(sl)
}


#' Bayesian Areal Wombling
#'
#' \code{areal_wombling_bayesian} for Bayesian areal wombling (Lu and Carlin 2005).
#'
#' By default, \code{censusr} downloads and recodes a selected set of variables.
#' These variables include 100-300 commonly used measures from the
#'
#' @param  formula  A formula for the covariate part of the model, using the same notation as for the \code{lm()} function.
#'   \code{y ~ 1} estimates a model without covariates.
#' @param  family One of either 'binomial', 'gaussian' or 'poisson', which respectively specify a binomial likelihood model with a logistic link function, a Gaussian likelihood model with an identity link function, or a Poisson likelihood model with a log link function.
#' @param  sp Object of type \code{SpatialPolygonsDataFrame} (sf objects as converted)
#' @param  phi Conditional autoregressive (CAR) prior for the random effect. Options are 'leroux' for the CAR prior proposed by Leroux et al. (1999) (the default), 'IAR' for the intrinsic CAR, and 'BYM' for the BYM CAR proposed by Besag et al. (1991).
#' @param threshold Threshold for the boundary membership value (BMV). If \code{threshold} is 
#'   /code{NA} (the default), /code{areal_wombling} uses fuzzy wombling. If \code{threshold} is 
#'   specified (any value between 0 and 1), /code{areal_wombling} uses crisp wombling
#'   using \code{threshold} to determine boundary membership.
#' @param  \dots   Arguments passed to estimation command including \code{burnin}, \code{n.sample}, \code{thin}, and parameters for various priors.
#' @return  Object of class \code{carbayes} from package \code{CARbayes} with two additions:
#'   First, additional element \code{borders} of class \code{SpatialLinesDataFrame}, which includes 
#'   all border lines and the postior median estimates for the boundary likelihood value and the boundary 
#'   membership value (boundary probability if \code{threshold} is defined). Second, \code{sampes} includes
#'   additional elements for the McMC samples of the boundary likelihood value and the boundary membership value.
#' @export
areal_wombling_bayesian <- function(formula, family, sp, phi = "leroux", threshold = NA, ...) {
    if (!(phi %in% c("leroux", "IAR", "BYM"))) stop("Incorrect prior for the random effect.")
    # Coerce simple feature geometries to corresponding Spatial* objects
    if (is(sp, "sf")) sp <- as(sp, "Spatial")
    # get borders as line segments in SpatialLinesDataFrame
    sl <- border_lines(sp)
    # polygon adjacency matrix
    W.nb  <- spdep::poly2nb(sp, row.names = rownames(sp))
    W.mat <- spdep::nb2mat(W.nb, style = "B", zero.policy = TRUE)
    # rownames(W.mat) <- NULL
    # Bayesian hierarchical model with spatially correlated random effects
    if (phi == "leroux") m <- CARBayes::S.CARleroux(formula, family, data = sp@data, W = W.mat, ...)
    if (phi == "IAR") m <- CARBayes::S.CARiar(formula, family, data = sp@data, W = W.mat, ...)
    if (phi == "BYM") m <- CARBayes::S.CARbym(formula, family, data = sp@data, W = W.mat, ...)
    # posterior distribution of boundary likelihood value (BLV) and boundary membership value (BMV)
    # fitted values (mu): m$samples$fitted[1,] == m$samples$beta[1,] + m$samples$phi[1,]
    blv <- posterior_blv(m$samples$fitted, as.matrix(sl@data[,1:2]))
    if ( is.na(threshold)) bmv <- t(apply(blv, 1, function(iter) iter / max(iter)))
    if (!is.na(threshold)) bmv <- blv > threshold
    # create MCMC object for blv and bmv
    mcpar         <- attr(m$samples$beta, "mcpar")
    m$samples$blv <- coda::mcmc(blv, start = mcpar[1], end = mcpar[2], thin = mcpar[3])
    m$samples$bmv <- coda::mcmc(bmv, start = mcpar[1], end = mcpar[2], thin = mcpar[3])
    # add posterior median to SpatialLinesDataFrame
    sl$blv_median <- apply(blv, 2, median)
    if ( is.na(threshold)) sl$bmv_median <- apply(blv, 2, median)
    if (!is.na(threshold)) sl$bmv_mean <- colMeans(bmv)
    # return model and SpatialLinesDataFrame
    m$borders <- sl
    return(m)
}


#' Convert SpatialPolygonsDataFrame to SpatialLinesDataFrame with border segments
#'
#' \code{border_lines} converts a SpatialPolygonsDataFrame to a SpatialLinesDataFrame with one element for each border between neighbouring areas.
#'
#' @param sp Object of type \code{SpatialPolygonsDataFrame} (sf objects as converted)
#' @param longlat Use Euclidean or Great Circle distance for calculation of line length. If FALSE, Euclidean distance, if TRUE Great Circle distance in kilometers.
#' @return Object of class \code{SpatialLinesDataFrame} with one element for each border between neighbouring areas. 
#'
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @export
border_lines <- function(sp, longlat = TRUE) {
    # Coerce simple feature geometries to corresponding Spatial* objects
    if (is(sp, "sf")) sp <- as(sp, "Spatial")
    P  <- sp::polygons(sp)
    # get adjacency matrix A
    # nb <- spdep::poly2nb(sp, row.names = rownames(sp), queen = FALSE)
    # A  <- nb2mat::nb2mat(nb, style = "B", zero.policy = TRUE)
    nb <- spdep::poly2nb(sp, queen = FALSE)
    # create data.frame with adjacent areas
    greater_than <- function(a, b) a[a > b]
    data <- data.frame(i = 1:length(nb), j = NA) %>%
        group_by(i, j) %>%
        do(expand.grid(i = .$i, j = greater_than(nb[[.$i]], .$i))) %>%
        as.data.frame()
    # area borders as SpatialLines
    lines <- apply(data, 1, function(d) {
        i <- as.numeric(d["i"])
        j <- as.numeric(d["j"])
        # get list of coordinates for polygons
        c1 <- plyr::llply(P@polygons[[i]]@Polygons, sp::coordinates)
        c2 <- plyr::llply(P@polygons[[j]]@Polygons, sp::coordinates)
        # get borders for each combination of polygons
        grid <- expand.grid(s1 = 1:length(c1), s2 = 1:length(c2))
        line <- apply(grid, 1, function(obj) {
            a   <- c1[[obj["s1"]]]
            b   <- c2[[obj["s2"]]]
            # select intersecting rows
            sel <- a[, 1] %in% b[, 1] & a[, 2] %in% b[, 2]
            if(sum(sel) == 0) return(NULL)
            # create Line object for each sequence of matching coordinates
            runs <- rle(sel)
            runs <- data.frame(
                    val = runs$values,
                    i   = c(1, cumsum(runs$length) + 1)[-(length(runs$length) + 1)],
                    len = runs$length) %>%
                dplyr::filter(val)
            # coordinates for each sequence
            pos    <- plyr::alply(as.matrix(runs), 1, . %>% {.[["i"]] : (.[["i"]] + .[["len"]] - 1)})
            coords <- plyr::llply(pos, . %>% a[., , drop = FALSE])
            # remove duplicate line elements
            len_one <- plyr::laply(coords, nrow) == 1
            if (!all(len_one) & any(len_one)) {
                B <- do.call(rbind, coords)
                coords <- plyr::llply(coords, function(A) {
                    if(nrow(A) > 1) return(A)
                    cond <- sum(A[, 1] == B[, 1] & A[, 2] == B[, 2]) > 1
                    if(cond) return(NULL)
                    return(A)
                })
                coords <- coords[!sapply(coords, is.null)]
            }
            # return list of Line objects (one element for each sequence of coordinates)
            plyr::llply(coords, sp::Line)
        })
        segments <- unlist(line[!sapply(line, is.null)], recursive=FALSE)
        sp::Lines(segments, ID = sprintf("i%s_j%s", i, j))
    })
    sl <- sp::SpatialLines(lines[!sapply(lines, is.null)], proj4string = sp::CRS(sp::proj4string(sp)))
    # SpatialLinesDataFrame from SpatialLines and data
    sldf <- sp::SpatialLinesDataFrame(sl, data, match.ID = FALSE)
    # sldf$length <- SpatialLinesLengths(sldf, longlat = longlat)
    return(sldf)
}