.SIMDENV <- new.env(parent=emptyenv())

# return a character vector of functions defined in .GlobalEnv
parent_env_fun <- function(){
    nms <- ls(envir = parent.frame(2L))
    is_fun <- sapply(nms, function(x, envir) is.function(get(x, envir=envir)),
                     envir = parent.frame(2L))
    return(nms[is_fun])
}

load_packages <- function(packages){
    if(!is.null(packages)){
        for(pack in packages){
            available <- suppressWarnings(require(substitute(pack), character.only=TRUE,
                    quietly=TRUE, warn.conflicts=FALSE))
            if(!available)
                stop(sprintf("Package \'%s\' is not available. Please install.", pack),
                     call.=FALSE)
        }
    }
    invisible(NULL)
}

get_packages <- function(packages){
    sapply(packages, function(x) as.character(packageVersion(x)))
}

# base-code borrowed and modified from pbapply
timeFormater <- function(time, decimals = TRUE){
    dec <- time - floor(time)
    time <- floor(time - dec)
    dec <- round(dec, 2)
    sec <- round(time %% 60)
    if(decimals) sec <- sec + dec
    time <- floor(time / 60)
    minutes <- floor(time %% 60)
    time <- floor(time / 60)
    days <- floor(time / 24)
    time <- floor(time %% 24)
    hours <- floor(time %% 60)
    resTime <- ""
    if (days > 0)
        resTime <- sprintf("%02id ", days)
    if (hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02ih ", hours), sep = "")
    if (minutes > 0 || hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02im ", minutes), sep = "")
    resTime <- if(decimals) paste0(resTime, sprintf("%.2fs", sec))
    else paste0(resTime, sprintf("%02is", sec))
    resTime
}

print_progress <- function(row, trow, stored_time, RAM, progress,
                           condition, replications){
    if(progress) cat('\n')
    tmp <- as.list(subset(condition, select=colnames(condition) != "ID"))
    nms <- names(tmp)
    nms2 <- do.call(c, lapply(tmp, as.character))
    wdth <- 85 - 13
    condstring <- paste0(nms, '=', nms2, collapse=', ')
    if(nchar(condstring) > wdth){
        nms <- abbreviate(nms, minlength = 6)
        condstring <- paste0(nms, '=', nms2, collapse=', ')
        if(nchar(condstring) > wdth){
            nms2 <- abbreviate(nms2)
            condstring <- paste0(nms, '=', nms2, collapse=', ')
        }
    }
    if(RAM != "") RAM <- sprintf(';   RAM Used: %s', RAM)
    cat(sprintf('\rDesign: %i/%i%s;   Replications: %i;   Total Time: %s ',
                row, trow, RAM, replications, timeFormater(sum(stored_time))))
    cat(sprintf('\n Conditions: %s\n', condstring))
    if(progress) cat('\r')
    invisible(NULL)
}

myundebug <- function(fun) if(isdebugged(fun)) undebug(fun)

notification_condition <- function(condition, results, total){
    RPushbullet::pbPost(type = 'note',
                        title = sprintf("Condition %i/%i completed", condition$ID, total),
                        body = sprintf("Execution time: %s \nErrors: %i \nWarnings: %i",
                                       timeFormater(results$SIM_TIME),
                                       ifelse(is.null(results$ERRORS), 0, results$ERRORS),
                                       ifelse(is.null(results$WARNINGS), 0, results$WARNINGS)))

    invisible(NULL)
}

notification_final <- function(Final){
    RPushbullet::pbPost(type = 'note',
                        title = "Simulation completed",
                        body = sprintf("Total execution time: %s \nTotal Errors: %i \nTotal Warnings: %i",
                                       timeFormater(sum(Final$SIM_TIME)),
                                       ifelse(is.null(Final$ERRORS), 0, sum(Final$ERRORS)),
                                       ifelse(is.null(Final$WARNINGS), 0, sum(Final$WARNINGS))))
    invisible(NULL)
}

#' Suppress function messages and Concatenate and Print (cat)
#'
#' This function is used to suppress information printed from external functions
#' that make internal use of \code{link{message}} and \code{\link{cat}}, which
#' provide information in interactive R sessions. For simulations, the session
#' is not interactive, and therefore this type of output should be suppressed.
#' For similar behaviour for suppressing warning messages see
#' \code{\link{suppressWarnings}}, though use this function carefully as some
#' warnings can be meaningful and unexpected.
#'
#' @param ... the functional expression to be evaluated
#'
#' @param messages logical; suppress all messages?
#'
#' @param cat logical; suppress all concatenate and print calls from \code{\link{cat}}?
#'
#' @export
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @examples
#' myfun <- function(x){
#'    message('This function is rather chatty')
#'    cat("It even prints in different output forms!\n")
#'    message('And even at different....')
#'    cat("...times!\n")
#'    x
#' }
#'
#' out <- myfun(1)
#' out
#'
#' # tell the function to shhhh
#' out <- quiet(myfun(1))
#' out
#'
quiet <- function(..., messages=FALSE, cat=FALSE){
    if(!cat){
        tmpf <- tempfile()
        sink(tmpf)
        on.exit({sink(); file.remove(tmpf)})
    }
    out <- if(messages) eval(...) else suppressMessages(eval(...))
    out
}

#' Auto-named Concatenation of Vector or List
#'
#' This is a wrapper to the function \code{\link{c}}, however names the respective elements
#' according to their input object name. For this reason, nesting \code{nc()} calls
#' is not recommended (joining independent \code{nc()} calls via \code{c()}
#' is however reasonable).
#'
#' @param ... objects to be concatenated
#'
#' @param use.names logical indicating if \code{names} should be preserved (unlike \code{\link{c}},
#'   default is \code{FALSE})
#'
#' @param error.on.duplicate logical; if the same object name appears in the returning object
#'   should an error be thrown? Default is \code{TRUE}
#'
#' @export
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @examples
#'
#' A <- 1
#' B <- 2
#' C <- 3
#'
#' names(C) <- 'LetterC'
#'
#' # compare the following
#' c(A, B, C) # unnamed
#'
#' nc(A, B, C) # named
#' nc(this=A, B, C) # respects override named (same as c() )
#' nc(this=A, B, C, use.names = TRUE) # preserve original name
#'
#' \dontrun{
#' # throws errors if names not unique
#' nc(this=A, this=B, C)
#' nc(LetterC=A, B, C, use.names=TRUE)
#' }
#'
#' # poor input choice names
#' nc(t.test(c(1:2))$p.value, t.test(c(3:4))$p.value)
#'
#' # better to explicitly provide name
#' nc(T1 = t.test(c(1:2))$p.value,
#'    T2 = t.test(c(3:4))$p.value)
#'
#' # vector of unnamed inputs
#' A <- c(5,4,3,2,1)
#' B <- c(100, 200)
#'
#' nc(A, B, C) # A's and B's numbered uniquely
#' c(A, B, C)  # compare
#' nc(beta=A, B, C) # replacement of object name
#'
#' # retain names attributes (but append object name, when appropriate)
#' names(A) <- letters[1:5]
#' nc(A, B, C)
#' nc(beta=A, B, C)
#' nc(A, B, C, use.names=TRUE)
#'
#' # mix and match if some named elements work while others do not
#' c( nc(A, B, use.names=TRUE), nc(C))
#'
#' \dontrun{
#' # error, 'b' appears twice
#' names(B) <- c('b', 'b2')
#' nc(A, B, C, use.names=TRUE)
#' }
#'
#' # List input
#' A <- list(1)
#' B <- list(2:3)
#' C <- list('C')
#'
#' names(C) <- 'LetterC'
#'
#' # compare the following
#' c(A, B, C) # unnamed
#'
#' nc(A, B, C) # named
#' nc(this=A, B, C) # respects override named (same as c() and list() )
#' nc(this=A, B, C, use.names = TRUE) # preserve original name
#'
#'
nc <- function(..., use.names=FALSE, error.on.duplicate = TRUE){
    dots <- list(...)
    len <- sapply(dots, length)
    object <- as.list(substitute(list(...)))[-1L]
    nms <- sapply(object, function(x) paste0(as.character(x), collapse='_'))
    nms[names(nms) != ""] <- names(nms[names(nms) != ""])
    if(any(len > 1L)){
        nms <- as.list(nms)
        for(i in length(nms):1L){
            if(len[i] > 1L)
                nms[[i]] <- paste0(rep(nms[[i]], len[i]),
                                   if(!is.null(names(dots[[i]]))) "." else NULL,
                                if(is.null(names(dots[[i]]))) 1L:len[i]
                                else names(dots[[i]]))
        }
        nms <- do.call(c, nms)
    }
    if(use.names){
        tmp <- do.call(c, lapply(dots, function(x){
            ret <- names(x)
            if(is.null(ret)) ret <- rep(NA, length(x))
            ret
        }))
        nms[!is.na(tmp)] <- tmp[!is.na(tmp)]
    }
    nms <- gsub("\\$\\_", "", nms)
    if(error.on.duplicate)
        if(any(duplicated(nms)))
            stop(sprintf('Vector/list contains the following duplicated names: %s',
                         paste0(nms[duplicated(nms)], collapse=', ')),
                 call.=FALSE)
    ret <- c(...)
    names(ret) <- nms
    ret
}

isList <- function(x) !is.data.frame(x) && is.list(x)

reduceTable <- function(tab){
    tab <- dplyr::bind_rows(tab)
    uniq <- sort(unique(tab$x))
    reps <- val <- numeric(length(uniq))
    for(i in seq_len(length(val))){
        tmp <- tab[uniq[i] == tab$x, , drop=FALSE]
        reps[i] <- sum(tmp$reps)
        val[i] <- sum(as.numeric(tmp$y) * as.numeric(tmp$reps) / reps[i])
    }
    reduced <- data.frame(y=val, x=uniq, reps=reps)
    reduced

}

sim_results_check <- function(sim_results){
    if(is(sim_results, 'try-error'))
        stop(c("Summarise() should not throw errors. Message was:\n    ", sim_results), call.=FALSE)
    if(is.data.frame(sim_results)){
        if(nrow(sim_results) > 1L)
            stop('When returning a data.frame in summarise() there should only be 1 row',
                 call.=FALSE)
        nms <- names(sim_results)
        sim_results <- as.numeric(sim_results)
        names(sim_results) <- nms
    }
    if(isList(sim_results)){
        if(is.null(names(sim_results)))
            stop("List elements must be named in Summarise() definition",
                 call.=FALSE)
        ret <- numeric(0)
        attr(ret, 'summarise_list') <- sim_results
        return(ret)
    }
    if(length(sim_results) == 1L){
        if(is.null(names(sim_results)))
            names(sim_results) <- 'value'
        if(!is.vector(sim_results) || is.null(names(sim_results)))
            stop('summarise() must return a named vector or data.frame object with 1 row',
                 call.=FALSE)
    }
    sim_results
}

unwind_apply_wind.list <- function(lst, mat, fun, ...){
    long_list <- do.call(rbind, lapply(lst, as.numeric))
    long_mat <- if(!is.null(mat)) as.numeric(mat) else NULL
    ret <- fun(long_list, long_mat, ...)
    if(!is.null(mat)){
        was_matrix <- is.matrix(mat)
        if(was_matrix){
            ret <- matrix(ret, nrow(mat), ncol(mat))
            rownames(ret) <- rownames(mat)
            colnames(ret) <- colnames(mat)
        } else names(ret) <- names(mat)
    }
    ret
}

combined_Analyses <- function(condition, dat, fixed_objects = NULL){
    if(!is.null(.SIMDENV$ANALYSE_FUNCTIONS)){
        ANALYSE_FUNCTIONS <- .SIMDENV$ANALYSE_FUNCTIONS
        TRY_ALL_ANALYSE <- .SIMDENV$TRY_ALL_ANALYSE
    }
    nfuns <- length(ANALYSE_FUNCTIONS)
    ret <- vector('list', nfuns)
    nms <- names(ANALYSE_FUNCTIONS)
    names(ret) <- nms
    if(is.null(nms)) nms <- 1L:nfuns
    for(i in nms){
        tried <- try(ANALYSE_FUNCTIONS[[i]](condition=condition, dat=dat,
                                            fixed_objects=fixed_objects), silent=TRUE)
        if(is(tried, 'try-error')){
            if(tried == 'Error : ANALYSEIF RAISED ERROR\n')
                tried <- NULL
            else if(!TRY_ALL_ANALYSE) return(tried)
        }
        ret[[i]] <- tried
    }
    if(TRY_ALL_ANALYSE){
        try_error <- sapply(ret, function(x) is(x, 'try-error'))
        if(any(try_error)){
            msg <- paste0(names(ANALYSE_FUNCTIONS)[try_error], ".ERROR:   ", as.character(ret[try_error]))
            if(length(msg) > 1L)
                msg[1L] <- sprintf("%i INDEPENDENT ERRORS THROWN:   %s", length(msg), msg[1L])
            msg <- paste0(msg, collapse = '  ')
            ret <- try(stop(msg), silent = TRUE)
            ret <- gsub("Error in try\\(stop\\(msg\\), silent = TRUE\\) : \\\n  ", "", ret)
            return(ret)
        }
    }
    if(all(sapply(ret, function(x) is.numeric(x) ||
                  (is.data.frame(x) && nrow(x) == 1L))))
        ret <- unlist(ret)
    ret
}

combined_Generate <- function(condition, fixed_objects = NULL){
    if(!is.null(.SIMDENV$GENERATE_FUNCTIONS))
        GENERATE_FUNCTIONS <- .SIMDENV$GENERATE_FUNCTIONS
    nfuns <- length(GENERATE_FUNCTIONS)
    ret <- vector('list', nfuns)
    nms <- names(GENERATE_FUNCTIONS)
    names(ret) <- nms
    if(is.null(nms)) nms <- 1L:nfuns
    for(i in nms){
        tried <- try(GENERATE_FUNCTIONS[[i]](condition=condition,
                                            fixed_objects=fixed_objects), silent=TRUE)
        if(is(tried, 'try-error')){
            if(tried == 'Error : GENERATEIF RAISED ERROR\n')
                tried <- NULL
        } else ret <- tried
    }
    if(is.null(ret))
        stop('No data was generated for supplied condition. Please fix', call.=FALSE)
    ret
}

toTabledResults <- function(results){
    tabled_results <- if(is.data.frame(results[[1]]) && nrow(results[[1L]]) == 1L){
        as.matrix(dplyr::bind_rows(results))
    } else if((is.data.frame(results[[1]]) && nrow(results[[1]]) > 1L) || is.list(results[[1L]])){
        results
    } else {
        as.matrix(dplyr::bind_rows(as.data.frame(do.call(rbind, results))))
    }
    tabled_results
}

stackResults <- function(results){
    if(!is.list(results[[1L]]) || (is.data.frame(results[[1L]]) &&
                                   nrow(results[[1L]]) == 1L)){
        old_nms <- names(results[[1L]])
        results <- as.data.frame(do.call(rbind, results))
        if(length(unique(colnames(results))) != ncol(results) && ncol(results) > 1L)
            stop('Object of results returned from analyse must have unique names', call.=FALSE)
        rownames(results) <- NULL
        if(ncol(results) == 1L && is.null(old_nms)) results <- results[,1]
    }
    results
}

SimSolveData <- function(burnin, full = TRUE){
    pick <- !sapply(.SIMDENV$stored_results, is.null)
    pick[1L:burnin] <- FALSE
    if(!any(pick))
        return(data.frame(y=numeric(0), IV=numeric(0), weights=numeric(0)))
    if(full){
        DV <- do.call(c, .SIMDENV$stored_results[pick])
        IV <- rep(.SIMDENV$stored_medhistory[pick],
                  times=sapply(.SIMDENV$stored_results[pick], length))
        ret <- data.frame(y=DV, x=IV, weights=1)
    } else {
        ret <- do.call(rbind, .SIMDENV$stored_history[pick])
        ret$weights <- 1/sqrt(ret$reps)
    }
    ret
}

SimSolveUniroot <- function(SimMod, b, interval, max.interval, median, CI=NULL){
    f.root <- function(x, b)
        predict(SimMod, newdata = data.frame(x=x), type = 'response') - b
    res <- try(uniroot(f.root, b=b, interval = interval), silent = TRUE)
    if(is(res, 'try-error')){
        # in case original interval is poor for interpolation
        interval <- max.interval
        for(i in seq_len(20L)){
            if(grepl('end points not of opposite sign', res)){
                diff <- abs(interval - median)
                interval[which.max(diff)] <- mean(c(median, interval[which.max(diff)]))
                res <- try(uniroot(f.root, b=b, interval = interval), silent = TRUE)
                if(!is(res, 'try-error')) break
            }
        }
    }
    if(is(res, 'try-error')) return(c(NA, NA, NA))
    root <- res$root
    abias <- bias(root, median, type = 'abs_relative')
    if(abias > .5) root <- median
    ci <- c(NA, NA)
    if(!is.null(CI)){
        preds <- predict(SimMod, newdata = data.frame(x=root),
                      se.fit=TRUE, type = 'link')
        ci <- SimMod$family$linkinv(preds$fit + qnorm(CI) * preds$se.fit)
    }
    c(root, ci)
}

collect_unique <- function(x){
    if(any(duplicated(colnames(x)))){
        uniq <- unique(colnames(x))
        for(u in uniq){
            pick <- colnames(x) %in% u
            if(sum(pick) == 1L) next
            whc <- sort(which(pick))
            tmp <- rowSums(x[,pick, drop=FALSE], na.rm = TRUE)
            tmp <- ifelse(tmp == 0, NA, tmp)
            x[[whc[1L]]] <- tmp
            x[whc[2L:length(whc)]] <- NULL
        }
    }
    x
}

bisection <- function (f, interval, ..., tol = 0.001, maxiter = 100,
                       f.lower = NULL, f.upper = NULL, check = FALSE)
{
    lower <- interval[1L]
    upper <- interval[2L]
    iter <- 0L
    if(check){
        if(is.null(f.lower)) f.lower <- f(lower, ...)
        if(is.null(f.upper)) f.upper <- f(upper, ...)
        stopifnot("No root in specified interval" = f.lower * f.upper < 0)
    } else {
        if(is.null(f.lower)) f.lower <- -Inf
        if(is.null(f.upper)) f.upper <- Inf
    }
    if(f.lower > f.upper){
        tmp <- lower
        lower <- upper
        upper <- tmp
        tmp <- f.lower
        f.lower <- f.upper
        f.upper <- tmp
    }
    false_converge <- FALSE
    for(i in 1L:maxiter){
        iter <- iter + 1L
        mid <- (lower + upper)/2
        f.mid <- f(mid, ...)
        if(f.mid < f.lower || f.mid > f.upper){
            false_converge <- TRUE
            break
        }
        if (isTRUE(f.lower * f.mid > 0)){
            lower <- mid
            f.lower <- f.mid
        } else {
            upper <- mid
            f.upper <- f.mid
        }
        if(abs(lower - upper) < tol) break
    }
    root <- (lower + upper)/2
    list(root=root, f.root=f(root, ...), iter=i,
         terminated_early=i < maxiter,
         false_converge=false_converge)
}

RAM_used <- function(){
    # borrowed and modified from pryr::node_size(), 13-06-2023
    bit <- 8L * .Machine$sizeof.pointer
    if (!(bit == 32L || bit == 64L)) {
        stop("Unknown architecture", call. = FALSE)
    }
    val <- if (bit == 32L) 28L else 56L
    # end borrowed portion
    bytes <- sum(gc()[, 1] * c(val, 8))
    size <- structure(bytes, class="object_size")
    format(size, 'MB')
}

clip_names <- function(vec, maxchar = 150L){
    names(vec) <- strtrim(names(vec), width=maxchar)
    vec
}

#' Form Column Standard Deviation and Variances
#'
#' Form column standard deviation and variances for numeric arrays (or data frames).
#'
#' @param x an array of two dimensions containing numeric, complex, integer or logical values,
#'   or a numeric data frame
#'
#' @param na.rm logical; remove missing values in each respective column?
#'
#' @param unname logical; apply \code{\link{unname}} to the results to remove any variable
#'   names?
#'
#' @seealso \code{\link{colMeans}}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#'
#' results <- matrix(rnorm(100), ncol=4)
#' colnames(results) <- paste0('stat', 1:4)
#'
#' colVars(results)
#' colSDs(results)
#'
#' results[1,1] <- NA
#' colSDs(results)
#' colSDs(results, na.rm=TRUE)
#' colSDs(results, na.rm=TRUE, unname=TRUE)
#'
colVars <- function(x, na.rm=FALSE, unname=FALSE){
    ret <- apply(x, 2L, FUN = var, na.rm=na.rm)
    if(unname) ret <- unname(ret)
    ret
}

#' @export
#' @rdname colVars
colSDs <- function(x, na.rm=FALSE, unname=FALSE){
    sqrt(colVars(x=x, na.rm=na.rm, unname=unname))
}

pickReps <- function(replications, iter){
    ret <- if(iter > length(replications))
        max(replications) else replications[iter]
    ret
}
