#' Plot xts without date range
#'
#' a small tweak of plot.xts as presented by Joshua Ulrich
#' on https://stackoverflow.com/questions/50051076/changes-in-plotting-an-xts-object/50051183#50051183
#'
#' @param ... all arguments as known with plot.xts
#'
#' @author Ross Bennett (Tweak by Joshua Ulrich)
#' @return The same as plot.xts without the date range
#' @export

plotxts <-
function (x, y = NULL, ..., subset = "", panels = NULL, multi.panel = FALSE,
    col = 1:8, up.col = NULL, dn.col = NULL, bg = "#FFFFFF",
    type = "l", lty = 1, lwd = 2, lend = 1, main = deparse(substitute(x)),
    observation.based = FALSE, ylim = NULL, yaxis.same = TRUE,
    yaxis.left = TRUE, yaxis.right = TRUE, major.ticks = "months",
    minor.ticks = NULL, grid.ticks.on = "months", grid.ticks.lwd = 1,
    grid.ticks.lty = 1, grid.col = "darkgray", labels.col = "#333333",
    format.labels = TRUE, grid2 = "#F5F5F5", legend.loc = NULL)
{
    if (is.numeric(multi.panel)) {
        multi.panel <- min(NCOL(x), multi.panel)
        idx <- seq.int(1L, NCOL(x), 1L)
        chunks <- split(idx, ceiling(seq_along(idx)/multi.panel))
        if (length(lty) < ncol(x))
            lty <- rep(lty, length.out = ncol(x))
        if (length(lwd) < ncol(x))
            lwd <- rep(lwd, length.out = ncol(x))
        if (length(col) < ncol(x))
            col <- rep(col, length.out = ncol(x))
        if (!is.null(panels) && nchar(panels) > 0) {
            multi.panel <- FALSE
        }
        else {
            multi.panel <- TRUE
            panels <- NULL
            if (yaxis.same)
                ylim <- range(x[subset], na.rm = TRUE)
        }
        for (i in 1:length(chunks)) {
            tmp <- chunks[[i]]
            p <- plot.xts(x = x[, tmp], y = y, ... = ..., subset = subset,
                panels = panels, multi.panel = multi.panel, col = col[tmp],
                up.col = up.col, dn.col = dn.col, bg = bg, type = type,
                lty = lty[tmp], lwd = lwd[tmp], lend = lend,
                main = main, observation.based = observation.based,
                ylim = ylim, yaxis.same = yaxis.same, yaxis.left = yaxis.left,
                yaxis.right = yaxis.right, major.ticks = major.ticks,
                minor.ticks = minor.ticks, grid.ticks.on = grid.ticks.on,
                grid.ticks.lwd = grid.ticks.lwd, grid.ticks.lty = grid.ticks.lty,
                grid.col = grid.col, labels.col = labels.col,
                format.labels = format.labels, grid2 = grid2,
                legend.loc = legend.loc)
            if (i < length(chunks))
                print(p)
        }
        return(p)
    }
    cs <- new.replot_xts()
    if (is.null(major.ticks)) {
        xs <- x[subset]
        mt <- c(years = nyears(xs), months = nmonths(xs), days = ndays(xs))
        major.ticks <- names(mt)[rev(which(mt < 30))[1]]
    }
    plot.call <- match.call(expand.dots = TRUE)
    if (isTRUE(multi.panel)) {
        if (NCOL(x) == 1)
            cs$set_asp(3)
        else cs$set_asp(NCOL(x))
    }
    else {
        cs$set_asp(3)
    }
    cs$Env$cex <- if (hasArg("cex"))
        eval.parent(plot.call$cex)
    else 0.6
    cs$Env$mar <- if (hasArg("mar"))
        eval.parent(plot.call$mar)
    else c(3, 2, 0, 2)
    cs$Env$theme$up.col <- up.col
    cs$Env$theme$dn.col <- dn.col
    if (hasArg("colorset"))
        col <- eval.parent(plot.call$colorset)
    if (length(col) < ncol(x))
        col <- rep(col, length.out = ncol(x))
    cs$Env$theme$col <- col
    cs$Env$theme$rylab <- yaxis.right
    cs$Env$theme$lylab <- yaxis.left
    cs$Env$theme$bg <- bg
    cs$Env$theme$grid <- grid.col
    cs$Env$theme$grid2 <- grid2
    cs$Env$theme$labels <- labels.col
    cs$Env$theme$srt <- if (hasArg("srt"))
        eval.parent(plot.call$srt)
    else 0
    cs$Env$theme$las <- if (hasArg("las"))
        eval.parent(plot.call$las)
    else 0
    cs$Env$theme$cex.axis <- if (hasArg("cex.axis"))
        eval.parent(plot.call$cex.axis)
    else 0.9
    cs$Env$format.labels <- format.labels
    cs$Env$major.ticks <- major.ticks
    cs$Env$minor.ticks <- minor.ticks
    cs$Env$grid.ticks.on <- grid.ticks.on
    cs$Env$grid.ticks.lwd <- grid.ticks.lwd
    cs$Env$grid.ticks.lty <- grid.ticks.lty
    cs$Env$type <- type
    if (length(lty) < ncol(x))
        lty <- rep(lty, length.out = ncol(x))
    if (length(lwd) < ncol(x))
        lwd <- rep(lwd, length.out = ncol(x))
    cs$Env$lty <- lty
    cs$Env$lwd <- lwd
    cs$Env$lend <- lend
    cs$Env$legend.loc <- legend.loc
    cs$Env$call_list <- list()
    cs$Env$call_list[[1]] <- plot.call
    cs$Env$observation.based <- observation.based
    if (is.character(x))
        stop("'x' must be a time-series object")
    cs$Env$xdata <- x
    cs$Env$xsubset <- subset
    cs$Env$column_names <- colnames(x)
    cs$Env$nobs <- NROW(cs$Env$xdata)
    cs$Env$main <- main
    if (cs$Env$observation.based) {
        cs$Env$xycoords <- xy.coords(1:NROW(cs$Env$xdata[subset]))
        cs$set_xlim(c(1, NROW(cs$Env$xdata[subset])))
        cs$Env$xstep <- 1
    }
    else {
        xycoords <- xy.coords(.index(cs$Env$xdata[cs$Env$xsubset]),
            cs$Env$xdata[cs$Env$xsubset][, 1])
        cs$Env$xycoords <- xycoords
        cs$Env$xlim <- range(xycoords$x, na.rm = TRUE)
        cs$Env$xstep <- diff(xycoords$x[1:2])
        cs$set_xlim(cs$Env$xlim)
    }
    if (is.null(ylim)) {
        if (isTRUE(multi.panel)) {
            if (yaxis.same) {
                yrange <- range(cs$Env$xdata[subset], na.rm = TRUE)
            }
            else {
                yrange <- range(cs$Env$xdata[, 1][subset], na.rm = TRUE)
            }
        }
        else {
            yrange <- range(cs$Env$xdata[subset], na.rm = TRUE)
        }
        if (yrange[1L] == yrange[2L]) {
            if (yrange[1L] == 0) {
                yrange <- yrange + c(-1, 1)
            }
            else {
                yrange <- c(0.8, 1.2) * yrange[1L]
            }
        }
        cs$set_ylim(list(structure(yrange, fixed = TRUE)))
        cs$Env$constant_ylim <- range(cs$Env$xdata[subset], na.rm = TRUE)
    }
    else {
        cs$set_ylim(list(structure(ylim, fixed = TRUE)))
        cs$Env$constant_ylim <- ylim
    }
    cs$set_frame(1, FALSE)
    cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset],
        ticks.on = grid.ticks.on), segments(xycoords$x[atbt],
        get_ylim()[[2]][1], xycoords$x[atbt], get_ylim()[[2]][2],
        col = theme$grid, lwd = grid.ticks.lwd, lty = grid.ticks.lty)),
        clip = FALSE, expr = TRUE)
    cs$add_frame(0, ylim = c(0, 1), asp = 0.5)
    cs$set_frame(1)
    cs$add(expression(if (NROW(xdata[xsubset]) < 400) {
        axis(1, at = xycoords$x, labels = FALSE, col = theme$grid2,
            col.axis = theme$grid2, tcl = 0.3)
    }), expr = TRUE)
    cs$add(expression(axt <- axTicksByTime(xdata[xsubset], ticks.on = major.ticks,
        format.labels = format.labels), axis(1, at = xycoords$x[axt],
        labels = names(axt), las = theme$las, lwd.ticks = 1.5,
        mgp = c(3, 1.5, 0), tcl = -0.4, cex.axis = theme$cex.axis,
        col = theme$labels, col.axis = theme$labels)), expr = TRUE)
    if (!is.null(minor.ticks)) {
        cs$add(expression(axt <- axTicksByTime(xdata[xsubset],
            ticks.on = minor.ticks, format.labels = format.labels),
            axis(1, at = xycoords$x[axt], labels = FALSE, las = theme$las,
                lwd.ticks = 0.75, mgp = c(3, 1.5, 0), tcl = -0.4,
                cex.axis = theme$cex.axis, col = theme$labels,
                col.axis = theme$labels)), expr = TRUE)
    }
    text.exp <- expression(text(xlim[1], 0.5, main, font = 2,
                col = theme$labels, offset = 0, cex = 1.1, pos = 4))
    cs$add(text.exp, env = cs$Env, expr = TRUE)
    cs$set_frame(2)
    cs$Env$y_grid_lines <- function(ylim) {
        p <- pretty(ylim, 5)
        p[p > ylim[1] & p < ylim[2]]
    }
    exp <- expression(segments(xlim[1], y_grid_lines(get_ylim()[[2]]),
        xlim[2], y_grid_lines(get_ylim()[[2]]), col = theme$grid,
        lwd = grid.ticks.lwd, lty = grid.ticks.lty))
    if (yaxis.left) {
        exp <- c(exp, expression(text(xlim[1], y_grid_lines(get_ylim()[[2]]),
            noquote(format(y_grid_lines(get_ylim()[[2]]), justify = "right")),
            col = theme$labels, srt = theme$srt, offset = 1,
            pos = 2, cex = theme$cex.axis, xpd = TRUE)))
    }
    if (yaxis.right) {
        exp <- c(exp, expression(text(xlim[2], y_grid_lines(get_ylim()[[2]]),
            noquote(format(y_grid_lines(get_ylim()[[2]]), justify = "right")),
            col = theme$labels, srt = theme$srt, offset = 1,
            pos = 4, cex = theme$cex.axis, xpd = TRUE)))
    }
    cs$add(exp, env = cs$Env, expr = TRUE)
    cs$set_frame(2)
    if (isTRUE(multi.panel)) {
        lenv <- cs$new_environment()
        lenv$xdata <- cs$Env$xdata[subset, 1]
        lenv$label <- colnames(cs$Env$xdata[, 1])
        lenv$type <- cs$Env$type
        if (yaxis.same) {
            lenv$ylim <- cs$Env$constant_ylim
        }
        else {
            lenv$ylim <- range(cs$Env$xdata[subset, 1], na.rm = TRUE)
        }
        exp <- quote(chart.lines(xdata, type = type, lty = lty,
            lwd = lwd, lend = lend, col = theme$col, up.col = theme$up.col,
            dn.col = theme$dn.col, legend.loc = legend.loc))
        exp <- as.expression(add.par.from.dots(exp, ...))
        cs$add(exp, env = lenv, expr = TRUE)
        text.exp <- expression(text(x = xycoords$x[2], y = ylim[2] *
            0.9, labels = label, col = theme$labels, adj = c(0,
            0), cex = 1, offset = 0, pos = 4))
        cs$add(text.exp, env = lenv, expr = TRUE)
        if (NCOL(cs$Env$xdata) > 1) {
            for (i in 2:NCOL(cs$Env$xdata)) {
                lenv <- cs$new_environment()
                lenv$xdata <- cs$Env$xdata[subset, i]
                lenv$label <- cs$Env$column_names[i]
                if (yaxis.same) {
                  lenv$ylim <- cs$Env$constant_ylim
                }
                else {
                  yrange <- range(cs$Env$xdata[subset, i], na.rm = TRUE)
                  if (all(yrange == 0))
                    yrange <- yrange + c(-1, 1)
                  lenv$ylim <- yrange
                }
                lenv$type <- cs$Env$type
                lenv$lty <- cs$Env$lty[i]
                lenv$lwd <- cs$Env$lwd[i]
                lenv$col <- cs$Env$theme$col[i]
                cs$add_frame(ylim = c(0, 1), asp = 0.25)
                cs$next_frame()
                text.exp <- expression(text(x = xlim[1], y = 0.5,
                  labels = "", adj = c(0, 0), cex = 0.9, offset = 0,
                  pos = 4))
                cs$add(text.exp, env = lenv, expr = TRUE)
                cs$add_frame(ylim = lenv$ylim, asp = NCOL(cs$Env$xdata),
                  fixed = TRUE)
                cs$next_frame()
                exp <- quote(chart.lines(xdata[xsubset], type = type,
                  lty = lty, lwd = lwd, lend = lend, col = col,
                  up.col = theme$up.col, dn.col = theme$dn.col,
                  legend.loc = legend.loc))
                exp <- as.expression(add.par.from.dots(exp, ...))
                lenv$y_grid_lines <- function(ylim) {
                  p <- pretty(ylim, 5)
                  p[p > ylim[1] & p < ylim[2]]
                }
                exp <- c(exp, expression(segments(xlim[1], y_grid_lines(ylim),
                  xlim[2], y_grid_lines(ylim), col = theme$grid,
                  lwd = grid.ticks.lwd, lty = grid.ticks.lty)),
                  expression(atbt <- axTicksByTime2(xdata[xsubset],
                    ticks.on = grid.ticks.on), segments(xycoords$x[atbt],
                    ylim[1], xycoords$x[atbt], ylim[2], col = theme$grid,
                    lwd = grid.ticks.lwd, lty = grid.ticks.lty)))
                if (yaxis.left) {
                  exp <- c(exp, expression(text(xlim[1], y_grid_lines(ylim),
                    noquote(format(y_grid_lines(ylim), justify = "right")),
                    col = theme$labels, srt = theme$srt, offset = 1,
                    pos = 2, cex = theme$cex.axis, xpd = TRUE)))
                }
                if (yaxis.right) {
                  exp <- c(exp, expression(text(xlim[2], y_grid_lines(ylim),
                    noquote(format(y_grid_lines(ylim), justify = "right")),
                    col = theme$labels, srt = theme$srt, offset = 1,
                    pos = 4, cex = theme$cex.axis, xpd = TRUE)))
                }
                cs$add(exp, env = lenv, expr = TRUE, no.update = TRUE)
                text.exp <- expression(text(x = xycoords$x[2],
                  y = ylim[2] * 0.9, labels = label, col = theme$labels,
                  adj = c(0, 0), cex = 1, offset = 0, pos = 4))
                cs$add(text.exp, env = lenv, expr = TRUE)
            }
        }
    }
    else {
        if (type == "h" & NCOL(x) > 1)
            warning("only the univariate series will be plotted")
        exp <- quote(chart.lines(xdata[xsubset], type = type,
            lty = lty, lwd = lwd, lend = lend, col = theme$col,
            up.col = theme$up.col, dn.col = theme$dn.col, legend.loc = legend.loc))
        exp <- as.expression(add.par.from.dots(exp, ...))
        cs$add(exp, expr = TRUE)
        assign(".xts_chob", cs, .plotxtsEnv)
    }
    if (!is.null(panels) && nchar(panels) > 0) {
        panels <- parse(text = panels, srcfile = NULL)
        for (p in 1:length(panels)) {
            if (length(panels[p][[1]][-1]) > 0) {
                cs <- eval(panels[p])
            }
            else {
                cs <- eval(panels[p])
            }
        }
    }
    assign(".xts_chob", cs, .plotxtsEnv)
    cs
}
