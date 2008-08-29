
eigen2mat <- function(e) {
    n <- length(e$values)
    ans <- matrix(0, n, n)
    for (i in seq_len(n))
    {
        ans[] <- ans + e$values[i] * (e$vectors[, i] %*% t(e$vectors[, i]))
    }
    ans
}



## Parse a gatingML file into a filterSet

"parse.gatingML.http...www.isac.net.org.std.Gating.ML.v1.3._Gating.ML" = function(root, ...) {
    ml <- new("filterSet")
    idnum  <- 0
    genid <- function() {
        idnum <<- idnum + 1;paste("genid",idnum,sep = "")
    }
    
    gate <- function(g, ...) UseMethod("gate")	
    createGate <- function(type, g, args) {
        args$filterId <- xmlGetAttr(g, "id", genid())
        f  <- do.call(type, args)
        ml[[NULL]]  <- if(!is.null(xmlGetAttr(g, "parent_id", NULL))) {
            ## The parent_id tells us that we should use a subsetFilter and THAT is the filter
            ## that should get the proper name
            if(is(f, "concreteFilter")) identifier(f) = paste(identifier(f), "lhs", sep = "_")
            new("subsetFilter", filters = list(f, filterReference(ml,
                                xmlGetAttr(g, "parent_id"))),
                filterId = args$filterId)
        } else f
        filterReference(ml, args$filterId)
    }
    coordinate <- function(g) 
        sapply(xmlGrep(g, "http...www.isac.net.org.std.Gating.ML.v1.3._coordinate"),
               xmlGetAttr, "value", NA, as.numeric)

    vertices <- function(g, type = "http...www.isac.net.org.std.Gating.ML.v1.3._vertex") {
        do.call("rbind", lapply(xmlGrep(g, type), coordinate))
    }
    dimensions <- function(g, type = "parameter", default = "", ...) {
        sapply(xmlGrep(g, "http...www.isac.net.org.std.Gating.ML.v1.3._dimension"), 
               xmlGetAttr, type, default, ...)
    }
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._RectangleGate <- function(g, ...) {
        points <- rbind(min = dimensions(g, "min", -Inf, as.numeric), 
                        max = dimensions(g, "max", Inf, as.numeric))
        colnames(points) = dimensions(g)
        createGate("rectangleGate", g, list(.gate = points))
    }
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._PolygonGate <- function(g, ...) {
        points <- vertices(g)
        colnames(points) <- dimensions(g)
        if(ncol(points) != 2) stop("polygon gates must have two dimensions")
        createGate("polygonGate", g, list(boundaries = points))
    }
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._PolytopeGate <- function(g, ...) {
        points <- vertices(g, "http...www.isac.net.org.std.Gating.ML.v1.3._point")
        colnames(points) <- dimensions(g)
        createGate("polytopeGate", g, list(boundaries = points))
    }	
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._EllipsoidGate <-
        function(g, ...) {
        foci  <- vertices(g,
        "http...www.isac.net.org.std.Gating.ML.v1.3._focus")
        cat("foci =", foci, "\n")
        colnames(foci) = dimensions(g)
        a <- as.numeric(xmlValue(xmlGrep(g,
        "http...www.isac.net.org.std.Gating.ML.v1.3._distance")[[1]]))
	cat("a =", a, "\n")
	f <- sqrt(sum(diff(foci)^2))
        cat("f =", f, "\n")
        b <- 2 * sqrt(((a/2)^2) - ((f/2)^2))
        cat("b =", b, "\n")
	center <- apply(foci, 2, mean)
        names(center) <- dimensions(g)
	av <- center - foci[1,]
        cat("av =", av,"\n")
	avu <- av/(f/2)
	bv <- c(center[1] - (av[2] - center[2]), 
	        center[2] + (av[1] - center[1]))
        cat("bv =",bv,"\n")
        cat("center =",center,"\n")
        bvu <- bv / sqrt(sum((bv - center)^2))
        browser()
	cov <- eigen2mat(list(values=c((a/2)^2,(b/2)^2), vectors=cbind(avu,bvu)))
	dimnames(cov) <- list(dimensions(g), dimensions(g))
        createGate("ellipsoidGate", g, list(.gate=cov, mean=center))
    }
    getSide <- function(g, side) {
        leaf  <- paste("http...www.isac.net.org.std.Gating.ML.v1.3._leaf", side, sep = "")
        node  <- paste("http...www.isac.net.org.std.Gating.ML.v1.3._node", side, sep = "")
        VAL   <- xmlGrep(g, leaf)
        if(length(VAL) == 0) {
            VAL  <- xmlGrep(g, node)
            if(length(VAL) == 0) stop(paste(leaf, "or", node, "is required at all levels of a decision tree."))
        }
        VAL[[1]]
    }
    makeCall <- function(param, thres, LT, GTE) {
        ## if both sides result in a false entry 
        NUM  <- if((is.logical(LT) && LT) || is.call(LT)) 1 else 0
        NUM  <- NUM + if((is.logical(GTE) && GTE) || is.call(GTE)) 2 else 0
        LESS   <- as.call(c(as.symbol("<"), as.symbol(param), thres))
        MORE   <- as.call(c(as.symbol("> = "), as.symbol(param), thres))
        switch(NUM + 1, 
               FALSE , 
               if (is.logical(LT)) LESS else as.call(c(as.symbol("&"), LESS, LT)), 
               if(is.logical(GTE)) MORE else as.call(c(as.symbol("&"), MORE, GTE)), 
               as.call(c(as.symbol("|"),
                         as.call(as.symbol("&", LESS, LT)),
                         as.call(as.symbol("&"), MORE, GTE))))
    }
    
    decisionHelper <- function(g, ...) {
        param  <- xmlGetAttr(g, "parameter")
        thres  <- xmlGetAttr(g, "threshold", Inf, as.numeric)
        LT     <- getSide(g, "LT")
        GTE    <- getSide(g, "GTE")
        
        if (is(LT, "http...www.isac.net.org.std.Gating.ML.v1.3._leafLT"))
            LT <-
                if (xmlGetAttr(LT, "inside") == "true") TRUE
                else FALSE else LT  = decisionHelper(LT)
## (WAS)    if (is(GTE, "http...www.isac.net.org.std.Gating.ML.v1.3._leafGTE"))
##             GTE <-
##                 if(xmlGetAttr(GTE, "inside") == "true") TRUE
##                 else FALSE
##         else GTE <- decisionHelper(GTE)
        GTE <-
            if (is(GTE, "http...www.isac.net.org.std.Gating.ML.v1.3._leafGTE")) {
                if(xmlGetAttr(GTE, "inside") == "true") TRUE
                else FALSE
            }
            else decisionHelper(GTE)
        makeCall(param, thres, LT, GTE)
    }
    
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._DecisionTreeGate <- function(g, ...) {
        root  <- xmlChildren(g)[[1]]
        test  <- decisionHelper(root)
        createGate("expressionFilter", g, list(expr = test))
    }

    ## For some reason BooleanGates are special in Gating-ML and their gate id information is encapsulated in an
    ## outer type rather than simply defining OrGate and what have you. The suspicion is that this is a Java-ism
    ## that we don't employ so we need to punt the outer gate down to the real gate.
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._BooleanGate <- function(g, ...) {
        gate(xmlChildren(g)[[1]], g)
    }
    ## Boolean gate types
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._or <- function(g, g2, ...) {
        m  <- xmlChildren(g)
        m  <- lapply(m, function(x) gate(x))
        createGate("new", g2,
                   list(Class = "unionFilter",
                        filterId = "",
                        filters = m))
    }
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._and <- function(g, g2, ...) {
        m  <- xmlChildren(g)
        m  <- lapply(m, function(x) gate(x))
        createGate("new", g2,
                   list(Class = "intersectFilter",
                        filterId = "",
                        filters = m))
    }
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._not <- function(g, g2, ...) {
        createGate("new", g2,
                   list(Class = "complementFilter",
                        filterId = "",
                        filters = list(gate(xmlChildren(g)[[1]]))))
    }
    ## References
    gate.http...www.isac.net.org.std.Gating.ML.v1.3._gateReference <- function(g, ...) {
        filterReference(ml, xmlGetAttr(g, "ref"))
    }
    gate.default <- function(g, ...) {
        ## A debugging tool mostly. If this happens,  something has gone wrong.
        print(g)
    }
    ## Do the actual parsing
    for(g in xmlChildren(root)) {
        gate(g)
    }
    ml
}


parse.gatingML.default <- function(root, ...) {
    stop("Not a support Gating-ML XML Document")
}

parse.gatingML <- function(root, ...) {
    UseMethod("parse.gatingML")
}

read.gatingML <- function(file, ...)
    parse.gatingML(xmlRoot(smartTreeParse(file, ...)))
