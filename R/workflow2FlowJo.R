## For each XML node in FlowJo's worspaces we have a separate constructor
## function. The XML has the following structure:
##  Worksapce (xmlWorkspace)
##    SampleList (xmlSampleList)
##      Sample (xmlSample)
##        DataSet (xmlDataSet)
##        Keywords (xmlKeywords)
##          Keyword (xmlKeýword)
##          Keyword...
##        SampleNode (xmlSampleNode)
##          Subpopulations (xmlSubpopulations)
##            Population (xmlSubpopulations)
##              Gate (xmlGate)          
##                gating:PolygonGate (xmlPolygonGate)
##                  gating:dimension (xmlDimension)
##                  gating:dimension...
##                  gating:vertex (xmlVertex)
##                  gating:vertex...
##            Subpopulations...
##            Subpopulations...
##          Subpopulations...
##      Sample...


 ## Return default attributes for an XML node of type 'tag' potentially 
 ## replacing values by the content of 'attrs' (in form of a named list)
 ## or by additional named '...' arguments.
 targs <- function(tag, attrs=NULL, ...)
 {
    defs <- fjSettings()
    tnam <- gsub(".*:", "", tag)
    if(!tnam %in% names(defs))
      stop("'", tnam, "' is not a valid XML tag in this context.")
    res <- defs[[tnam]]
    args <- c(list(...), attrs)
    if(length(args) && length(names(args)))
    {
      args <- args[names(args)!=""]
      res[names(args)] <- args
    }
    if(!is.null(res))
    {
      res <- res[!sapply(res, is.null)]
      n <- names(res)
      res <- as.character(res)
      names(res) <- n
    }
    return(res)
  } 
    
    
  ## Create XML node of type 'tag' taking the default attributes unless
  ## specifically altered via the 'attrs' argument. Further children of 
  ## the node can be passed in as a list using 'children' or as named
  ## '...' arguments
  xmlTag <- function(tag, attrs=NULL, children=NULL, ...)
  {
    mf <- list(...)
    tn <- if("namespace" %in% names(mf)) 
      paste(mf$namespace, tag, sep=":") else tag
    if(!is.list(children) || is(children, "XMLNode"))
      children <- list(children)
    xmlNode(name=tag, attrs=targs(tn, attrs=attrs), 
      .children=children, ...) 
  }
  


## The workspace XML node. This is the main container for the information about
## samples, groups and gates. Non of the node attributes seems to be mandatory
## but we nonetheless provide their defaults here. The 'children' argument is
## supposed to be a list of XML children nodes, hence this function has to be
## called last when building the tree, after all children haven been realized.
## 'outfile' is the link to the file in which the workspace is subsequently 
## written. Additional arguments in '...' will be treated as tag attributes.
xmlWorkspace <- function(children=NULL, outfile, ...)
{
  xmlTag("Workspace", attrs=list(modDate=format(Sys.time(), 
    "%a %b %d %H:%M:%S %Z %Y"), nonAutoSaveFileName=outfile,
    ...), children=children)
}


## The WindowPosition XML node. Additional arguments in '...' will be treated 
## as tag attributes.
xmlWindowPosition <- function(...)
{
  xmlTag("WindowPosition", attrs=list(...))
}


## The TextTraits XML node. The 'name' attribute is supposed to be a character
## mapping the node to a FlowJo structure. Additional arguments in '...' 
## will be treated as tag attributes.
xmlTextTraits <- function(name="Workspace", ...)
{
  xmlTag("TextTraits", attrs=list(...))
} 


## The Columns XML node. The children are 'TColumn' tags that are generated
## from the 'columns' argument which is a numeric vector of column widths and
## the names of the vector items are used as column headers. Additional 
## arguments in '...' will be treated as tag attributes.
xmlColumns <- function(sort="$BTIM", columns, ...)
{
  if(missing(columns))
  {
    columns <- c("240", "80", "80")
    names(columns) <- c("Name", "Statistic", "#Cells")
  }
  else
  {
    if(!is.numeric(columns) || is.null(names(columns)))
      stop("'columns' must be a named numeric vector")
  }
  tcols <- mapply(function(n,w) xmlTag("TColumn", attrs=list(name=n, width=w)), 
    names(columns), columns, SIMPLIFY=FALSE)
  xmlTag("Columns", attrs=list(wsSortOrder=sort, ...), children=tcols)
}


## The Groups XML node. We put all samples in the single group 'All Samples'.
## The 'set' argument is supposed to be the flowSet.
xmlGroups <- function(set)
{
  refs <- xmlTag("SampleRefs", children=lapply(1:length(set), function(x)
    xmlTag("SampleRef", attrs=list(sampleID=x))), simplify=FALSE)
  grp <- xmlTag("Group", children=list(xmlTag("Criteria"), refs))
  grpNode <- xmlTag("GroupNode", children=list(xmlGraph(count=FALSE), grp))
  xmlTag("Groups", children=grpNode)
}


## The TableEditor XML node. The 'title' argument is only used for the window
## header, it should be the name of the workspace xml file.
xmlTableEditor <- function(title="workspace.wsp")
{
  tab <-  xmlTag("Table", children=xmlTag("Iteration"))
  wp <- xmlWindowPosition(x="0", y="0", width="675", height="340", 
    displayed="0", panelState=NULL)
  xmlTag("TableEditor", attrs=list(title=sprintf("FlowJo Tables - %s", title)),
    children=list(tab, wp))
}


## The LayoutEditor XML node. The 'title' argument is only used for the window
## header, it should be the name of the workspace xml file.
xmlLayoutEditor <- function(title="workspace.wsp")                                                      
{
  lo <-  xmlTag("Layout", children=list(xmlTag("FigList"), xmlTag("Iteration")))
  wp <- xmlWindowPosition(x="0", y="0", width="650", width="624", 
    displayed="0", panelState=NULL)
  xmlTag("LayoutEditor", attrs=list(title=sprintf("FlowJo Tables - %s", title)),
    children=list(lo, wp))
}


## The CompensationEditor XML node. The 'title' argument is only used for the 
## window header, it should be the name of the workspace xml file.
## FIXME: How does that look like with a spilloever matrix?
xmlCompensationEditor <- function(title="workspace.wsp", spillover=FALSE)
{
  comp <- if(!spillover) NULL else
  xmlTag("Compensation", children=list(xmlTag("spilloverMatrix", 
    namespace="comp"), xmlTag("Transformations",
     children=xmlTag("transformation", namespace="transforms", 
     children=xmlTag("pre-defined", namespace="transforms", 
     children=xmlTag("logicle", namespace="transforms"))))))
  
  xmlTag("CompensationEditor", attrs=list(title=sprintf("FlowJo Tables - %s", 
    title)), children=list(comp, xmlWindowPosition(x="0", y="0", width="800", 
    height="570", displayed="0", panelState=NULL)))
} 


## The DivaSettings XML node.
xmlDivaSettings <- function()
  xmlTag("DivaSettings")
     

## The SampleList XML node. This is basically just a wrapper for multiple 
## 'Sample' child nodes.
xmlSampleList <- function(Sample)
  xmlTag("SampleList", children=Sample)


## The Sample XML node. All the information about a single sample, both 
## regarding the linked data file and the full gating hierarchy if present.
xmlSample <- function(DataSet, Keywords, SampleNode)
  xmlTag("Sample", children=list(DataSet, Keywords, SampleNode))


## The DataSet XML node. Information about the linked data file for a 
## particular sample. We constuct this from the flowFrame identifier, so we
## have to make sure ahead of time that it matches the filename. The sample ID
## is just an integer number and should be supplied by an appropriate iterator.
xmlDataSet <- function(frame, id)
  xmlTag("DataSet", attrs=list(uri=file.path(".", identifier(frame), 
    fsep="\\"), sampleID=id))

  
## The Keywords XML node. Those are directly extracted from the flowFrame. 
## FlowJo is a bit picky about which characters are allowed in the keyword
## names. In case the file does not load properly, this is a good point to 
## start debugging.
xmlKeywords <- function(frame)
{
  kw <- sapply(description(frame), paste, collapse=" ")
  sel <- sapply(names(kw), function(x) length(grep("&", x, fixed=TRUE))>0)
  kw <- kw[!sel]
  xkw <- mapply(function(n, v) xmlTag("Keyword", attrs=list(name=n, value=v)),
    names(kw), kw, SIMPLIFY=FALSE)
  xmlTag("Keywords", children=xkw)
}


## The SampleNode XML node. This represents a single base node in FlowJo's event 
## tree. It maps back into the DataSet node via the sampleID attribute. 
xmlSampleNode <- function(frame, id, gates=NULL, transforms=NULL, level=0)
{
  pars <- if(is.null(gates)) 1:2 
    else if(level==0) parameters(gates$gates[[names(gates$tree)]]) 
    else parameters(gates$gates[[names(gates$tree[[level]])]]) 
  spops <- if(length(gates$tree)) xmlSubpopulations(gates, transforms, 
    level=level, frame=frame) else NULL
  g <- xmlGraph(frame[,pars])
  xmlTag("SampleNode", attrs=list(name=identifier(frame), count=nrow(frame),
      sampleID=id), children=list(spops, g))
}


## The Graph XML node. This sets the defaults for the plots.
xmlGraph <- function(frame, count=TRUE)
{
  d <- if(missing(frame)) c("x", "y") else head(c("x", "y"), ncol(frame))
  n <- if(missing(frame)) c("", "") else head(colnames(frame), 2)
  m <- if(missing(frame)) c("0.0", "0.0") else head(range(frame)["max",], 2)
  ax <- mapply(function(d, n ,m) xmlTag("Axis", attrs=list(dimension=d, 
    name=n, max=m)), d, n, m, SIMPLIFY=FALSE)
  traits <- lapply(c("Labels", "", "Numbers"), xmlTextTraits)
  wp <- xmlWindowPosition(x="623", y="20", width="382", height="526", 
    displayed="1")
  ats <- if(count && !missing(frame)) list(rowCount=nrow(frame)) else NULL
  xmlTag("Graph", attrs=ats, children=c(ax, traits, list(wp))) 
}


## The Subpopulations XML node. This function is recursive and recreates the
## gating tree. The 'gates' and 'gresults' arguments have to be named list of 
## matching filter and filterResult objects, and the named list structure
## in 'glist' should represent the tree. E.g., if the names in 'gates' are
## 'a', 'b' and 'c', then glist=list(a=list(b=list()), c=list()) would create
## a tree with 'b' nested in 'a' and 'c' as a separate node on the same level
## as 'a'.
## The Populations XML node contained within represents a single gating node in 
## FLowJo's event tree. Population nodes can be nested using further 
## Subpopulation nodes, creating the hierarchical structure. This is 
## archived by recursively passing additional arguments on to the 
## xmlSubpopulation constructor. We also pass  the filterResult because we 
## need to know the number of events after the gating. Note the recomputing 
## the event counts in FlowJo could potentially  yield slightly different 
## results.
xmlSubpopulations <- function(gates, transforms, level, frame)
{
  pop <- c("<wrapper xmlns:gating=\"dummy\" xmlns:data-type=\"dummy\">",
    xmlSubpopulationsHelper(gates=gates$gates, gresults=gates$result, 
      glist=gates$tree, transforms=gates$transforms, level=level, frame=frame), 
    "</wrapper>")
  xmlTreeParse(pop, asText=TRUE, 
    addAttributeNamespaces=TRUE)$doc$children[[1]][[1]]
}

xmlSubpopulationsHelper <- function(gates, gresults, glist, pops=NULL,
  transforms, level, frame)
{
  children <- names(glist)
  if(!is.null(children) && any(children %in% names(gates)))
  {
    level <- level+1
    pops <- append(pops, "<Subpopulations>")
  } 
  for(i in children)
  {
    g <- NULL
    if(i %in% names(gates))
    { 
      if(i == children[[1]])
      {
        tmpglist <- glist[[1]]
        grandchild <- NULL
        while(!is.null(names(tmpglist)))
        {
          ng <- names(tmpglist)[1]
          if(ng %in% names(gates))
          { 
            grandchild <- ng
            break
          }
          tmpglist <- tmpglist[[1]]
        }
        cparms <- if(is.null(grandchild)) parameters(gates[[i]]) 
          else parameters(gates[[grandchild]])
          g <- toString.XMLNode(xmlGraph(frame[,cparms]))  
      }
      pops <- c(pops, sprintf(paste("<Population name=\"%s\" annotation=\"\"", 
        "owningGroup=\"\" expanded=\"1\" sortPriority=\"10\" count=\"%s\">"),
        identifier(gates[[i]]), toTable(summary(gresults[[i]]))$true), g,
        toString.XMLNode(xmlGateNode(gates[[i]], i, transforms[[i]], gresults[[i]])))
    }
    pops <- xmlSubpopulationsHelper(gates, gresults, glist[[1]], pops, transforms,
      level=level, frame=frame)
    if(i %in% names(gates))
      pops <- append(pops, "</Population>")
  }
  if(!is.null(children) && any(children %in% names(gates)))
  pops <- append(pops, "</Subpopulations>")
  return(pops)
}


## The Gate XML node. This holds the geometric definiton of a gate. The 
## translateGate function makes sure that we created the appropriate 
## representation for the respective gate types.
xmlGateNode <- function(gate, id, transforms, gres=NULL)
{
  xmlTag("Gate", attrs=c("gating:id"=id), children=translateGate(gate,
    transforms, gres))
} 


## Create a unique integer identifier for a gate.
guid <- function(...) substr(as.character(as.vector(as.integer(Sys.time())/
  runif(1) * proc.time()["elapsed"])), 0, 8)
                    

## The PolygonGate XML node. This represents dimensions and vertices for a 
## single polygon gate.
xmlPolygonGateNode <- function(gate, tf)
{
  if(!missing(tf) && !is.null(tf))
  {
    for(p in parameters(gate))
      gate@boundaries[,p] <- tf[[p]](gate@boundaries[,p])
  }     
  dims <- lapply(parameters(gate), xmlDimensionNode)
  verts <- apply(gate@boundaries, 1, xmlVertexNode)
  xmlTag("PolygonGate", namespace="gating", children=c(dims, verts))
}



xmlEllipsoidGateNode <- function(gate, tf)
{
  parms <- parameters(gate)
  if(length(parms)!=2)
    stop("FlowJo only supports 2D ellipsoidal gates.")
  dims <- lapply(parms, xmlDimensionNode)
  center <- gate@mean[parms]
  if (is.null(rownames(gate@cov))) 
    rownames(gate@cov) <- colnames(gate@cov)
  cov <- gate@cov[parms, parms]
  radius <- gate@distance
  ev <- eigen(cov)
  eVal <- sqrt(ev$values)*radius
  eVect <- ev$vectors
  names(eVal) <- colnames(eVect) <- parms
  ans <- rbind(center - eVal[1]*eVect[,1],
               center + eVal[1]*eVect[,1],
               center - eVal[2]*eVect[,2],
               center + eVal[2]*eVect[,2])/4
  fd <- sqrt(eVal[1]^2 - eVal[2]^2)
  f <- rbind(center - fd*eVect[,1], center + fd*eVect[,1])
  if(!missing(tf) && !is.null(tf))
  {
    for(p in parms)
    {
      ans[,p] <- tf[[p]](ans[,p])
      f[,p] <- tf[[p]](f[,p])
    }
  }
  foci <- xmlFociNode(apply(f, 1, xmlVertexNode))
  verts <- xmlEdgeNode(apply(ans, 1, xmlVertexNode))
  xmlTag("EllipsoidGate", namespace="gating", children=c(dims, list(foci, verts)))
}


xmlRectangleGateNode <- function(gate, tf)
{
  pars <- parameters(gate)
  if(!missing(tf) && !is.null(tf))
  {
    gate@min <- sapply(pars, function(x) 
      as.vector(tf[[x]](gate@min[x])))
    gate@max <- sapply(pars, function(x) 
      as.vector(tf[[x]](gate@max[x])))
  }
  dims <- lapply(parameters(gate), function(x) 
    xmlDimensionNode(parameter=x, min=gate@min[x], max=gate@max[x]))
  xmlTag("RectangleGate", namespace="gating", children=dims)
}


## The dimension XML node. Basically the parameter name. For polygon gates there
## need to be two of those.
xmlDimensionNode <- function(parameter, min=NULL, max=NULL)
{
  xmlTag("dimension", namespace="gating", 
    children=xmlTag("parameter", namespace="data-type", 
      attrs=list("data-type:name"=parameter)), 
      attrs=list("gating:min"=as.vector(min), 
      "gating:max"=as.vector(max)))
}


## The vertex XML node. A single vertex in a polygon gate. It consists of two
## coordinate subnodes, one for each of the two dimensions.
xmlVertexNode <- function(xy)
{
  xmlTag("vertex", namespace="gating",
    children=lapply(xy, function(x) xmlTag("coordinate", 
    namespace="gating", attrs=list("data-type:value"=x))))
}


## The edges of an ellipse gate, essentially a collection of 4 vertex nodes
xmlEdgeNode <- function(xy)
{
  xmlTag("edge", namespace="gating", children=xy)
}


## The foci of an ellipse gate, essentially a collection of 2 vertex nodes
xmlFociNode <- function(xy)
{
  xmlTag("foci", namespace="gating", children=xy)
}


## Build a single xmlSample node from a flowFrame, accessed via an integer 
## index. This is supposed to be called using lapply, and the resulting list
## can be the input to the xmlSampleList constructor.
createSample <- function(i, set, gates, transforms)
  {
    kw <- xmlKeywords(set[[i]])
    ds <- xmlDataSet(set[[i]], i)
    sn <- xmlSampleNode(set[[i]], i, gates[[i]], transforms)
    xmlSample(ds, kw, sn)
  }


## The main function to create a flowJo workspace from a flowSet. The gating
## hierarchy needs to be supplied as a separate list argument, where each list
## item contains the (potentially nested) gates and gating results for a 
## particular sample.
createWorkspace <- function(set, outdir="flowJo", filename="workspace.wsp",
  gates=NULL, transforms=NULL)
{
  ## Setting the sampleNames of the flowSet to whatever write.flowSet will 
  ## create later in order to be sure that identifiers and file names match.
  sn <- sampleNames(set)
  hasExt <- sapply(sn, function(x) length(grep(".", x, fixed = TRUE))>0)
  sn[!hasExt] <- paste(sn[!hasExt], "fcs", sep=".")
  sampleNames(set) <- sn
  ## We write our flowSet out as FCS files and read it back in to guarantee
  ## concordance with the keywords we write in the XML
  write.flowSet(set, outdir=outdir, what="integer")
  set <- read.flowSet(path=outdir, phenoData="annotation.txt")
  ## Create the sample list from a flowSet and the gating structure object
  slist <- xmlSampleList(lapply(1:length(set), createSample, set, gates, 
    transforms))
  wp <- xmlWindowPosition()
  traits <- xmlTextTraits()
  cols <- xmlColumns()
  grps <- xmlGroups(set)
  te <- xmlTableEditor(filename)
  le <- xmlLayoutEditor(filename)
  ce <- xmlCompensationEditor(filename) 
  set <- xmlDivaSettings()
  ws <- xmlWorkspace(list(wp, traits, cols, grps, slist, te, le, ce, set), 
    file.path(gsub("/", "\\", getwd(), fixed=TRUE), outdir, filename, fsep="\\"))
  ## Write out to an XML file
  saveXML(ws, file=file.path(outdir, filename), 
    prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", 
    localeToCharset()))
}


                                                

setAs(from="workFlow", to="list", def=function(from)
{
  wt <- tree(from)
  n <- nodes(wt)
  tv <- grep("transView", n)
  gv <- grep("gateView", n)
  rv <- setdiff(seq_along(n), gv)
  ##if(length(tv) > 1)
  ##  stop("Multiple transformation operations are not supported by flowJo.")
  ##if(length(gv) && length(rv) && !all(rv < min(gv)))
  ##  stop("Only further gate nodes are allowed as children of a gate node.")
  start <- n[max(1, min(gv)-1)]
  parent <- n[max(1, min(gv)-2)]
  gates <- gres <- transforms <- list()
  buildList <- function(tree, node, wf, first=TRUE, parent)
  {
    clist <- list()
    if(length(node))
    {
      children <- as.vector(unlist(adj(tree, node)))
      gchildren <- processed <- NULL
      ids <- sapply(children, guid)
      for(i in children)
      {
        v <- wf[[i]]
        if(is(v, "gateView"))
        {
          fr <- action(v)@filterResult
          if(is(get(fr)[[1]], "logicalFilterResult") && !identifier(fr) %in% processed)
          {
            gchildren <- c(gchildren, i)
            processed <- c(processed, identifier(fr))
            gates[[ids[i]]] <<- gate(action(v))
            gres[[ids[i]]] <<- get(fr)
            transforms[[ids[i]]] <<- if(first) identTransform(Data(wf[[node]]))
              else if(is(wf[[node]], "transformView")) 
                estimateBackTransform(Data(wf[[parent]]), 
                  get(action(wf[[node]])@transform))
              else if(is(wf[[node]], "normalizeView"))
                estimateBackNorm(Data(wf[[parent]]), 
                  attr(Data(wf[[node]]), "warping")) 
              else identTransform(Data(wf[[node]]))
          }
        }
        else
        {
           gchildren <- c(gchildren, i)
        }
      }
      for(i in gchildren)  
          clist[[ids[i]]] <- buildList(tree, i, wf, FALSE, parent=node)
    }
    return(clist)
  }
  if(is.na(start))
    return(list(gates=NULL, results=NULL, tree=list()))
  tree <- buildList(wt, start, from, parent=parent) 
  return(list(gates=gates, results=gres, transforms=transforms, 
    tree=tree))
})



createGlist <- function(wf, backTrans)
{
  glistSet <- as(wf, "list")
  sn <- sampleNames(Data(wf[["base view"]]))
  glist <- vector(mode="list", length=length(sn))
  names(glist) <- sn
  for(i in sn)
  {
    gtmp <- glistSet$gates
    ## gates <- lapply(gtmp, function(x) if(is(x, "filter")) as(x, "polygonGate") 
    ## else as(x[[i]], "polygonGate"))
    gates <- lapply(gtmp, function(x) if(is(x, "filter")) x else x[[i]])
    res <- lapply(glistSet$results, function(x) x[[i]])
    trans <- lapply(names(gtmp), collapseTransforms, fid=i, 
      transforms=glistSet$transforms, backTrans=backTrans)
    names(trans) <- names(gtmp) 
    glist[[i]] <- list(gates=gates, results=res, tree=glistSet$tree,
      transforms=trans)
  }
  return(glist)
}   

wfToFlowJo <- function(wf, outdir="flowJo", 
  filename="workspace.wsp", backTrans=NULL)
{
  wt <- tree(wf)
  n <- nodes(wt)
  tv <- grep("transView", n)
  nv <- grep("normView", n)
  relBaseView <- min(if(length(tv)) min(tv) else Inf,
                     if(length(nv)) min(nv) else Inf)
  set <- if(!is.infinite(relBaseView)) Data(wf[[n[relBaseView]]]) else 
    Data(wf[["base view"]])
  pars <- colnames(set[[1]])
  if(is.null(backTrans))
  {
    backTrans <- makeLinear(set[[1]], listOnly=TRUE)
    set <- fsApply(set, makeLinear)
  }
  else
  {
    set <- transform(set, backTrans)
  }
  gates <- createGlist(wf, backTrans=backTrans)
  createWorkspace(set, outdir=outdir, filename=filename, gates=gates,
    transforms=NULL)
}


translateGate <- function(gate, transformation, gres)
{
  if(!is(gate, "parameterFilter") && !is(gate, "subsetFilter"))
    stop("We only know how to represent object inheriting from 'parameterFilter'",
      " in FlowJo.")
  type <- class(gate)
  pars <- parameters(gate)   
  if(!all(pars %in% names(transformation)))
    stop("Transformation missing for gating parameter.")
  switch(type,
    ## FIXME: This is cheating
    "subsetFilter"={
      ##xmlEllipsoidGateNode(flowViz:::norm2Ell(filterDetails(gres)[[2]],
      ##parms=parameters(gate)), 
      ##transformation)
      xmlPolygonGateNode(flowViz:::norm2Polygon(filterDetails(gres)[[2]],
      parms=parameters(gate)), 
      transformation)
    },
    "polygonGate"={
      xmlPolygonGateNode(gate, transformation)
    },
    "rectangleGate"={
      xmlRectangleGateNode(gate, transformation)
    },
    "ellipsoidGate"={
      ##xmlEllipsoidGateNode(gate, transformation)
      xmlPolygonGateNode(flowViz:::ell2Polygon(filterDetails(gres)[[1]],
      parms=parameters(gate)), 
      transformation)
    },
    "norm2Filter"={
      ##xmlEllipsoidGateNode(flowViz:::norm2Ell(filterDetails(gres)[[1]],
      ##  parms=parameters(gate)), 
      ##  transformation)
      xmlPolygonGateNode(flowViz:::norm2Polygon(filterDetails(gres)[[1]],
      parms=parameters(gate)), 
      transformation)
    },
    "quadGate"={
    },
    stop("Unsupported gate type.")
    )
}   


## The default attributes for all types of XML nodes needed for a FlowJo 
## workspace. NULL values are ignored. These are stored in inst/defaults.xml
## and new tags have to be added there.
fjSettings <- function() .fuEnv$fjDefaults

  
 
makeLinear <- function(x, range=1023, listOnly=FALSE)
{
  parms <- pData(parameters(x))
  rownames(parms) <- parms$name
  isExp <- sapply(keyword(x, sprintf("$P%dE", 1:ncol(x))), 
    function(y) length(y) && y!="0,0")
  tl <- lapply(parms[isExp, "name"], function(y) 
    new("transformMap", output=y, input=y, 
    f=function(zp){fun <- function(z) 
    (z - parms[y, "minRange"])/diff(unlist(parms[y, c("minRange", 
    "maxRange")]))*range;fun(zp)}))
    names(tl) <- parms[isExp, "name"]
  tlist <- new("transformList", transforms=tl)
  if(listOnly)
    return(tlist)
  x <- transform(x, tlist)
  repl <- list()
  repl[names(which(isExp))] <- "0,0"
  keyword(x) <- repl
  return(x)
}


estimateBackTransform <- function(x, tf, n=1000)
{
  dexpr <- apply(range(x[[1]]), 2, function(r) seq(r[1], r[2], len=n))
  dummy <- flowFrame(dexpr)
  dummyt <- transform(dummy, tf)
  apf <- vector(mode="list", length(colnames(dummy)))
  names(apf) <- colnames(dummy)
  for(p in colnames(dummy))
    apf[[p]] <- if(p %in% names(tf@transforms)) 
      approxfun(exprs(dummyt[,p]), exprs(dummy[,p])) else function(x) x
  res <- lapply(sampleNames(x), function(y) apf)
  names(res) <- sampleNames(x)
  return(res)
}

estimateBackNorm <- function(x, norm, n=1000)
{
  enFun <- function(fid, norm)
  {
    apf <- vector(mode="list", length(colnames(x)))
    names(apf) <- colnames(x)
    for(p in colnames(x))
      apf[[p]] <- if(length(norm[[p]])) 
        norm[[p]][["revWarpFuns"]][[fid]] else function(x) x 
    return(apf) 
  } 
  res <- lapply(sampleNames(x), enFun, norm)
  names(res) <- sampleNames(x)
  return(res)
}

identTransform <- function(x)
{
  res <- lapply(sampleNames(x), function(y){
    resY <- lapply(colnames(x[[y]]), function(z) function(zr) zr)
    names(resY) <- colnames(x[[y]])
    resY})
  names(res) <- sampleNames(x)
  return(res)
}


collapseTransforms <- function(gid, fid, transforms, backTrans=NULL)
{
  wh <- which(names(transforms) == gid)
  ttf <- transforms[wh:1]
  parms <- names(ttf[[1]][[fid]])
  funs <- sapply(parms, function(p) ttf[[1]][[fid]][[p]])
  if(length(ttf) > 1)
    for(t in 2:length(ttf))
      for(p in parms)
      {
        makeFun <- function()
        {
          f1 <- funs[[p]]
          f2 <- ttf[[t]][[fid]][[p]]
          function(x) f1(f2(x))
        }
        funs[[p]] <- makeFun()
      }
  if(!is.null(backTrans))
  {
    ltf <- backTrans@transforms
    for(p in parms)
      if(p %in% names(ltf))
      {
        makeFun <- function()
        {
          f1 <- funs[[p]]
          f2 <- ltf[[p]]@f
        function(x) f2(f1(x))
        }
        funs[[p]] <- makeFun()
      }
  }
  return(funs)
}