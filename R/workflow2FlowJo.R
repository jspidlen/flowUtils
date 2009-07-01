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
    if(!tag %in% names(defs))
      stop("'", tag, "' is not a valid XML tag in this context.")
    res <- defs[[tag]]
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
xmlSampleNode <- function(frame, id, gates=NULL, transforms=NULL)
{
  spops <- if(length(gates$tree)) xmlSubpopulations(gates, transforms) else
    NULL
  g <- xmlGraph(frame[,1:2])
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
xmlSubpopulations <- function(gates, transforms)
{
  pop <- c("<wrapper xmlns:gating=\"dummy\" xmlns:data-type=\"dummy\">",
    xmlSubpopulationsHelper(gates=gates$gates, gresults=gates$result, 
      glist=gates$tree, transforms=transforms), 
    "</wrapper>")
  xmlTreeParse(pop, asText=TRUE, 
    addAttributeNamespaces=TRUE)$doc$children[[1]][[1]]
}

xmlSubpopulationsHelper <- function(gates, gresults, glist, current, pops=NULL,
  transforms)
{
  children <- if(missing(current)) names(glist) else names(glist[[current]])
  if(!is.null(children))
    pops <- append(pops, "<Subpopulations>") 
  for(i in children)
  {
    pops <- c(pops, sprintf(paste("<Population name=\"%s\" annotation=\"\"", 
      "owningGroup=\"\" expanded=\"1\" sortPriority=\"10\" count=\"%s\">"),
      identifier(gates[[i]]), summary(gresults[[i]])$true), 
      toString.XMLNode(xmlGateNode(gates[[i]], i, transforms)))
    pops <- xmlSubpopulationsHelper(gates, gresults, glist, i, pops, transforms)
    pops <- append(pops, "</Population>")
  }
  if(!is.null(children))
  pops <- append(pops, "</Subpopulations>")
  return(pops)
}


## The Gate XML node. This holds the geometric definiton of a gate. The 
## translateGate function makes sure that we created the appropriate 
## representation for the respective gate types.
xmlGateNode <- function(gate, id, transforms)
{
  xmlTag("Gate", attrs=c("gating:id"=id), children=translateGate(gate,
    transforms))
} 


## Create a unique integer identifier for a gate.
guid <- function(...) substr(as.character(as.vector(as.integer(Sys.time())/
  runif(1) * proc.time()["elapsed"])), 0, 8)
                    

## The PolygonGate XML node. This represents dimensions and vertices for a 
## single polygon gate.
xmlPolygonGateNode <- function(gate)
{
  dims <- lapply(parameters(gate), xmlDimensionNode)
  verts <- apply(gate@boundaries, 1, xmlVertexNode)
  xmlTag("PolygonGate", namespace="gating", children=c(dims, verts))
}



xmlEllypsoidGateNode <- function()
{
  #   <gating:EllipsoidGate gating:distance="89.6517268546188" eventsInside="1" annoOffsetX="0" annoOffsetY="0" tint="#000000" isTinted="0" lineWeight="Hairline" isFJGate="1"  >
#                <gating:dimension  >
#                <data-type:parameter data-type:name="FSC-H"  />
#                </gating:dimension >
#                <gating:dimension  >
#                <data-type:parameter data-type:name="SSC-H"  />
#                </gating:dimension >
#                <gating:foci  >
#                <gating:vertex  >
#                <gating:coordinate data-type:value="71.97366325550765"  />
#                <gating:coordinate data-type:value="163.04098515178657"  />
#                </gating:vertex >
#                <gating:vertex  >
#                <gating:coordinate data-type:value="141.02633674449237"  />
#                <gating:coordinate data-type:value="121.95901484821343"  />
#                </gating:vertex >
#                </gating:foci >
#                <gating:edge  >
#                <gating:vertex  >
#                <gating:coordinate data-type:value="67.0"  />
#                <gating:coordinate data-type:value="166.0"  />
#                </gating:vertex >
#                <gating:vertex  >
#                <gating:coordinate data-type:value="146.0"  />
#                <gating:coordinate data-type:value="119.0"  />
#                </gating:vertex >
#                <gating:vertex  >
#                <gating:coordinate data-type:value="94.0"  />
#                <gating:coordinate data-type:value="124.0"  />
#                </gating:vertex >
#                <gating:vertex  >
#                <gating:coordinate data-type:value="119.0"  />
#                <gating:coordinate data-type:value="161.0"  />
#                </gating:vertex >
#                </gating:edge >
#                </gating:EllipsoidGate >
}


xmlRectangleGateNode <- function(gate)
{
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
  write.flowSet(set, outdir=outdir, what="numeric")
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
  if(length(tv) > 1)
    stop("Multiple transformation operations are not supported by flowJo.")
  if(length(gv) && length(rv) && !all(rv < min(gv)))
    stop("Only further gate nodes are allowed as children of a gate node.")
  start <- n[max(1, min(gv)-1)]
  gates <- gres <- list()
  buildList <- function(tree, node, wf)
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
        fr <- action(v)@filterResult
        if(is(get(fr)[[1]], "logicalFilterResult") && !identifier(fr) %in% processed)
        {
          gchildren <- c(gchildren, i)
          processed <- c(processed, identifier(fr))
          gates[[ids[i]]] <<- gate(action(v))
          gres[[ids[i]]] <<- get(fr)
        }
      }
      for(i in gchildren)      
        clist[[ids[i]]] <- buildList(tree, i, wf)
    }
    return(clist)
  }
  if(is.na(start))
    return(list(gates=NULL, results=NULL, tree=list()))
  tree <- buildList(wt, start, from) 
  return(list(gates=gates, results=gres, tree=tree))
})



createGlist <- function(wf)
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
    glist[[i]] <- list(gates=gates, results=res, tree=glistSet$tree)
  }
  return(glist)
}   


wfToFlowJo <- function(wf, transforms, outdir="flowJo", 
  filename="workspace.wsp")
{
  wt <- tree(wf)
  n <- nodes(wt)
  tv <- grep("transView", n)
  ## set <- if(length(tv)) Data(wf[[n[min(tv)]]]) else Data(wf[["base view"]])
  set <-  Data(wf[["base view"]])
  pars <- colnames(set)
  miss <- setdiff(pars, c(names(transforms@transforms), 
    flowCore:::findTimeChannel(set)))
  for(m in miss)
    transforms@transforms[[m]] <- new("transformMap", output=m, input=m,
      f=function(x) x)
  gates <- createGlist(wf)
  createWorkspace(set, outdir=outdir, filename=filename, gates=gates,
    transforms=transforms)
}


translateGate <- function(gate, transformation)
{
  if(!is(gate, "parameterFilter"))
    stop("We only know how to represent object inheriting from 'parameterFilter'",
      " in FlowJo.")
  type <- class(gate)
  pars <- parameters(gate)   
  if(!all(pars %in% names(transformation@transforms)))
    stop("Transformation missing for gating parameter.")
  switch(type,
    "polygonGate"={
      for(p in pars)
        gate@boundaries[,p] <- transformation@transforms[[p]]@f(gate@boundaries[,p])
      xmlPolygonGateNode(gate)
    },
    "rectangleGate"={
      gate@min <- sapply(pars, function(x) 
        as.vector(transformation@transforms[[x]]@f(gate@min[x])))
      gate@max <- sapply(pars, function(x) 
        as.vector(transformation@transforms[[x]]@f(gate@max[x])))
      xmlRectangleGateNode(gate)
    },
    "ellipsoidGate"={
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

  
 
