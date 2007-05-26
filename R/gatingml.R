##
## Parsing Gating-ML into a structured filter.

xmlGrep = function(x,type) UseMethod("xmlGrep")
xmlGrep.XMLNode = function(x,type) {
	children = xmlChildren(x)
	children[sapply(children,"is",type)]
}

#A gate does not need to appear in the tree BEFORE being referenced
#so when we encounter a gate we process it and return a gate reference
#to resolve on another pass. We keep trying until we make it through the
#tree without errors or we don't add another gate to the hash of gates. This
#should catch circular references. In an ideal world I suppose I would set things
#up as a topological sort, but I'm not willing to take the time right now.

returnGate = function(g,refs) {
	#If we can add it, return a reference to pick up later
	if(g@filterId != "dummyGate") {
		refs[[g@filterId]] = g
		r = xmlNode("gateReference",attrs=list("ref"=g@filterId),namespace="gating")
		class(r) = c("gating_gateReference",class(r))
		r
	} else
		g	
}

gate.gating_RectangleGate = function(x,refs,...) {
	#This can be written more compactly, but later...
	cn       = sapply(xmlGrep(x,"gating_dimension"),xmlGetAttr,"parameter","")
	minvalues= sapply(xmlGrep(x,"gating_dimension"),xmlGetAttr,"min",-Inf,as.numeric)
	maxvalues= sapply(xmlGrep(x,"gating_dimension"),xmlGetAttr,"max",Inf,as.numeric)
	gate = rbind("min"=minvalues,"max"=maxvalues)
	colnames(gate) = cn
	returnGate(rectangleGate(xmlGetAttr(x,"id","dummyGate"),gate),refs)
}
gate.gating_EllipsoidGate = function(x,refs,...) {
	#Okay, this gate is rough. First we need some dimensions. I assume that the dimension order
	#corresponds to the coordinate order, which seems odd but whatever.
	dim_names = sapply(xmlGrep(x,"gating_dimension"),xmlGetAttr,"parameter",NULL)
	foci      = t(sapply(xmlGrep(x,"gating_focus"),function(focus) {
		structure(sapply(xmlGrep(focus,"gating_coordinate"),xmlGetAttr,"value",NA,as.numeric),
			names=dim_names)
	}))
	
	dist = as.numeric(xmlValue(xmlGrep(x,"gating_distance")[[1]]))
	returnGate(ellipsoidGate(xmlGetAttr(x,"id","dummyGate"),foci,dist),refs)
}

gate.gating_PolytopeGate = function(x,refs,...) {
	#Okay, this gate is rough. First we need some dimensions. I assume that the dimension order
	#corresponds to the coordinate order, which seems odd but whatever.
	dim_names = sapply(xmlGrep(x,"gating_dimension"),xmlGetAttr,"parameter",NULL)
	points      = t(sapply(xmlGrep(x,"gating_point"),function(focus) {
		structure(sapply(xmlGrep(focus,"gating_coordinate"),xmlGetAttr,"value",NA,as.numeric),
			names=dim_names)
	}))
	#Special case, polygonGate in 2D. I don't think polytope gates exist for higher dimension
	#right now.
	if(length(dim_names) == 2) 
		returnGate(polygonGate(xmlGetAttr(x,"id","dummyGate"),points),refs)
	else
		returnGate(polytopeGate(xmlGetAttr(x,"id","dummyGate"),points),refs)
}

gate.gating_PolygonGate = function(x,refs,...) {
	#Okay, this gate is rough. First we need some dimensions. I assume that the dimension order
	#corresponds to the coordinate order, which seems odd but whatever.
	dim_names = sapply(xmlGrep(x,"gating_dimension"),xmlGetAttr,"parameter",NULL)
	points      = t(sapply(xmlGrep(x,"gating_vertex"),function(focus) {
		structure(sapply(xmlGrep(focus,"gating_coordinate"),xmlGetAttr,"value",NA,as.numeric),
			names=dim_names)
	}))
	#Special case, polygonGate in 2D. I don't think polytope gates exist for higher dimension
	#right now.
	if(length(dim_names) == 2) 
		returnGate(polygonGate(xmlGetAttr(x,"id","dummyGate"),points),refs)
	else
		returnGate(polytopeGate(xmlGetAttr(x,"id","dummyGate"),points),refs)
}


#When we see a reference we can actually just resolve to the gate
gate.gating_gateReference = function(x,refs,...) {
	y = refs[[xmlGetAttr(x,"ref")]]
	if(is.null(y))
		stop(paste("Unable to resolve reference \"",xmlGetAttr(x,"ref"),"\"",sep=""))
	y
}

gate.gating_BooleanGate = function(x,refs,...) {
	r = try({
		y = gate(x$children[[1]],refs,...)
		if(is(y,"filter")) {
			y@filterId = xmlGetAttr(x,"id")
			refs[[y@filterId]] = y
			y
		} else NULL
	},silent=TRUE)
	#If we fail to complete construction of the gate
	#we just return ourselves to try again.
	if(is(r,"try-error")) x else returnGate(r,refs)
}

gate.gating_not = function(x,...) {
	ch = xmlChildren(x)
	e1 = gate(ch[[1]],...) 
	#If we get a gate reference back, guess what? The gate is 
	#actually available so simply try to re-resolve it.
	if(is(e1,"gating_gateReference")) e1=gate(e1,...)
	if(is(e1,"filter"))
		!e1
	else 
		stop(capture.output(print(x)))
	
}
gate.gating_and = function(x,...) {
	ch = xmlChildren(x)
	e1 = gate(ch[[1]],...) 
	e2 = gate(ch[[2]],...)
	
	if(is(e1,"gating_gateReference")) e1=gate(e1,...)
	if(is(e2,"gating_gateReference")) e2=gate(e2,...)
	
	if(!is(e1,"filter"))
		stop(paste("Lefthand side is not a filter:",capture.output(print(e1)),sep=" ",collapse="\n"))
	if(!is(e2,"filter"))
		stop(paste("Righthand side is not a filter:",capture.output(print(e2)),sep=" ",collapse="\n"))
	e1 & e2
}

gate.gating_or = function(x,...) {
	ch = xmlChildren(x)
	e1 = gate(ch[[1]],...)
	e2 = gate(ch[[2]],...)

	if(is(e1,"gating_gateReference")) e1=gate(e1,...)
	if(is(e2,"gating_gateReference")) e2=gate(e2,...)
	
	
	if(!is(e1,"filter"))
		stop(paste("Lefthand side is not a filter:",capture.output(print(e1)),sep=" ",collapse="\n"))
	if(!is(e2,"filter"))
		stop(paste("Righthand side is not a filter:",capture.output(print(e2)),sep=" ",collapse="\n"))
	e1 | e2
}


gate.XMLNode = function(x,...) paste(xmlName(x),xmlGetAttr(x,"id"),sep=":")
gate.default = function(x,...) x
gate = function(x,...) UseMethod("gate")

read.gatingML = function(file) {
	#Read in the XML file and mark it up 
	#so that we can use dispatch	
	x = xmlTreeParse(file,
		handlers=list(
		"comment"=function(x,...) NULL,
		"startElement"=function(x) {
			cn = paste(make.names(c(xmlNamespace(x),xmlName(x))),collapse="_")
			class(x) = c(cn,class(x))
			x
		}
	),asTree=TRUE)
	
	#A place to stash our gating ids
	gate_list = new.env(hash=TRUE)
	end = FALSE
	last_len = length(ls(env=gate_list))
        if(!is(xmlRoot(x),"gating_Gating.ML"))
		stop("Not a GatingML Document")
	ret = xmlChildren(xmlRoot(x))
	print(str(ret))
       
        attempts = 0
	while(!end) {
		ret = lapply(ret,gate,gate_list)
		new_len = length(ls(env=gate_list))
		if(new_len != last_len) attempts = 0
		if(all(sapply(ret,is,"filter")) || attempts >= 5) end = TRUE
		last_len = new_len
		attempts = attempts + 1
	}
	names(ret) = lapply(ret,slot,"filterId")
	ret
}
