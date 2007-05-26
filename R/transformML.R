w## read.transformML("../inst/xml/linearTramsform.xml")

returnTransforms = function(g,refs) {
    
   tranformationId = g@transformationId

    ##If we can add it, return a reference to pick up later
	if(tranformationId != "dummyTransform") {
		refs[[tranformationId]] = g
		r = xmlNode("transformReference",attrs=list("ref"=tranformationId), namespace="transforms")
                class(r) = c("transforms_transformsReference",class(r))
		r
	} else
		g
       
}

##Linear transformation
transforms.transforms_linear= function(x,refs,...) {
  print("linear")
  cn = xmlGetAttr(x, "parameter","")
  a =  xmlGetAttr(x, "a", 1, as.double)
  b =  xmlGetAttr(x, "b", 0, as.double)
 
  transform = c("a"=a,"b"=b)
  
  returnTransforms(linearTransform(transformationId="dummyTransform", transform), refs)
}

##quadratic transformation
transforms.transforms_quadratic= function(x,refs,...) {
  print("quadratic")
  
  cn = xmlGetAttr(x, "parameter","")
  a =  xmlGetAttr(x, "a", 1, as.double)
  b =  xmlGetAttr(x, "b", 1, as.double)
  c =  xmlGetAttr(x, "c", 0, as.double)

  transform = c("a"=a,"b"=b, "c"=c)
  
  returnTransforms(quadraticTransform(transformationId="dummyTransform", transform), refs)
}


##Ln transformation
transforms.transforms_ln= function(x,refs,...) {
  print("ln")
  cn = xmlGetAttr(x, "parameter","")
  r =  xmlGetAttr(x, "r", 1, as.double)
  d =  xmlGetAttr(x, "d", 1, as.double)
 
  transform = c("r"=r,"d"=d)
  
  returnTransforms(lnTransform(transformationId="dummyTransform", transform), refs)
}

##log transformation
transforms.transforms_log= function(x,refs,...) {
  print("log")
  cn = xmlGetAttr(x, "parameter","")
  logbase =  xmlGetAttr(x, "logbase", 10, as.double)
  r =  xmlGetAttr(x, "r", 1, as.double)
  d =  xmlGetAttr(x, "d", 1, as.double)
 
  transform = c("logbase"=logbase, "r"=r,"d"=d)
  
  returnTransforms(logTransform(transformationId="dummyTransform", transform), refs)
}

##logicle transformation
transforms.transforms_logicle= function(x,refs,...) {
 print("logicle")
  ##cn = xmlGetAttr(x, "parameter","")
  ##b =  xmlGetAttr(x, "b", 1, as.double)
  ##w =  xmlGetAttr(x, "w", 0, as.double)
  ##r =  xmlGetAttr(x, "r", 262144, as.double)
  ##tol =  xmlGetAttr(x, "tol", .Machine$double.eps^0.25, as.double)
  ##maxit= xmlGetAttr(x, "maxit", 5000, as.integer)
  res = "not implemented"
  res
 ## transform = c("b"= b, "w"= w, "r"=r, "tol"= tol, maxit="maxit")
 ## returnTransforms(logicleTransform(transformationId="dummyTransform", transform), refs)
}


##Bi-exponential transformation
transforms.transforms_bi.exponential= function(x,refs,...) {
 print("bi")
  cn = xmlGetAttr(x, "parameter","")
  a =  xmlGetAttr(x, "a", .5, as.double)
  b =  xmlGetAttr(x, "b", 1,  as.double)
  c =  xmlGetAttr(x, "c", .5, as.double)
  d =  xmlGetAttr(x, "d", 1,  as.double)
  f =  xmlGetAttr(x, "f", 0,  as.double)
  w =  xmlGetAttr(x, "w", 0,  as.double)
  tol =  xmlGetAttr(x, "tol", .Machine$double.eps^0.25, as.double)
  maxit= xmlGetAttr(x, "maxit", 5000, as.integer)
  
  transform = c("a"= a, "b"= b,"c"= c,"d"= d,"f"= f,"w"= w, "tol"= tol, maxit="maxit")
  
  returnTransforms(biexponentialTransform(transformationId="dummyTransform", transform), refs)
}

transforms.transforms_hyperlog= function(x,refs,...) {
 print("hyper")
  res = "not implemented"
  res
}


transforms.transforms_split.scale= function(x,refs,...) {
 print("split")
  res = "not implemented"
  res
}

transforms.XMLNode = function(x,...) paste(xmlName(x), xmlGetAttr(x, "id"), sep=":")
transforms.default = function(x,...) x
transforms = function(x,...) UseMethod("transforms")

transforms.transformsReference = function(x,refs,...) {
	y = refs[[xmlGetAttr(x,"ref")]]
	if(is.null(y))
		stop(paste("Unable to resolve reference \"",xmlGetAttr(x,"ref"),"\"",sep=""))
	y
}

read.transformML = function(file) {
	#Read in the XML file and mark it up 
	#so that we can use dispatch	
	x = xmlTreeParse(file,
		handlers=list(
                  "comment"=function(x,...) NULL,
                  "startElement"=function(x) {
                    cn = paste(make.names(c(xmlNamespace(x),xmlName(x))),collapse="_")
                    tn = make.names(xmlName(x))
                    class(x) = c(cn,tn, class(x))
                    x
                  }
                  
	),asTree=TRUE)
        
        ##A place to stash our transformation ids
	transform_list = new.env(hash=TRUE)
	end = FALSE
        last_len = length(ls(env=transform_list))
       	if(!is(xmlRoot(x),"transforms_Transformation.ML"))
		stop("Not a transformML Document")
	ret = xmlChildren(xmlRoot(x))
        nomen = sapply(ret,xmlGetAttr,"newName")
        
        def = sapply(ret, xmlChildren)
        noDef = which(names(def)!= "transformation.pre-defined")
        if(length(noDef) > 0)
          stop("Do not know what to do yet")
        ans <-sapply(def, xmlChildren)
        names(ans) <- nomen 
        
        attempts = 0
        
        while(!end) {

            ret = lapply(ans, transforms, transform_list)
            new_len = length(ls(env= transform_list))
            if(new_len != last_len) attempts = 0
            if(all(sapply(ret,is,"transforms")) || attempts >= 5) end = TRUE
            last_len = new_len
            attempts = attempts + 1
	}

        for(i in 1:length(ret)){
          ret[[i]]@transformationId = nomen[i]}

        names(ret) = nomen 
	ret
    }










