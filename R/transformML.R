## read.transformML("/home/nlemeur/proj/bioc-madman/Rpacks/flowUtils/inst/xml/linearTramsform.xml")

returnTransforms = function(g,refs) {
    print(g)
    ##If we can add it, return a reference to pick up later
	if(g@transformationId != "dummyTransform") {
		refs[[g@transformationId]] = g
		r = xmlNode("transformReference",attrs=list("ref"=g@transformationId), namespace="transforms")
		class(r) = c("transforms_transformsReference",class(r))
		r
	} else
		g	
}
  
transforms.transforms_linear= function(x,refs,...) {
    ##This can be written more compactly, but later...
    cn = xmlGetAttr(x, "parameter","")
    a =  xmlGetAttr(x, "a", 1, as.double)
    b =  xmlGetAttr(x, "b", 0, as.double)
   
    transform = c("a"=a,"b"=b)
    returnTransforms(linearTransform(transformationId="Linear", transform), refs)
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
			class(x) = c(cn,class(x))
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
        nomen=sapply(ret,xmlGetAttr,"newName")
        print(ret)

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
       	names(ret) = lapply(ret, slot, "")
	ret
    }










