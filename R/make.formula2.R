make.formula2 <-
function(lhs, vars.vec, rand.vec=NULL){
if(is.null(rand.vec)) {
	if (length(vars.vec) < 2)
	return(noquote(paste( '~',vars.vec)))

	rhs <- vars.vec[1]
	for (v in vars.vec[2:length(vars.vec)])
		rhs <- paste(rhs, '+', v)
	noquote(paste( '~', rhs))	
} else {
	if (length(vars.vec) < 2){
	rhs <- vars.vec[1]	
	for (r in rand.vec[1:length(rand.vec)])
			rhs <- paste(rhs, ' + (1|', r,')' ,sep="")
	return(noquote(paste( '~', rhs)))
	}

	rhs <- vars.vec[1]
	for (v in vars.vec[2:length(vars.vec)])
		rhs <- paste(rhs, '+', v)
	for (r in rand.vec[1:length(rand.vec)])
			rhs <- paste(rhs, ' + (1|', r,')' ,sep="")
	noquote(paste( '~', rhs))	
	}
}