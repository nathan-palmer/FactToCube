#' A function to convert a table of triples into a 2-dimensional numeric matrix.
#'
#' This function allows you to e.g. convert a fact table from SQL into a matrix.
#' It uses multiple threads and compiled C code to be more efficient than native R
#' alternatives.
#' @param T A three column table containing the data to cast into a matrix -- the first column contains row indices, the second column contains column indices and the third column contains the values to be stored at each location in the output matrix.
#' This can either be a data frame with the first two columns of type character and the third of type numeric, or a numeric three-column matrix.
#' @param n.threads The number of threads to use.  You probably don't want to ask for more cores than are available to your R process.
#' Internally, the package limits the number of threads to floor((number of rows * number of cols)/2).
#' @keywords reshape
#' @export
#' @useDynLib FactToCube
#' @examples
#' factToNumericMatrix(data.frame(c("a","a","a","b","b","b","c","c","c"), rep(times=3, c("x", "y", "z")), as.numeric(1:9)), 1)

factToNumericMatrix <- function (T, n.threads=1) {

	row.names = sort(unique(T[,1]))
	col.names = sort(unique(T[,2]))

	row.offsets = match(T[,1], row.names)
	col.offsets = match(T[,2], col.names)
	values = T[,3]

	## set up memory -- this can take considerable time for very large output matrices
	M = matrix(0, ncol=length(col.names), nrow=length(row.names), dimnames=list(row.names, col.names))

	## pass the integer offsets, melted data, and destination matrix to C
	## for multithreaded cast'ing

	.Call(
		"factToDoubleMatrix",
		rowIndices=as.integer(row.offsets-1),
		colIndices=as.integer(col.offsets-1),
		values=values,
		nEntries=nrow(T),
		outMatrix=M,
		outNRows=as.integer(nrow(M)),
		outNColumns=as.integer(ncol(M)),
		nThreads=as.integer(n.threads),
		PACKAGE="FactToCube"
	)

	return (M)
}
