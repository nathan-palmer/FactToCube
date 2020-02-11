/*
 * factToDoubleMatrix.c
 * 21.july, 2014
 * nathan palmer, harvard medical school
 *
 * build with, e.g., ~/Software/bin/R CMD SHLIB factToDoubleMatrix.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>

// arguments to stuffMatrixThreadStart
typedef struct {
    int* rowIndices;
    int* colIndices;
    double* values;
    int startOffset;
    int stopOffset;
    double* outMatrix;
    int outNRows;
    int outNColumns;
} stuffMatrixThreadArgs;

// entry point for each pThread.
void* stuffMatrixThreadStart(void* arg) {

	stuffMatrixThreadArgs* currThreadArgs = (stuffMatrixThreadArgs*) arg;

	int threadStartOffset = currThreadArgs->startOffset;
	int threadStopOffset = currThreadArgs->stopOffset;
	int currRowInd;
	int currColInd;
	long matrixOffset;

	int i=0;

	for (i=threadStartOffset; i<threadStopOffset; i++) {

		currRowInd = currThreadArgs->rowIndices[i];
		currColInd = currThreadArgs->colIndices[i];
		matrixOffset =
			(
				((long)currColInd) * ((long)(currThreadArgs->outNRows))
			)
			+ (long)currRowInd;
		currThreadArgs->outMatrix[matrixOffset] = currThreadArgs->values[i];
	}
}

// use nThreads to stuff the nEntries in values into the respective rows and columns of outMatrix
SEXP factToDoubleMatrix(
	SEXP rowIndices_sxp,  // vector of ints
	SEXP colIndices_sxp,  // vector of ints
	SEXP values_sxp,      // vector of doubles
	SEXP nEntries_sxp,    // int pointer
	SEXP outMatrix_sxp,   // double matrix
	SEXP outNRows_sxp,    // int pointer
	SEXP outNColumns_sxp, // int pointer
	SEXP nThreads_sxp     // int pointer
	) {

	int nprot=0;

	int* rowIndices = INTEGER(rowIndices_sxp);
	int* colIndices = INTEGER(colIndices_sxp);
	double* values = REAL(values_sxp);
	int nEntries = *INTEGER(nEntries_sxp);
	SEXP outMatrixReal = AS_NUMERIC(outMatrix_sxp);
	PROTECT(outMatrixReal); nprot++;
	double* outMatrix = REAL(outMatrixReal);
	int outNRows = *INTEGER(outNRows_sxp);
	int outNColumns = *INTEGER(outNColumns_sxp);
	int nThreads = *INTEGER(nThreads_sxp);

	//printf("number of entries: %d\n", nEntries);

	int nWorkingThreads=nThreads;
	if (nThreads > nEntries/2) {
		nWorkingThreads = nEntries/2;
	}
	//printf("number working threads: %d\n", nWorkingThreads);

	stuffMatrixThreadArgs* threadArgs = malloc(sizeof(stuffMatrixThreadArgs) * nWorkingThreads);
	pthread_attr_t* threadAttrs = malloc(sizeof(pthread_attr_t) * nWorkingThreads);
	pthread_t* threadIds = malloc(sizeof(pthread_t) * nWorkingThreads);

	int lastStop=0;
	int chunkSize=nEntries/nWorkingThreads;
	//printf("chunk size: %d\n", chunkSize);

	int currThread;
	for (currThread=0; currThread<nWorkingThreads; currThread++) {

		threadArgs[currThread].rowIndices = rowIndices;
		threadArgs[currThread].colIndices = colIndices;
		threadArgs[currThread].values = values;
		threadArgs[currThread].startOffset = lastStop;
		threadArgs[currThread].outMatrix = outMatrix;
		threadArgs[currThread].outNRows = outNRows;
		threadArgs[currThread].outNColumns = outNColumns;

		// make sure we handle the last one correctly
		if (currThread == (nWorkingThreads-1)) {
			threadArgs[currThread].stopOffset = nEntries;
		} else {
			threadArgs[currThread].stopOffset = lastStop+chunkSize;
		}
		// create a new thread
		pthread_attr_init(&(threadAttrs[currThread]));
	    pthread_attr_setdetachstate(&(threadAttrs[currThread]), PTHREAD_CREATE_JOINABLE);
	    pthread_create(&(threadIds[currThread]), &(threadAttrs[currThread]), stuffMatrixThreadStart, &(threadArgs[currThread]));

		lastStop += chunkSize;
	}

	// join the threads so that we don't return until they're all done
	for (currThread=0; currThread<nWorkingThreads; currThread++) {

		//printf("curr thread number %d\n", currThread);
		//printf("start %d\n", threadArgs[currThread].startOffset);
		//printf("stop %d\n", threadArgs[currThread].stopOffset);

		pthread_join(threadIds[currThread], NULL);
	}

	free(threadArgs);
	free(threadAttrs);
	free(threadIds);

	UNPROTECT(nprot);

	return R_NilValue;
}
