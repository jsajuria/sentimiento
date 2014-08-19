#' classify_polarity
#'
#' Classifies the polarity (e.g. positive or negative) of a set of texts using a naive Bayes classifier trained on Janyce Wiebe's \code{\link{lexicon}} lexicon. The words have been manually translated into Spanish and checked to make sure the polarity is right.
#' @param textColumns A \code{data.frame} of text documents listed one per row.
#' @param algorithm A \code{string} indicating whether to use the naive \code{bayes} algorithm or a simple \code{voter} algorithm.
#' @param pstrong A \code{numeric} specifying the probability that a strongly subjective term appears in the given text.
#' @param pweak A \code{numeric} specifying the probability that a weakly subjective term appears in the given text.
#' @param prior A \code{numeric} specifying the prior probability to use for the naive Bayes classifier.
#' @param verbose A \code{logical} specifying whether to print detailed output regarding the classification process.
#' @param \ldots Additional parameters to be passed into the \code{\link{create_matrix}} function.
#' @keywords sentiment
#' @export
#' @examples
#' # LOAD LIBRARY
#' library(sentimiento)
#' # DEFINE DOCUMENTS
#' documents <- c("I am very happy, excited, and optimistic.",
#'               "I am very scared, annoyed, and irritated.",
#'               "Iraq's political crisis entered its second week one step closer to the potential 
#'				dissolution of the government, with a call for elections by a vital coalition partner 
#'				and a suicide attack that extended the spate of violence that has followed the withdrawal 
#'				of U.S. troops.",
#'               "With nightfall approaching, Los Angeles authorities are urging residents to keep their
#'				outdoor lights on as police and fire officials try to catch the person or people responsible 
#'				for nearly 40 arson fires in the last three days.")
#'
#' # CLASSIFY POLARITY
#' classify_polarity(documents,algorithm="bayes",verbose=TRUE)
#' @author Javier Sajuria \email{j.sajuria@@ucl.ac.uk}

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
	matrix <- create_matrix(textColumns,...)
	lexicon <- read.csv(system.file("data/lexicon.csv.gz",package="sentimiento"),header=FALSE)

	counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
	documents <- c()

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(positive=0,negative=0)
		doc <- matrix[i,]
		words <- findFreqTerms(doc,lowfreq=1)
		
		for (word in words) {
			index <- pmatch(word,lexicon[,1],nomatch=0)
			if (index > 0) {
				entry <- lexicon[index,]
				
				polarity <- as.character(entry[[2]])
				category <- as.character(entry[[3]])
				count <- counts[[category]]
	
				score <- pweak
                if (polarity == "strongsubj") score <- pstrong
				if (algorithm=="bayes") score <- abs(log(score*prior/count))
		
				if (verbose) {
                    print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
				}
				
				scores[[category]] <- scores[[category]]+score
			}		
		}
		
		if (algorithm=="bayes") {
			for (key in names(scores)) {
				count <- counts[[key]]
				total <- counts[["total"]]
				score <- abs(log(count/total))
				scores[[key]] <- scores[[key]]+score
			}
		} else {
			for (key in names(scores)) {
				scores[[key]] <- scores[[key]]+0.000001
			}
		}
		
        best_fit <- names(scores)[which.max(unlist(scores))]
        ratio <- as.integer(abs(scores$positive/scores$negative))
        if (ratio==1) best_fit <- "neutral"
		documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
		if (verbose) {
			print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
			cat("\n")
		}
	}
	
	colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
	return(documents)
}