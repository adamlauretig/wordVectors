##' Dump context too
##'
##' Given words, run w2v, keep context
##'
##'
##'
##' @title Train a model by word2vec.
##' @param train_file Path of a single .txt file for training. Tokens are split on spaces.
##' @param output_file Path of the output file.
##' @param vectors The number of vectors to output. Defaults to 100.
##' More vectors usually means more precision, but also more random error, higher memory usage, and slower operations.
##' Sensible choices are probably in the range 100-500.
##' @param threads Number of threads to run training process on.
##' Defaults to 1; up to the number of (virtual) cores on your machine may speed things up.
##' @param window The size of the window (in words) to use in training.
##' @param classes Number of classes for k-means clustering. Not documented/tested.
##' @param cbow If 1, use a continuous-bag-of-words model instead of skip-grams.
##' Defaults to false (recommended for newcomers).
##' @param min_count Minimum times a word must appear to be included in the samples.
##' High values help reduce model size.
##' @param iter Number of passes to make over the corpus in training.
##' @param force Whether to overwrite existing model files.
##' @param negative_samples Number of negative samples to take in skip-gram training. 0 means full sampling, while lower numbers
##' give faster training. For large corpora 2-5 may work; for smaller corpora, 5-15 is reasonable.
##' @return A VectorSpaceModel object.
##' @author Jian Li <\email{rweibo@@sina.com}>, Ben Schmidt <\email{bmchmidt@@gmail.com}>
##' @references \url{https://code.google.com/p/word2vec/}
##' @export
##'
##' @useDynLib wordVectors
##'
##' @examples \dontrun{
##' model = train_word2vec(system.file("examples", "rfaq.txt", package = "wordVectors"))
##' }

dyn.load("~/data/wordVectors_dump/src/tmcn_word2vec.so")

train_word2vec_dumpcv <- function(train_file, output_file = "vectors.bin",vectors=100,threads=1,window=12,
  classes=0,cbow=0,min_count=5,iter=5,force=F, negative_samples=5, dumpcv = 1, dumpcv_file = "cv.bin")
{
  if (!file.exists(train_file)) stop("Can't find the training file!")
  if (file.exists(output_file) && !force) stop("The output file '",
                                     output_file ,
                                     "' already exists: give a new destination or run with 'force=TRUE'.")

  train_dir <- dirname(train_file)

  # cat HDA15/data/Dickens/* | perl -pe 'print "1\t"' | egrep "[a-z]" | bookworm tokenize token_stream > ~/test.txt

  if(missing(output_file)) {
    output_file <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
    output_file <- file.path(train_dir, output_file)
  }

  outfile_dir <- dirname(output_file)
  if (!file.exists(outfile_dir)) dir.create(outfile_dir, recursive = TRUE)

  train_file <- normalizePath(train_file, winslash = "/", mustWork = FALSE)
  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  dumpcv_file <- normalizePath(dumpcv_file, winslash = "/", mustWork = FALSE)
  # Whether to output binary, default is 1 means binary.
  binary = 0

  OUT <- .C("CWrapper_word2vec",
            train_file = as.character(train_file),
            output_file = as.character(output_file),
            dumpcv_file = as.character(dumpcv_file),
            binary = as.character(binary),
            dims=as.character(vectors),
            threads=as.character(threads),
            window=as.character(window),
            classes=as.character(classes),
            cbow=as.character(cbow),
            min_count=as.character(min_count),
            iter=as.character(iter),
            neg_samples=as.character(negative_samples),
            dumpcv=as.character(dumpcv)
            
  )
  if(binary == 1){
  vectors <- wordVectors::read.vectors(output_file)
  contexts <- wordVectors::read.vectors(dumpcv_file, vectors = 20)
  }else{
  v_size <- file.info(output_file)$size
  vectors_raw <-  readChar(output_file, v_size)
  words_char <- vectors_raw
  words_lines <- unlist(strsplit(words_char, "\n"))
  words_dim <- as.integer(unlist(strsplit(words_lines[1], " ")))
  vec_vals <- strsplit(words_lines[2:length(words_lines)], " ")
  vec_mat <-do.call(rbind, vec_vals)
  rownames(vec_mat) <- vec_mat[,1]
  vec_mat <- vec_mat[,2:(words_dim[2]+1)]
  vectors <- wordVectors::as.VectorSpaceModel(vec_mat)

  c_size <- file.info(dumpcv_file)$size
  contexts_raw <-  readChar(dumpcv_file, c_size)
  contexts_char <- contexts_raw
  contexts_lines <- unlist(strsplit(contexts_char, "\n"))
  contexts_dim <- as.integer(unlist(strsplit(contexts_lines[1], " ")))
  contexts_vals <- strsplit(contexts_lines[2:length(contexts_lines)], " ")
  
  lapply(contexts_vals, any, contexts_dim[2] + 1)
  <- sapply(contexts_vals, any, contexts_dim[2] + 1)
  contexts_vals <- contexts_vals[length(contexts_vals) == (contexts_dim[2] + 1)]
  contexts_mat <-do.call(rbind, contexts_vals)
  rownames(contexts_mat) <- contexts_mat[,1]
  contexts_mat <- contexts_mat[,2:(contexts_dim[2]+1)]
  contexts <- wordVectors::as.VectorSpaceModel(contexts_mat)
  }
  overlap <- rownames(vectors) %in% rownames(contexts)
  vectors <- vectors[overlap, ]
  list(vectors, contexts)
}

test <- train_word2vec_dumpcv(train_file = "~/cookbooks.txt",
  output_file = "~/cookbook_vectors.txt",vectors=20,
  threads=4,window=5,iter=5,negative_samples=5, dumpcv = 1, 
  dumpcv_file = "~/cv.txt", force = TRUE)

words_char <- test[[1]]
words_lines <- unlist(strsplit(words_char, "\n"))
words_dim <- as.integer(unlist(strsplit(words_lines[1], " ")))
vec_vals <- strsplit(words_lines[2:length(words_lines)], " ")
vec_mat <-do.call(rbind, vec_vals)
rownames(vec_mat) <- vec_mat[,1]
vec_mat <- vec_mat[,2:(words_dim[2]+1)]
vectors <- wordVectors::as.VectorSpaceModel(vec_mat)
contexts <- test[[2]]


t <- wordVectors::read.vectors( "~/cv.bin")
