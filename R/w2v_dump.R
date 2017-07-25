# Word2vec which will also dump context
# 

# run in terminal R CMD SHLIB ~/data/wordVectors/src/tmcn_word2vec.c

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
  # editing the .h file w/Goldberg's mod to w2v, so that we get contexts
  # source: https://github.com/BIU-NLP/word2vecf/blob/master/word2vec.c
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
  vectors <- wordVectors::read.vectors(output_file, binary = TRUE)
  contexts <- wordVectors::read.vectors(dumpcv_file, binary = TRUE)
  }else{
  v_size <- file.info(output_file)$size
  vectors_raw <-  readChar(output_file, v_size)
  words_char <- vectors_raw
  words_lines <- unlist(strsplit(words_char, "\n"))
  words_dim <- as.integer(unlist(strsplit(words_lines[1], " ")))
  vec_vals <- strsplit(words_lines[2:length(words_lines)], " ")
  vec_mat <-do.call(rbind, vec_vals)
  rownames(vec_mat) <- vec_mat[,1]
  vectors <- matrix(as.numeric(vec_mat[,2:(words_dim[2]+1)]), nrow = words_dim[1], ncol = words_dim[2])
  rownames(vectors) <- vec_mat[,1]
  vectors <- wordVectors::as.VectorSpaceModel(vectors)
  
  c_size <- file.info(dumpcv_file)$size
  contexts_raw <-  readChar(dumpcv_file, c_size)
  contexts_char <- contexts_raw
  contexts_lines <- unlist(strsplit(contexts_char, "\n"))
  contexts_dim <- as.integer(unlist(strsplit(contexts_lines[1], " ")))
  contexts_vals <- strsplit(contexts_lines[2:length(contexts_lines)], " ")
  length_test <- sapply(contexts_vals, length)
  contexts_vals <- contexts_vals[unlist(
    which(length_test == (contexts_dim[2] + 1)))]
  contexts_mat <-do.call(rbind, contexts_vals)
  rownames(contexts_mat) <- contexts_mat[,1]
  contexts <- matrix(as.numeric(contexts_mat[,2:(contexts_dim[2]+1)]), 
    nrow = dim(contexts_mat)[1], ncol = contexts_dim[2])
  rownames(contexts) <-  contexts_mat[,1]
  contexts <- wordVectors::as.VectorSpaceModel(contexts)

  }
  overlap <- rownames(vectors) %in% rownames(contexts)
  vectors <- vectors[overlap,]
  list(vectors, contexts)
}
# 
# test <- train_word2vec_dumpcv(train_file = "~/cookbooks.txt",
#   output_file = "~/cookbook_vectors.txt",vectors=20,
#   threads=4,window=5,iter=3,negative_samples=5, dumpcv = 1, 
#   dumpcv_file = "~/cv.txt", force = TRUE)
# 
# 
# words <- wordVectors::normalize_lengths(test[[1]])
# contexts <- wordVectors::normalize_lengths(test[[2]])
# probs_mat <- words %*% t(contexts)
# probs_mat[500:510, 500:510]
# probs_vec <- words[1,] %*% t(contexts)

