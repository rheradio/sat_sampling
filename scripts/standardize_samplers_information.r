# This script standardizes the outputs of the following samplers: 
# Spur, QuickSampler, Smarch, and Unigen2.
# For each model and sampler, a resulting csv file called
# model_name_varprob.sampler is generated in the 
# corresponding model_name/st_samples folder: 
#
# Code written by Ruben Heradio

library(tidyverse)
library(gmp)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  MODELS_PATH <- "../data"
} else {
  MODELS_PATH <- args[1]
}

MODELS_EXTENSIONS <-
  c("quicksampler", "smarch", "spur", "unigen2")

models <- dir(path = MODELS_PATH)

for (m in models) {
  
  writeLines(str_c("Processing ", m))
  
  # get var names from .dddmp #################
  
  path <- str_c(MODELS_PATH,
                "/",
                m,
                "/bool_formula")
  
  writeLines(str_c("  Reading the dddmp file for ", path))
  dddmp_filename <- list.files(
    path = path,
    pattern = str_c("[.]dddmp$"),
    recursive = TRUE,
    include.dirs = TRUE
  )
  if (length(dddmp_filename) != 1) {
    stop(str_c("  << ERROR: no dddmp file for ", path, " >>\n"))
  }
  dddmp_code <- read_file(str_c(path, "/", dddmp_filename))
  var_names <-
    str_extract(dddmp_code, "(?<=[.]varnames\\s).*") %>%
    str_split("\\s+") %>%
    unlist 
  var_names <- stringi::stri_remove_empty(var_names)

  for (ext in MODELS_EXTENSIONS) {
    
    writeLines(str_c("  Analyzing ", m, ".", ext))
    
    path <- str_c(MODELS_PATH,
                  "/",
                  m,
                  "/samples")
    m_filename <- list.files(
      path = path,
      pattern = str_c("[.]", ext, "$"),
      recursive = TRUE,
      include.dirs = TRUE
    )
    if (length(m_filename) != 1) {
      cat(str_c("  << WARNING: ", ext, " sample not found >>\n"))
      next()
    }
    
    path <- str_c(MODELS_PATH,
                  "/",
                  m,
                  "/samples/",
                  m_filename)
    
    # get sample data.frame
    
    if (ext %in% c("quicksampler", "smarch", "unigen2")) {
      sep <- NA
      if (ext %in% c("quicksampler", "unigen2")) {
        sep <- " "
      } else { # (ext == "smarch")
        sep <- ","
      }
      sample <- read.csv(
        path,
        header = FALSE,
        sep = sep,
        na = c("", "NA"),
        strip.white = FALSE
      )
    } else if (ext == "spur") { 
      sample_code <- read_file(path) 
      aux_sample <- unlist(str_extract_all(sample_code,
                                           regex("^\\d+,[01*]+", multiline=TRUE)))
      nrows <-
        str_extract_all(sample_code,
                        regex("^\\d+(?=,[01*]+)", multiline=TRUE))  %>%
        unlist() %>%
        as.numeric() %>%
        sum()
      ncols <- aux_sample[1] %>%
        str_extract_all("(?<=,)[01*]+") %>%
        nchar()
      
      sample <- matrix(rep(NA, nrows * ncols),
                       nrows,
                       ncols)
      
      i = 1
      for (s in aux_sample) {
        n <- str_extract(s,
                         "\\d+(?=,[01*]+)") %>%
          unlist() %>%
          as.numeric()
        for (j in 1:n) {
          aux_s <- s
          while (str_detect(aux_s, "[*]")) {
            aux_s <- str_replace(aux_s, "[*]",
                                 if_else(rbernoulli(1), "1", "0"))
          }
          row <-
            str_extract(aux_s, "(?<=,)[01]+") %>%
            str_replace_all("([01])", "\\1,") %>%
            str_split(",") %>%
            unlist()
          row <- row[-length(row)]
          sample[i, ] <- row
          i <- i + 1
        }
      } # for (s in aux_sample)
      sample <- as.data.frame(sample,
                              stringsAsFactors = FALSE)
    } 
        
    # get satdist from the sample data.frame
        
    # ext == quicksampler or unigen2 ########################################
    
    if (ext %in% c("quicksampler", "unigen2")) {
      sample[sample > 0] <- 1
      sample[sample < 0] <- 0
            
      # Variable probability
      probability_sample <- rep(NA, ncol(sample))
      for (i in 1:ncol(sample)) {
        probability_sample[i] <- sum(sample[[i]])
      }
      probability_sample <- probability_sample / nrow(sample)
      
    }
    
    # ext == smarch ########################################
    
    if (ext == "smarch") {
      # remove last column
      sample <- sample[-ncol(sample)]
            
      # Variable probability
      probability_sample <- rep(NA, ncol(sample))
      for (i in 1:ncol(sample)) {
        probability_sample[i] <- sum(sample[[i]])
      }
      probability_sample <- probability_sample / nrow(sample)
      
      
    }
    
    # ext == spur ########################################
    
    if (ext == "spur") {
      
      # Variable probabilities
      probability_sample <- rep(NA, ncol(sample))
      for (i in 1:(ncol(sample))) {
        probability_sample[i] <- sum(as.numeric(sample[[i]]))
      }
      probability_sample <- probability_sample / nrow(sample)
    }
    
    # Write the resulting file ########################################
        
    probability_sample <-
        tibble(var = var_names, prob = probability_sample)
    
    file_name <- str_c(MODELS_PATH,
                       "/",
                       m,
                       "/std_samples/",
                       m, "_varprob.", ext)
    write.table(
      probability_sample,
      file = file_name,
      sep = ";",
      row.names = FALSE
    )

  } # for (ext in MODELS_EXTENSIONS) 
} # for (m in models)
