# This script runs all Samplers to produce the corresponding samples according 
# to the ALPHA, POWER, and EFFECT_SIZE parameters. 
#
# It requires the installation of:
#   1) Unigen2: https://bitbucket.org/kuldeepmeel/unigen
#   2) QuickSampler: https://github.com/RafaelTupynamba/quicksampler
#   3) Spur: https://github.com/ZaydH/spur
#   4) Smarch: https://github.com/jeho-oh/Kclause_Smarch
#   5) PicoSAT: http://fmv.jku.at/picosat/ 
#   6) probability: https://github.com/rheradio/VMStatAnal 
#
# Please configure the constants QUICK_SAMPLER, QUICK_SAMPLER_VALID, 
# SMARCH, SPUR, UNIGEN2_dir, and UNIGEN2 according to the locations where you 
# have installed the samplers

# This script's inputs are the .dimacs and .bdd files that specify the Boolean 
# encoding of the models. Those models are placed in model_name/bool_formula folders

# This script's outputs are the resulting samples placed in the folders 
# model_name/samples

# Code written by Ruben Heradio

library(tidyverse)
library(pwr)
library(tictoc)
library(R.utils)
library(rapport)

options(warn=-1) # To enable warnings options(warn=0)

# Timeout to get the population characterization, i.e.,
# variable probabilities and SAT solutions' distribution
TIMEOUT_GOODNESS_OF_FIT <- 3600 # in seconds

# Timeout each sampler has to generate a given sampler
TIMEOUT <- 3600 # in seconds

# Parameters to accomodate the reliability of the Chi-Squared test
# The sample size depends on: (i) the degrees of freedom of the Chi-Squared test, 
# i.e., on the configuration model, and (ii) three parameters that accommodate 
# the reliability of the test: ALPHA, POWER, and EFFECT_SIZE
ALPHA <- 0.01 # Probability of making an error of Type I
POWER <- 0.99 # 1 - Probability of making an error of Type II
# As a rule of thumb, EFFECT_SIZE values around 0.1, 0.3, and 0.5 
# correspond to small, medium, and large effect sizes, respectively.
EFFECT_SIZE <- 0.1 # 0.1, 0.3, 0.5

args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  MODELS_PATH <- "~/random_sampling/models"
} else {
  MODELS_PATH <- args[1]
}

MODELS_EXTENSIONS <-
  c("quicksampler", "smarch", "spur", "unigen2")
DIRECTORIES <- 
  c("goodness_of_fit", "population_desc", "samples", "std_samples")
VAR_PROB <- "~/random_sampling/samplers/bddsampler/bin/probability "
QUICK_SAMPLER <- "~/random_sampling/samplers/quicksampler/quicksampler/quicksampler "
QUICK_SAMPLER_VALID <- "~/random_sampling/samplers/quicksampler/z3/build/z3 "
SMARCH <- "~/anaconda3/bin/python3 ~/random_sampling/samplers/smarch/smarch.py "
SPUR <- "~/random_sampling/samplers/spur/build/Release/spur "
UNIGEN2_dir <- "~/random_sampling/samplers/unigen2/ugen2/build"
UNIGEN2 <- "python ../../UniGen2.py -threads=1 "

population_desc_stats <- 
  tibble(model = character(), 
         vars = numeric(), 
         clauses = numeric(), 
         var_prob_time = numeric())

samplers_stats <- 
  tibble(model = character(), 
         vars = numeric(), 
         clauses = numeric(), 
         sample_size = numeric(),
         quicksampler_time = numeric(), 
         quicksampler_valid_time_percentage = numeric(), 
         quicksampler_valid_percentage = numeric(), 
         smarch_time = numeric(), 
         spur_time = numeric(), 
         unigen2_time = numeric())

models <- dir(path = MODELS_PATH)

for (m in models) {
  
  writeLines(str_c("================================================================="))
  writeLines(str_c("Proccessing ", m, "..."))
  writeLines(str_c("================================================================="))
  
  # Create directories
  for (d in DIRECTORIES) {
    dir_name <- str_c(MODELS_PATH, "/", m, "/", d)
    if (!dir.exists(dir_name)) {
      dir.create(dir_name)
    }
  }
  
  dimacs <- str_c(MODELS_PATH, "/", m, "/bool_formula/", m, ".dimacs")
  dimacs_code <- read_file(dimacs)
  dimacs_tmp <- dimacs_code %>%
    str_extract("p\\s+cnf\\s+(\\d+)\\s+(\\d+)") %>%
    str_extract_all("\\d+") %>% 
    unlist
  var_num <- dimacs_tmp[1]
  clause_num <- dimacs_tmp[2]
  
  # Get population descriptions
  model_file <- str_c(MODELS_PATH, "/", m, "/bool_formula/", m)
  pop_desc_dir <- str_c(MODELS_PATH, "/", m, "/population_desc")
  
  writeLines(str_c("  Getting var probabilities"))
  tic(quiet=TRUE)
  var_prob_result <- system(str_c(VAR_PROB, model_file, " > ", pop_desc_dir, "/", m, ".varprob"), 
                            timeout = TIMEOUT_GOODNESS_OF_FIT)
  t <- toc(quiet=TRUE)
  var_prob_t <- t$toc-t$tic
  
  if (var_prob_result == 0) {
    population_desc_stats <- population_desc_stats %>%
      add_row(model = m, 
              vars = var_num,
              clauses = clause_num, 
              var_prob_time = var_prob_t)
  } else {
    writeLines(str_c("  << TIMEOUT for computing the population variable probabilities >>"))
    next()
  }
  
  # Get sample size ##################################################
  
  degrees_of_freedom <- read_delim(
    file = str_c(pop_desc_dir, "/", m, ".varprob"),
    delim = " ",
    col_names = FALSE,
    col_types = cols(
      col_character(),
      col_double()
    )
  ) %>% 
    filter(X2>0, X2<1) %>%
    nrow
  degrees_of_freedom <- degrees_of_freedom-1
  
  sample_size <- pwr.chisq.test(
    w = EFFECT_SIZE,                   # 0.1, 0.3, 0.5
    N = NULL,                          # Total number of observations
    df = degrees_of_freedom,           # Degrees of freedom
    power = POWER,                     # 1 minus Type II Error probability
    sig.level = ALPHA)                 # Type I Error probability 
  sample_size <- round(sample_size$N)
  
  sample_dir <-  str_c(MODELS_PATH, "/", m, "/samples")
  unlink(sample_dir, recursive=TRUE)
  dir.create(sample_dir)
  
  for (ext in MODELS_EXTENSIONS) {
    
    writeLines(str_c("  sampling with ", ext))
    
    # QuickSampler and Unigen2 produce concise outputs as Minimal Independent Support 
    # (MIS). To extend such MISes to "complete samples", PicoSAT is called    
    if (ext == "quicksampler") {
      
      output_file <- str_c(sample_dir, "/", m, ".", ext)
      tic(quiet=TRUE)
      quicksampler_result <- system(str_c(QUICK_SAMPLER, "-n ", sample_size, " -t ", TIMEOUT + 60, " ", model_file, ".dimacs"), 
                                    timeout = TIMEOUT, ignore.stdout=TRUE)
      t <- toc(quiet=TRUE)
      quicksampler_t <- t$toc-t$tic
      
      if (quicksampler_result != 0) {
        quicksampler_valid_percentage <- NA
        quicksampler_valid_t_percentage <- NA
        quicksampler_valid_t <- NA
      } else {
        writeLines("    checking the validity of the samples")      
        tic(quiet=TRUE)
        quicksampler_valid_result <- system(str_c(QUICK_SAMPLER_VALID, "sat.quicksampler_check=true sat.quicksampler_check.timeout=", TIMEOUT - quicksampler_t + 60, " ", model_file, ".dimacs"),
                                            timeout = TIMEOUT-quicksampler_t, ignore.stdout=TRUE)  
        t <- toc(quiet=TRUE)
        if (quicksampler_valid_result != 0) {
          quicksampler_valid_t <- NA
          quicksampler_valid_percentage <- NA
          quicksampler_valid_t_percentage <- NA
          file.remove(str_c(model_file, ".dimacs.samples"))
        } else {
          valid_samples_size <- countLines(str_c(model_file, ".dimacs.samples.valid"))[1]
          final_sample_size <- min(valid_samples_size, sample_size)
          writeLines("    getting samples from independent supports")      
          sample <- rep(NA, final_sample_size)
          independent_support_assignments <- readLines(str_c(model_file, ".dimacs.samples.valid"))
          i <- 1
          while (i<=final_sample_size) {
            ind_sup <- independent_support_assignments[i]
            if (i%%10 == 0) {
              writeLines(str_c("    ", i, " of ", final_sample_size))
            }
            literals <- str_split(ind_sup, "\\s+") %>% unlist 
            literals <- literals[-length(literals)]
            literals_text <- str_c(literals, collapse=" 0\n")
            dimacs_code_aux <- str_c(dimacs_code, literals_text," 0\n") 
            clauses <- str_extract(dimacs_code_aux, "(?<=p cnf \\d{1,1000000} )\\d+")
            clauses <- as.numeric(clauses)
            dimacs_code_aux <- str_replace(dimacs_code_aux, 
                                           "(p cnf \\d+ )\\d+", 
                                           str_c("\\1", length(literals)+clauses))
            write(dimacs_code_aux, str_c(model_file, "_tmp.dimacs"))
            system(str_c("picosat ", model_file, "_tmp.dimacs > ", model_file, ".picosat"))
            picosat_output <- read_file(str_c(model_file, ".picosat")) %>%
              str_extract_all("-?\\d+") %>%
              unlist
            assignment <- picosat_output[!(picosat_output == "0")] %>%
              str_c(collapse=" ")
            sample[i] <- assignment
            i <- i + 1
          }
          write(sample, str_c(sample_dir, "/", m, ".quicksampler"))
          quicksampler_valid_t <- t$toc-t$tic
          quicksampler_t <- quicksampler_t + quicksampler_valid_t
          quicksampler_valid_t_percentage <- (100*quicksampler_valid_t)/quicksampler_t
          quicksampler_valid_percentage <- (final_sample_size * 100)/sample_size
          file.copy(str_c(model_file, ".dimacs.samples.valid"), str_c(sample_dir, "/", m, "..quicksampler_ind_supp"))
          file.remove(str_c(model_file, ".dimacs.samples"))
          file.remove(str_c(model_file, ".dimacs.samples.valid"))
          file.remove(str_c(model_file, "_tmp.dimacs"))
          file.remove(str_c(model_file, ".picosat"))
        }
      } 
    } # if (ext == "quicksampler") 
    
    if (ext == "smarch") {
      
      output_file <- str_c(sample_dir, "/", m, ".", ext)
      tic(quiet=TRUE)
      smarch_result <- system(str_c(SMARCH, " -o ", sample_dir, " ", model_file, ".dimacs ", sample_size), 
                              timeout = TIMEOUT, ignore.stdout=TRUE)
      t <- toc(quiet=TRUE)
      smarch_t <- t$toc-t$tic
      
      if (smarch_result != 0) {
        file.remove(str_c(sample_dir, "/", m, "_", sample_size, ".samples"))
        smarch_t <- NA
      } else {
        file.rename(str_c(sample_dir, "/", m, "_", sample_size, ".samples"),
                    str_c(sample_dir, "/", m, ".", ext))
      }
      unlink(str_c(sample_dir, "/smarch"), recursive=TRUE)
      
    } # if (ext == "smarch")      
    
    if (ext == "spur") {
      
      output_file <- str_c(sample_dir, "/", m, ".", ext)
      tic(quiet=TRUE)
      spur_result <- system(str_c(SPUR, "-t ", TIMEOUT + 60, " -s ", sample_size, " -cnf ", model_file, ".dimacs", " -out ", output_file),
                            timeout = TIMEOUT, ignore.stdout=TRUE)
      t <- toc(quiet=TRUE)
      spur_t <- t$toc-t$tic
      
      if (spur_result != 0) {
        file.remove(output_file)
        spur_t <- NA
      } 
    } # if (ext == "spur")    
    
    if (ext == "unigen2") {
      
      output_file <- str_c(sample_dir, "/", m, ".", ext)
      
      wd_aux <- getwd()
      setwd(UNIGEN2_dir)
      tic(quiet=TRUE)
      unigen2_result <- system(str_c(UNIGEN2, "-samples=", sample_size, " ", model_file, ".dimacs", " ", sample_dir),
                               timeout = TIMEOUT, ignore.stdout=TRUE)
      t <- toc(quiet=TRUE)
      unigen2_t <- t$toc-t$tic
      
      unigen_txt <- list.files(
        path = sample_dir,
        pattern = str_c("[.]txt$"),
        recursive = TRUE,
        include.dirs = TRUE
      )
      
      unigen_count <- list.files(
        path = sample_dir,
        pattern = str_c("[.]count$"),
        recursive = TRUE,
        include.dirs = TRUE
      )
      
      if ( (unigen2_result != 0) || identical(unigen_txt, character(0))){
        file.remove(str_c(sample_dir, "/", unigen_txt))
        unigen2_t <- NA
      } else {
        file.rename(str_c(sample_dir, "/", unigen_txt),
                    str_c(sample_dir, "/", m, ".unigen2_ind_supp"))
        sample <- rep(NA, sample_size)
        independent_support_assignments <- readLines(str_c(sample_dir, "/", m, ".unigen2_ind_supp"))
        i <- 1
        for (ind_sup in independent_support_assignments) {
          if (i%%10 == 0) {
            writeLines(str_c("    ", i, " of ", sample_size))
          }
          if (str_trim(ind_sup) == "") {
            next
          }
          literals <- str_extract(ind_sup, "(?<=v)(-?\\d+\\s)+") %>% 
            trim %>%
            str_split("\\s") %>%
            unlist
          dimacs_code_aux <- str_c(dimacs_code, literals_text," 0\n") 
          literals_text <- str_c(literals, collapse=" 0\n")
          number_of_sol <- str_extract(ind_sup, "(?<=0:)\\d+") %>% as.numeric
          clauses <- str_extract(dimacs_code_aux, "(?<=p cnf \\d{1,1000000} )\\d+")
          clauses <- as.numeric(clauses)
          dimacs_code_aux <- str_replace(dimacs_code_aux, 
                                         "(p cnf \\d+ )\\d+", 
                                         str_c("\\1", length(literals)+clauses))
          write(dimacs_code_aux, str_c(model_file, "_tmp.dimacs"))    
          for (sol in 1:number_of_sol) {
            system(str_c("picosat ", model_file, "_tmp.dimacs > ", model_file, ".picosat"))
            picosat_output <- read_file(str_c(model_file, ".picosat")) %>%
              str_extract_all("-?\\d+") %>%
              unlist
            assignment <- picosat_output[!(picosat_output == "0")] %>%
              str_c(collapse=" ")
            sample[i] <- assignment
            i <- i+1
          }  # for (i in number_of_sol)
        }  # for (ind_sup in independent_support_assignments)
        write(sample, str_c(sample_dir, "/", m, ".unigen2"))
        file.remove(str_c(model_file, "_tmp.dimacs"))
        file.remove(str_c(model_file, ".picosat"))
      } # else 
      
      file.remove(str_c(sample_dir, "/", unigen_count))
      setwd(wd_aux)
      
    } # if (ext == "unigen2")       
    
  } # for (ext in MODELS_EXTENSIONS) 
  
  samplers_stats <- samplers_stats %>%
    add_row(model = m, 
            vars = var_num, 
            clauses = clause_num, 
            sample_size = sample_size,
            quicksampler_time = quicksampler_t, 
            quicksampler_valid_time_percentage = quicksampler_valid_t_percentage, 
            quicksampler_valid_percentage = quicksampler_valid_percentage, 
            smarch_time = smarch_t, 
            spur_time = spur_t, 
            unigen2_time = unigen2_t)
  
} # for (m in models) 

model_group_name <- str_extract(MODELS_PATH, "(?<=~/random_sampling/).+$")
model_group_name <- MODELS_PATH

write.table(
  population_desc_stats,
  file = str_c(model_group_name,"_population_desc_stats.csv"),
  sep = ";",
  row.names = FALSE
) 

write.table(
  samplers_stats,
  file = str_c(model_group_name,"_samplers_stats.csv"),
  sep = ";",
  row.names = FALSE
)