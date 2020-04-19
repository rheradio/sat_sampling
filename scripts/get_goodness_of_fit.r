# This script takes the get_sample_desc.r output (i.e., files 
# model_name_varprob.sampler) 
# and tests the goodness of fit of the samples.
#
# This script's outputs are
# 1) sampler_pvalues.csv stores the pvalues corresponding to the goodness of fit
# 2) a density plot comparing the population's variable probabilities with the sample's ones. 
#    These plots are placed in the folder:  model_name/goodness_of_fit
#
# Code written by Ruben Heradio

library(tidyverse)
library(gmp)
library(forcats)
library(psych)
library(gridExtra)
require("Rmpfr")
library(philentropy) 

args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  MODELS_PATH <- "../data"
} else {
  MODELS_PATH <- args[1]
}

writeLines(MODELS_PATH)

pretty_name <- function(id) {
  if (id == "quicksampler") {
    "Quicksampler"
  } else if (id == "smarch") {
    "Smarch"
  } else if (id == "spur") {
    "Spur"
  } else if (id == "unigen2") {
    "Unigen2"
  } else {
    stop("error in pretty_name")
  }
}

MODELS_EXTENSIONS <-
  c("quicksampler", "smarch", "spur", "unigen2")

models <- dir(
  path = MODELS_PATH)

models <- models[!str_detect(models, "[.]csv")]


# auxiliary function and variable for plots 

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

blank_plot <-
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

p_values <- 
  tibble(model = character(), 
         quicksampler_varprob_p = numeric(), 
         smarch_varprob_p = numeric(), 
         spur_varprob_p = numeric(), 
         unigen2_varprob_p = numeric()  
)


for (m in models) {

  writeLines(str_c("Goodness of fit for ", m))

  quicksampler_varprob_p_value <- NA 
  smarch_varprob_p_value <- NA 
  spur_varprob_p_value <- NA 
  unigen2_varprob_p_value <- NA

  # Get theoretical informations ############
  
  th_path <- str_c(MODELS_PATH,
                "/",
                m,
                "/population_desc")
  

  theoretical_var_probs <- read_delim(
    file = str_c(th_path, "/", m, ".varprob"),
    delim = " ",
    col_names = FALSE,
    col_types = cols(
      col_character(),
      col_double()
    )
  )
  colnames(theoretical_var_probs) <- c("var", "th_prob")
  theoretical_var_probs$var <- str_trim(theoretical_var_probs$var)
  
  # Check empirical goodness of fit
  
  plot_index <- 0
  density_plots <- list()
  
  sampler_stats <- read_delim(
    file = str_c(MODELS_PATH, "/samplers_stats.csv"),
    delim = ";",
    col_names = TRUE,
    col_types = cols(
      col_character(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_double(),
      col_double(),
      col_double(),
      col_double(),
      col_double(),
      col_double()
    )
  )  
  
  for (ext in MODELS_EXTENSIONS) {
    plot_index <- plot_index + 1
    
    file_name <- str_c(MODELS_PATH,
                       "/",
                       m,
                       "/std_samples/",
                       m,
                       "_varprob.",
                       ext)
    
    if (!file.exists(file_name)) {
      empty_plot <-
        ggplot()+
        ggtitle(pretty_name(ext)) +
        annotate(geom="text", x=3, y=30, label="<< time out >>",
                 color="red",
                 size=12) +
        blank_plot
      density_plots[[plot_index]] <- empty_plot 
      next()
    }    
    
    empirical_var_probs <- read_delim(
      file = file_name,
      delim = ";",
      col_names = TRUE,
      col_types = cols(
        col_character(),
        col_double()
      )
    )    
    sample_size <- sampler_stats[sampler_stats$model == m, ]$sample_size

    var_probs <- theoretical_var_probs %>%
      inner_join(empirical_var_probs, by="var") %>%
      rename(emp_prob = prob) %>%
      filter(th_prob>0, th_prob<1) %>%
      mutate(th_prob_adj = th_prob*sample_size) %>%
      mutate(emp_occurrences = emp_prob*sample_size) %>%
      mutate(X2 = (emp_occurrences-th_prob_adj)^2 / th_prob_adj)
    
    # Var probs' goodness of fit ######################################
    
    jensen_shannon_value <- 
      jensen_shannon(var_probs$th_prob,
                     var_probs$emp_prob,
                     testNA = FALSE,
                     unit = "log2")
    X2 <- 2*sample_size*log(2)*jensen_shannon_value
    degrees_of_freedom <- nrow(var_probs)-1
    var_prob_p_value <- 1-pchisq(X2, degrees_of_freedom)    
    writeLines(str_c(m, ".", ext, "'s prob p-value = ", var_prob_p_value))
    
    abs_diff <- tibble(
      diff = abs(var_probs$emp_prob - var_probs$th_prob)
    )
    
    density_plots[[plot_index]] <- 
      ggplot(abs_diff, aes(x=diff)) +
      geom_density(fill="sienna3", col="sienna3",alpha=0.6) +
      coord_cartesian(xlim=c(0,1)) +
      scale_x_continuous("Variable probability absolute difference") +
      scale_y_continuous("Density") +
      theme(axis.text.x = element_text (angle=60, hjust=1)) +
      ggtitle(pretty_name(ext))
    
    if (ext == "quicksampler") {
      quicksampler_varprob_p_value <- var_prob_p_value      
    } else if (ext == "smarch") {
      smarch_varprob_p_value <- var_prob_p_value 
    } else if (ext == "spur") {
      spur_varprob_p_value <- var_prob_p_value
    } else if (ext == "unigen2") {
      unigen2_varprob_p_value <- var_prob_p_value    
    } 
  } # for (ext in MODELS_EXTENSIONS)
  
  p_values <- p_values %>%
    add_row(model = m, 
           quicksampler_varprob_p = quicksampler_varprob_p_value, 
           smarch_varprob_p = smarch_varprob_p_value, 
           spur_varprob_p = spur_varprob_p_value, 
           unigen2_varprob_p = unigen2_varprob_p_value
    )  
  
  file_name <- str_c(MODELS_PATH,
                     "/",
                     m,
                     "/goodness_of_fit/",
                     m,
                     "_var_prob_density.pdf")
  cairo_pdf(file_name, width=15, height=6)
  grid.arrange(density_plots[[1]], density_plots[[2]], 
               density_plots[[3]], density_plots[[4]], 
               nrow=2)
  dev.off()

}

write.table(
  p_values,
  file = str_c(MODELS_PATH,"/sampler_pvalues.csv"),
  sep = ";",
  row.names = FALSE
)
