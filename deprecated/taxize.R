library(tidyverse)
library(taxize)

setwd("/home/st938/ycelp_nbclust")
load("taxonomies_1.RData")

Sys.setenv(ENTREZ_KEY = "0a02b08164c497abda7cc02cb33efdb32409")
taxize_options(ncbi_sleep = 2.0) 
tmp_taxized <- classification(tmp3_1$GENUS_SPECIES, db = "ncbi")

# Function to get classification with rate limiting and retry mechanism
# get_taxon <- function(taxon_name, retries = 5) {
#   for (i in 1:retries) {
#     tryCatch({
#       result <- classification(taxon_name, db = "ncbi")
#       return(result)
#     }, error = function(e) {
#       if (grepl("API rate limit exceeded", e$message)) {
#         message(paste("Rate limit exceeded for taxon:", taxon_name, "- retrying in", i * 30, "seconds"))
#         Sys.sleep(i * 30)
#       } else {
#         message(paste("Error with taxon:", taxon_name, " - returning NA"))
#         return(NA)
#       }
#     })
#   }
#   return(NA)
# }


#tmp_taxized <- lapply(tmp_taxized$GENUS_SPECIES, get_taxon)

save(tmp_taxized, file="tmp_taxized_slurm.RData")

# extract desired values

extract_foc <- function(df) {
  if (is.data.frame(df)) {
    family_row <- df[df$rank == "family", ]
    order_row = df[df$rank == "order", ]
    class_row = df[df$rank == "class",]
    return(paste(family_row$name, order_row$name,
                 class_row$name, 
                 sep="_"))
  } else {
    return(NA)
  }
}

tmp_foc <- lapply(tmp_taxized, extract_foc)

tmp_foc <- tmp_foc[!sapply(list(tmp_foc), is.na)]

tmp_foc <- tmp_foc[sapply(tmp_foc, length) > 0]

save(tmp_foc, file="tmp_foc.RData")

tmp_foc2 = as.data.frame(tmp_foc) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "species") 

tmp_foc2$species =  str_replace_all(tmp_foc2$species,"\\."," ")

tmp_foc2 = tmp_foc2 %>%
  separate(V1, into = c("family", "order", "class"), sep = "_")

write.csv(tmp_foc2, file = "tmp_foc2.csv")
