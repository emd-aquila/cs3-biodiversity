# =====================================================
# Prepare community matrix and compute phylogenetic diversity
# =====================================================

assert_exists(predicts_filtered_path)
assert_exists(tree_file)

# Read and prepare data
raw_data <- read_csv(predicts_filtered_path, show_col_types = FALSE) %>%
  mutate(
    Sample_midpoint = as.Date(Sample_midpoint),
    sample_id = paste(SSBS, Sample_midpoint, sep = "__")
  )

# Export list of family names 
phylo_df <- raw_data %>%
  select(Reference, SSBS, Kingdom:Genus, Effort_corrected_measurement) %>%
  filter(Effort_corrected_measurement != 0) %>%
  mutate(combined_taxonomy = paste(Kingdom, Phylum, Class, Order, Family, Genus, sep = ","))

family_names <- phylo_df %>%
  distinct(Family) %>%
  filter(!is.na(Family))

write.table(
  family_names$Family,
  file = family_present_path,
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

unmatched_names <- c(
  "Napoleonaceae", "Carcinophoridae", "Aphodiidae", "Rhipiphoridae", "Oriolidae",
  "Melolonthidae", "Cephalotaxaceae", "Cetoniidae", "Troglodytidae", "Emberizidae",
  "Anobiidae", "Scydmaenidae", "Coerebidae", "Corytophanidae", "Xanthorrhoeaceae",
  "Cracticidae", "Eumenidae", "Aromobatidae", "Agoutidae", "Callitrichidae",
  "Neosittidae", "Pangrolaimidae", "Paratropididae", "Viduidae", "Aegithinidae",
  "Tropiduridae", "Protoribatidae", "Arctiidae", "Lymantriidae", "Cyphoderidae",
  "Melianthaceae", "Dynastidae", "Pteroclididae", "Dryophthoridae", "Opluridae",
  "Chrysothricaceae", "Not assigned", "Masaridae", "Austrachipteriidae", "Ptilocercidae",
  "Ptilogonatidae", "Languriidae", "Hemiprocnidae", "Bankeraceae", "Phallogastraceae",
  "Corethrellidae", "Indridae", "Symmocidae", "Rhyscotidae", "Ascalaphidae",
  "Tubiferaceae", "Scoliciosporaceae", "Cinclidotaceae", "Taenitidaceae", "Ganodermataceae",
  "Cantacaderidae", "Epimerellidae", "Biatorellaceae", "Eupetidae", "Remizidae",
  "Rutelidae", "Thyrisomidae", "Oxydiridae", "Rhynchitidae", "Heterozetidae",
  "Stemonitidaceae", "Anomalepidae", "Pudeoniscidae", "Odontorhabditidae", "Labidostommidae",
  "Laelaptidae", "Pleuroziopsaceae", "Charipidae", "Paraphelenchidae", "Myoxidae",
  "Microzetidae", "Aleurodamaeidae", "Plateremaeidae", "Spinozetidae", "NA"
)

family_matched <- family_names %>%
  filter(!Family %in% unmatched_names)

write.table(
  setdiff(family_names$Family, family_matched$Family),
  file = family_unmatched_path,
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

# Prepare community matrix (sample_id x Family) for derivation of phylogenetic distance scores
pd_long <- raw_data %>%
  filter(Effort_corrected_measurement > 0, !is.na(Family)) %>%
  group_by(sample_id, Family) %>%
  summarise(abundance = sum(Effort_corrected_measurement), .groups = "drop") %>%
  mutate(abundance = round(abundance, 0))

comm <- pd_long %>%
  pivot_wider(names_from = Family, values_from = abundance, values_fill = 0)

comm_mat <- comm %>%
  column_to_rownames("sample_id") %>%
  as.data.frame()

phylo_tree <- read.tree(tree_file)

if (any(grepl("^f__", phylo_tree$tip.label)) && !any(grepl("^f__", colnames(comm_mat)))) {
  colnames(comm_mat) <- paste0("f__", colnames(comm_mat))
}

pruned_tree <- drop.tip(phylo_tree, setdiff(phylo_tree$tip.label, colnames(comm_mat)))
comm_mat <- comm_mat[, colnames(comm_mat) %in% pruned_tree$tip.label, drop = FALSE]

pd_out <- pd(samp = comm_mat, tree = pruned_tree, include.root = TRUE)

pd_result <- pd_out %>%
  rownames_to_column("sample_id") %>%
  rename(phylo_div = PD)

write_csv_safe(pd_result, pd_output_path)
