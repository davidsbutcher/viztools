protein_results_M9 <-
   readRDS("test_df_protein.rds")

pform_results_M9 <-
   readRDS("test_df_proteoform.rds")

make_protein_heatmap(
   protein_results_M9,
   savePDF = F
)

make_proteoform_heatmap(
   pform_results_M9,
   savePDF = F
)


# dissect TDsummary -------------------------------------------------------

TDsummarypath <-
   # "Z:/ICR/David Butcher/Data Summaries/EcoliMG1655/EcoliMG1655_TDdatasummary.xlsx"
   "C:/Users/ranar/Documents/zdrive_local/EcoliMG1655_TDdatasummary.xlsx"

# List of shortnames of TD reports to get UpSet plots for

shortname <- c("peppi04d")

# Set the following parameters appropriately for the UpSet plots you
# want to make

make_protein_UpSet <- TRUE
make_proteoform_UpSet <- TRUE

# This should only be used if working with an unfractionated run

make_protein_UpSet_unfrac <-  FALSE

dissect_TDsummary(
   "C:/Users/ranar/Documents/zdrive_local/EcoliMG1655_TDdatasummary.xlsx",
   c("peppi04d", "peppi04b")
) %>%
   UpSet_maker(
      savePDF = T,
      outputDir = getwd()
   )
