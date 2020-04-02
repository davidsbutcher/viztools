library(magrittr)


# dissect TDsummary testing ----------------------------------------------------

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
      savePDF = F,
      outputDir = getwd()
   )


# Heatmap testing ---------------------------------------------------------

protein_results_M9 <-
   readRDS("inst/test/test_df_protein.rds")

# protein_results_allhits_M9 <-
#    readr::read_csv("inst/test/test_df_protein_allhits.csv")

pform_results_M9 <-
   readRDS("inst/test/test_df_proteoform.rds")


##

make_protein_heatmap(
   protein_results_M9,
   savePDF = F
)

make_proteoform_heatmap(
   pform_results_M9,
   savePDF = F
)




## TAM only allhits protein heatmap

readxl::read_xlsx("C:/Users/ranar/Documents/guppi_output/peppi_vs_gelfree/protein_results_allhits/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_allhits.xlsx") %>%
   dplyr::filter(ResultSet == "Tight Absolute Mass") %>%
   make_protein_heatmap(
      savePDF = T
   )

## No BioMarker allhits protein heatmap

readxl::read_xlsx("C:/Users/ranar/Documents/guppi_output/peppi_vs_gelfree/protein_results_allhits/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_allhits.xlsx") %>%
   dplyr::filter(ResultSet != "BioMarker") %>%
   make_protein_heatmap(
      savePDF = T
   )

## BioMarker only allhits protein heatmap

readxl::read_xlsx("C:/Users/ranar/Documents/guppi_output/peppi_vs_gelfree/protein_results_allhits/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_allhits.xlsx") %>%
   dplyr::filter(ResultSet == "BioMarker") %>%
   make_protein_heatmap(
      savePDF = T
   )

## Result sets by ObsPrecursorMass - added to GUPPI

library(ggplot2)

df <-
   readxl::read_xlsx("C:/Users/ranar/Documents/guppi_output/peppi_vs_gelfree/protein_results_allhits/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_allhits.xlsx")

df %>%
   dplyr::mutate(ResultSet = stringr::str_wrap(ResultSet,10)) %>%
   ggplot(aes(ObservedPrecursorMass/1000, ResultSet)) +
   geom_bin2d(aes(group = ResultSet), position = "identity", stat = "bin2d") +
   scale_fill_viridis_c(option = "C", direction = -1) +
   labs(
      x = "Mass Bin (kDa)",
      y = "Result Set"
   ) +
   guides(
      group = "Protein\nCount"
   ) +
   theme_minimal() +
   theme(text = ggplot2::element_text(size=18))

df %>%
   dplyr::filter(ResultSet != "Find Unexpected Modifications") %>%
   dplyr::mutate(ResultSet = stringr::str_wrap(ResultSet,10)) %>%
   ggplot(aes(ObservedPrecursorMass/1000, ResultSet)) +
   geom_bin2d(aes(group = ResultSet), position = "identity", stat = "bin2d") +
   scale_fill_viridis_c(option = "C", direction = -1) +
   labs(
      x = "Mass Bin (kDa)",
      y = "Result Set"
   ) +
   guides(
      group = "Protein\nCount"
   ) +
   theme_minimal() +
   theme(text = ggplot2::element_text(size=18))

## Further experimentation

dissect_TDsummary(
   "C:/Users/ranar/Documents/zdrive_local/EcoliMG1655_TDdatasummary.xlsx",
   c("gf06a"),
   UpSetType = "protein"
) %>%
   UpSet_maker(
      savePDF = F,
      outputDir = "C:/Users/ranar/Documents/viztools_output",
      UpSetType = "protein"
   )

dissect_TDsummary(
   "C:/Users/ranar/Documents/zdrive_local/EcoliMG1655_TDdatasummary.xlsx",
   c("gf06a"),
   UpSetType = "proteoform"
) %>%
   UpSet_maker(
      savePDF = T,
      outputDir = "C:/Users/ranar/Documents/viztools_output",
      UpSetType = "proteoform"
   )

## Test modify_hit_report and make_heatmap

hit_report <-
   readxl::read_xlsx(
      "C:/Users/ranar/Documents/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_Hit_Report.xlsx"
   )

isoform_report <-
   readxl::read_xlsx(
      "C:/Users/ranar/Documents/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_Isoform_Report.xlsx"
   )

proteoform_report <-
   readxl::read_xlsx(
      "C:/Users/ranar/Documents/20190907_EcoliMG1655_PEPPI_M9_F01-F09_CAMsearch_newF7_Proteoform_Report.xlsx"
   )

testHitRep <-
   modify_hit_report(
      hit_report,
      isoformReport = isoform_report,
      proteoformReport = proteoform_report,
      IDtype = "proteoform"
   )

make_heatmap(
   testHitRep2,
   heatmapType = "Proteoform",
   savePDF = F,
   binSize = 1000
)




tibble::tibble(
   "fraction" = c(1,1,1,1,2,2,3,3,3,3),
   "ObservedPrecursorMass" = c(1500,3000,4200,4250,3500,4500,10500,12050,12075,14050)
) %>%
   make_heatmap(
      heatmapType = "Proteoform",
      savePDF = F,
      binSize = 500
   )

tibble::tibble(
   "fraction" = c(1),
   "ObservedPrecursorMass" = c(2001)
) %>%
   make_heatmap(
      heatmapType = "Proteoform",
      savePDF = F,
      binSize = 1000
   )


