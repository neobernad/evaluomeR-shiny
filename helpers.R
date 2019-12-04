wd_helpers = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")

SAMPLE_FILE_MAP = c("Gene expression differences" = paste0(wd_helpers, "data/dataset-metrics-Imbeaudetal-NAR2005.csv"),
                    "78 AgroPortal ontologies" = paste0(wd_helpers, "data/agro.csv"),
                    "119 OBO Foundry ontologies" = paste0(wd_helpers, "data/obo-119.csv"))

getSampleFilesMap <- function() {
    return(SAMPLE_FILE_MAP)
}