## Messages

MSG_NO_INPUT_DATA="No input data selected"
MSG_K_MIN_NOT_IN_RANGE="Min. num. of clusters must be in range [2,15]"
MSG_K_MAX_NOT_IN_RANGE="Min. num. of clusters must be in range [2,15]"
MSG_K_MIN_GREATER_THAN_K_MAX="Min. num. of clusters cannot be greater than max. num. of clusters"
MSG_STABILITY_WENT_WRONG="Stability indexes calculation could not finish properly"

## // Messages

## Sample CSV data

wd = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")

SAMPLE_FILE_MAP = c("Gene expression differences" = paste0(wd, "data/dataset-metrics-Imbeaudetal-NAR2005.csv"),
                    "78 AgroPortal ontologies" = paste0(wd, "data/agro.csv"),
                    "119 OBO Foundry ontologies" = paste0(wd, "data/obo-119.csv"))

## // Sample CSV data