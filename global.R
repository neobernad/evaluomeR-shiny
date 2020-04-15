## Messages

MSG_NO_INPUT_DATA="No input data selected"
MSG_K_MIN_NOT_IN_RANGE="Min. num. of clusters must be in range [2,15]"
MSG_K_MAX_NOT_IN_RANGE="Min. num. of clusters must be in range [2,15]"
MSG_K_MIN_GREATER_THAN_K_MAX="Min. num. of clusters cannot be greater than max. num. of clusters"
MSG_STABILITY_WENT_WRONG="Stability indexes calculation could not finish properly"
MSG_QUALITY_WENT_WRONG="Quality analysis calculation could not finish properly"
MSG_CORRELATIONS_WENT_WRONG="Correlations calculation could not finish properly"

## // Messages

## Sample CSV data

#wd = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
wd = paste0(getwd(),"/")
print(wd)

SAMPLE_FILE_MAP = c("Gene expression differences" = paste0(wd, "data/dataset-metrics-Imbeaudetal-NAR2005.csv"),
                    "78 AgroPortal ontologies" = paste0(wd, "data/agro.csv"),
                    "119 OBO Foundry ontologies" = paste0(wd, "data/obo-119.csv"))

## // Sample CSV data

## Global functions

getFormattedK <- function(k) {
  return(gsub("^.*_","", k))
}

standardizeQualityData <- function(qualData, k.range=NULL) {
  lengthQuality = length(qualData)
  qualRangeStart = getFormattedK(names(qualData)[1])
  qualRangeEnd = getFormattedK(names(qualData)[lengthQuality])
  Metric = NULL
  kValues = list()
  for (i in seq(qualRangeStart, qualRangeEnd, 1)) {
    curQual = as.data.frame(assay(getDataQualityRange(qualData, i)))
    if (i == qualRangeStart) {
      Metric = as.character(curQual$Metric)
    }
    kValues[[i]] = as.numeric(as.character(curQual$Avg_Silhouette_Width))
  }
  qualDf = as.data.frame(Metric)
  for (i in seq(qualRangeStart, qualRangeEnd, 1)) {
    values = kValues[[i]]
    newColname = paste0("k_", i)
    k = as.numeric(getFormattedK(newColname))
    if (!is.null(k.range) && (k < k.range[1] || k > k.range[2])) {
      next
    }
    qualDf[[newColname]] = values
  }
  
  if (!is.null(k.range) && (k.range[1] < qualRangeStart || k.range[2] > qualRangeEnd)) {
    # Input k.range is not a subset of the stabData k ranges
    stop("Input k.range [", k.range[1], ", ", k.range[2], "] is not a subset of range [",
         qualRangeStart, ", ", qualRangeEnd, "]")
  }
  
  rownames(qualDf) = qualDf$Metric
  qualDf = qualDf[, -1] # Remove "Metric" column, metrics are rownames now
  qualDf <- qualDf[ order(row.names(qualDf)), ]
  return(qualDf)
}

## // Global functions