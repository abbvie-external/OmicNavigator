# This file contains the documentation for the example objects from the User's
# Guide.
#
# Created by scripts/RNAseq123.R
# Saved in data/RNAseq123.RData
# Used by vignettes/OmicAnalyzerUsersGuide.Rnw

#' basal.vs.lp from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{basal.vs.lp} from Bioconductor workflow RNAseq123.
#'
#' @format A data frame with 24 rows and 8 columns:
#' \describe{
#'   \item{ENTREZID}{Entrez ID of mouse gene}
#'   \item{SYMBOL}{Symbol of mouse gene}
#'   \item{TXCHROM}{Chromosome location of mouse gene}
#'   \item{logFC}{Log fold change}
#'   \item{AveExpr}{Average expression level of the gene across all samples}
#'   \item{t}{Moderated t-statistic}
#'   \item{P.Value}{p-value}
#'   \item{adj.P.Val}{Adjusted p-value}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   head(basal.vs.lp)
#'   str(basal.vs.lp)
#'
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"basal.vs.lp"

#' basal.vs.ml from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{basal.vs.ml} from Bioconductor workflow RNAseq123.
#'
#' @format A data frame with 24 rows and 8 columns:
#' \describe{
#'   \item{ENTREZID}{Entrez ID of mouse gene}
#'   \item{SYMBOL}{Symbol of mouse gene}
#'   \item{TXCHROM}{Chromosome location of mouse gene}
#'   \item{logFC}{Log fold change}
#'   \item{AveExpr}{Average expression level of the gene across all samples}
#'   \item{t}{Moderated t-statistic}
#'   \item{P.Value}{p-value}
#'   \item{adj.P.Val}{Adjusted p-value}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   head(basal.vs.ml)
#'   str(basal.vs.ml)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"basal.vs.ml"

#' cam.BasalvsLP from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{cam.BasalvsLP} from Bioconductor workflow RNAseq123.
#'
#' @format A data frame with 4 rows and 4 columns:
#' \describe{
#'   \item{NGenes}{Number of genes in each term}
#'   \item{Direction}{Direction of the enrichment}
#'   \item{PValue}{Nominal p-value}
#'   \item{FDR}{Multiple-testing adjusted p-value}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   head(cam.BasalvsLP)
#'   str(cam.BasalvsLP)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"cam.BasalvsLP"

#' cam.BasalvsML from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{cam.BasalvsML} from Bioconductor workflow RNAseq123.
#'
#' @format A data frame with 4 rows and 4 columns:
#' \describe{
#'   \item{NGenes}{Number of genes in each term}
#'   \item{Direction}{Direction of the enrichment}
#'   \item{PValue}{Nominal p-value}
#'   \item{FDR}{Multiple-testing adjusted p-value}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   head(cam.BasalvsML)
#'   str(cam.BasalvsML)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"cam.BasalvsML"

#' genes from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{genes} from Bioconductor workflow RNAseq123.
#'
#' @format A data frame with 24 rows and 3 columns:
#' \describe{
#'   \item{ENTREZID}{Entrez ID of mouse gene}
#'   \item{SYMBOL}{Symbol of mouse gene}
#'   \item{TXCHROM}{Chromosome location of mouse gene}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   head(genes)
#'   str(genes)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"genes"

#' group from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{group} from Bioconductor workflow RNAseq123.
#'
#' @format A factor with 3 levels:
#' \describe{
#'   \item{Basal}{Basal cells}
#'   \item{LP}{Luminal progenitor cells}
#'   \item{ML}{Mature luminal cells}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   table(group)
#'   str(group)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"group"

#' lane from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{lane} from Bioconductor workflow RNAseq123.
#'
#' @format A factor with 3 levels:
#' \describe{
#'   \item{L004}{Sample sequenced on lane 4}
#'   \item{L006}{Sample sequenced on lane 6}
#'   \item{L008}{Sample sequenced on lane 8}
#' }
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   table(lane)
#'   str(lane)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"lane"

#' lcpm from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{lcpm} from Bioconductor workflow RNAseq123.
#'
#' @format A matrix with 24 rows and 9 columns
#'
#' @examples
#'   data("RNAseq123", package = "OmicAnalyzer")
#'   head(lcpm)
#'   str(lcpm)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"lcpm"

#' Mm.c2 from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{Mm.c2} from Bioconductor workflow RNAseq123.
#'
#' @format A list of 4 character vectors
#'
#' @examples
#'  data("RNAseq123", package = "OmicAnalyzer")
#'  Mm.c2[[1]]
#'  str(Mm.c2)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"Mm.c2"

#' samplenames from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{samplenames} from Bioconductor workflow RNAseq123.
#'
#' @format A character vector containing the unique sample identifiers
#'
#' @examples
#'  data("RNAseq123", package = "OmicAnalyzer")
#'  head(samplenames)
#'  str(samplenames)
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
"samplenames"
