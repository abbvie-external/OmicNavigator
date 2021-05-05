# This file contains the documentation for the example objects from the User's
# Guide.
#
# Created by scripts/RNAseq123.R
# Saved in data/RNAseq123.RData
# Used by vignettes/OmicNavigatorUsersGuide.Rnw

#' Shared sections for data objects
#'
#' @name shared-data
#'
#' @source \url{https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/limmaWorkflow.html}
#'
#' @references
#'   Law CW, Alhamdoosh M, Su S, Dong X, Tian L, Smyth GK, Ritchie ME.
#'   \href{https://f1000research.com/articles/5-1408/v3}{RNA-seq analysis is easy as 1-2-3 with limma, Glimma and edgeR [version 3; peer review: 3 approved].}
#'   F1000Research 2018, 5:1408
#'   \doi{10.12688/f1000research.9005.3}
#'
#'   Sheridan, J.M., Ritchie, M.E., Best, S.A. et al.
#'   \href{https://bmccancer.biomedcentral.com/articles/10.1186/s12885-015-1187-z}{A pooled shRNA screen for regulators of primary mammary stem and progenitor cells identifies roles for \emph{Asap1} and \emph{Prox1}.}
#'   BMC Cancer 2015, 15:221
#'   \doi{10.1186/s12885-015-1187-z}
#'
#' @keywords internal
NULL

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
#'   head(basal.vs.lp)
#'   str(basal.vs.lp)
#'
#' @inherit shared-data source
#' @inherit shared-data references
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
#'   head(basal.vs.ml)
#'   str(basal.vs.ml)
#'
#' @inherit shared-data source
#' @inherit shared-data references
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
#'   head(cam.BasalvsLP)
#'   str(cam.BasalvsLP)
#'
#' @inherit shared-data source
#' @inherit shared-data references
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
#'   head(cam.BasalvsML)
#'   str(cam.BasalvsML)
#'
#' @inherit shared-data source
#' @inherit shared-data references
"cam.BasalvsML"

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
#'   table(group)
#'   str(group)
#'
#' @inherit shared-data source
#' @inherit shared-data references
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
#'   table(lane)
#'   str(lane)
#'
#' @inherit shared-data source
#' @inherit shared-data references
"lane"

#' lcpm from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{lcpm} from Bioconductor workflow RNAseq123.
#'
#' @format A matrix with 24 rows and 9 columns
#'
#' @examples
#'   head(lcpm)
#'   str(lcpm)
#'
#' @inherit shared-data source
#' @inherit shared-data references
"lcpm"

#' Mm.c2 from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{Mm.c2} from Bioconductor workflow RNAseq123.
#'
#' @format A list of 4 character vectors
#'
#' @examples
#'   Mm.c2[[1]]
#'   str(Mm.c2)
#'
#' @inherit shared-data source
#' @inherit shared-data references
"Mm.c2"

#' samplenames from Bioconductor workflow RNAseq123
#'
#' A subset of the object \code{samplenames} from Bioconductor workflow RNAseq123.
#'
#' @format A character vector containing the unique sample identifiers
#'
#' @examples
#'   head(samplenames)
#'   str(samplenames)
#'
#' @inherit shared-data source
#' @inherit shared-data references
"samplenames"
