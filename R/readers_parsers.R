#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Import Data from Shimadzu ASCII (.txt) GC-FID, GC-MS, or HPLC-DAD output
#This function was adapted and expanded from package chromConverter by Ethan Bass
#https://rdrr.io/github/ethanbass/chromConverter/src/R/parsers.R
#https://cran.rstudio.com/web/packages/chromConverter/index.html
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Read Shimadzu LabSolutions GC-FID, GC-MS, and HPLC data from ASCII file
#'
#' @description Reads ASCII files originating from Shimadzu LabSolutions and
#' GCMS Solutions software and extracts the chromatogram, peak table, metadata,
#' and MS similarity search table. Modified and extended from function
#' \code{\link[chromConverter]{read_shimadzu}} from the \pkg{chromConverter} package.
#'
#' @param file The \code{character} file path to a compatible Shimadzu ASCII.
#' @param ptable A \code{logical} specifying whether the peak table is extracted. Defaults to \code{TRUE}.
#' @param simtable A \code{logical} specifying whether the GC-MS similarity table is extracted. Only valid when \code{mode = "gcms"}.
#' Defaults to \code{TRUE}.
#' @param pnames A \code{logical} specifying whether peak names are retrieved (where available). \code{TRUE} by default.
#' @param pcas A \code{logical} specifying whether CAS numbers corresponding to peak names are retrieved.
#' Only valid when \code{mode = "gcms"}. \code{TRUE} by default.
#' @param metadata A \code{logical} specifying whether metadata is retrieved (defaults to \code{TRUE}).
#' @param mode A \code{character} value of the type of chromatogram to be read. One of: \code{"fid"} (GC-FID; default),
#' \code{"gcms"} (GC-MS), or \code{"lc"} (HPLC).
#' @param sep,decsep A single \code{character} indicating the \strong{column} and \strong{decimal} (\code{"decsep"}) separator
#' of input ASCII files. The default value \code{"auto"} attempts to determine this automatically.
#' @param fix_names A \code{logical} specifying whether column names should be made syntactically correct (\code{TRUE} by default).
#' @param fil_cols A \code{logical} specifying whether to truncate the peak table to only include the most relevant columns
#' (\code{TRUE} by default).
#' @param cols,chromcols Either \code{"default"} of a \code{character} vector of column names to extract from the \strong{peak table}
#' (\code{cols}) or \strong{chromatogram} (\code{chromcols}).
#' @param rm_dups A \code{logical} specifying whether duplicates should be removed from the GC-MS peak table.
#' Only valid when \code{mode = "gcms"}. Defaults to \code{FALSE} by default.
#' @param unify_cols Either \code{NA} (default) or an \strong{ordered} \code{character} vector of \strong{length 8} containing
#' universal column names to assign for all function output and corresponding to the following variables:
#' peak number, retention time, start time, end time, area, height, area/height ratio, and compound name.
#' @param trange Either \code{NA} (default) or a \code{numeric} vector of length \strong{1} or \strong{2} specifying
#' the \strong{lower} and/or \strong{upper} retention time limit(s) used to truncate the chromatogram and peak table.
#'
#' @return A named \code{list} containing various data extracted from the ASCII file. These include \code{metadata},
#' \code{chromatogram} (one or more), \code{ptable} (peak table), \code{simtable} (MS similarity table), \code{pnames}
#' (a \code{character} vector of peak names), and \code{pcas} (a \code{character} vector of corresponding CAS numbers).
#' When \code{mode = "gcms"} and Selected Ion Monitoring (SIM) chromatograms are present, they are all extracted alongside
#' the Total Ion Current (TIC) chromatogram as separate list elements. Similarly, all wavelength-specific LC chromatograms
#' are extracted separately when \code{mode = "lc"}.
#'
#' @references
#' Bass, E. (2022), \emph{chromConverter}, available at: \url{https://cran.r-project.org/package=chromConverter} (accessed 25.04.2024).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Get data paths of external Shimadzu ASCII data included with lcqc
#' dtp <- ext_lcqc()
#'
#' #Read FID chromatogram
#' fidc <- read_shim(dtp[grep("exgc_",dtp)[1]], mode = "fid")
#'
#' #Read GCMS chromatogram and peak table
#' msc <- read_shim(dtp[grep("exgcms_",dtp)[1]], mode = "gcms")
#'
#' #Read HPLC chromatogram
#' lcc <- read_shim(dtp[grep("exlc_",dtp)[1]], mode = "lc")
#'}
#'
#' @seealso \code{\link{ext_lcqc}}
#'
#' @importFrom utils read.csv
read_shim <- function(file, ptable = TRUE, simtable = TRUE, pnames = TRUE, pcas = TRUE, metadata = TRUE, mode = "fid", sep = "auto", decsep = "auto", fix_names = TRUE,
                      fil_cols = TRUE, cols = "default", chromcols = "default", rm_dups = FALSE, unify_cols = NA, trange = NA) { #"fid", "gcms", "lc"

  #Check for required packages
  #Perform checks and read data
  #if(!all(out_format %in% c("wide", "long"))) stop("Argument 'out_format' must be one of: 'wide' or 'long'!")
  if(!file.exists(file)) {
    stop("The specified file path does not exist... Check the 'file' argument!")
  } else x <- readLines(file)

  if(!any(c("gcms", "fid", "lc") %in% mode)) stop("Argument 'mode' must be one of: 'lc', 'fid', or 'gcms'!")
  if(!any(is.na(unify_cols)) & length(unify_cols)!=8) stop("Argument 'unify_cols' must be an ordered character vector of length 8 corresponding to the following columns in the peak table: peak number, retention time, start time, end time, area, height, area/height, and compound name!")
  if(!any(is.na(trange)) & length(trange)>2) stop("The length of retention time range 'trange' must be no more than 2!")

  #Define colnames for the MS Similarity Results (only when mode == "gcms")
  simcols <- c("Spectrum.", "Hit .", "SI", "CAS .", "Name", "Mol.Weight", "Mol.Form", "Retention Index")

  #Define colnames that go into the Peak Table based on 'mode'
  if(any(cols %in% "default")) {
    cols <- if(mode == "gcms") {
      c("Peak.", "Ret.Time", "Proc.From", "Proc.To", "Area", "Height", "A.H", "Name", "Ret..Index", "Area.", "SI", "CAS..")
    } else if(mode == "fid") {
      c("Peak.", "R.Time", "I.Time", "F.Time", "Area", "Height", "A.H", "Name")
    } else if(mode == "lc") c("Peak.", "R.Time", "I.Time", "F.Time", "Area", "Height", "A.H", "Name")
  }

  #Define identifying tags based on 'mode'
  tags <- list(c(gcms = "\\[MC Peak Table]", fid = "\\[Peak Table", lc = "\\[Peak Table"),
               c(gcms = "\\[MS Chromatogram]", fid = "\\[Chromatogram", lc = "\\[LC Chromatogram|\\[PDA Multi Chromatogram"),
               c(gcms = "# of Points", fid = "# of Points", lc = "# of Points"),
               c(gcms = "Ret.Time", fid = "R.Time", lc = "R.Time"),
               c(gcms = "Name", fid = "Name", lc = "Name"),
               c(gcms = "m/z", fid = "", lc = "Wavelength"))

  tags <- sapply(tags, function(x) x[mode])
  names(tags) <- c("ptab","chrom","pts","rt","pname", "channel_id")
  if(mode=="gcms") {
    tags["simtab"] <- "\\[MS Similarity Search Results for Spectrum Process Table]"
    tags["cas"] <- "CAS.."
  }

  #Define chromatogram colnames when set to 'default'
  if(any(chromcols %in% "default")) {
    chromcols <- list(gcms = c("Ret.Time", "Absolute Intensity", "Relative Intensity"),
                      fid = c("R.Time (min)", "Intensity"),
                      lc = c("R.Time (min)", "Intensity"))
    chromcols <- chromcols[[mode]]
  }

  #Retrieve various variables
  if(sep=="auto") sep <- if(!any(mode %in% "gcms")) substr(x[2], 17, 17) else substr(x[2], 15, 15)
  headings <- grep("\\[*\\]", x)
  peaktab_idx <- grep(tags["ptab"], x)
  chrom_idx <- grep(tags["chrom"], x)
  simtab_idx <- grep(tags["simtab"], x)

  #Vector to store channel names (wavelengths and m/z for LC and GC-MS)
  chanvec <- c()

  #Retrieve chromatogram markers
  if (length(chrom_idx) != 0){

    cat("\nRetrieving chromatogram(s)...")

    #Extract header
    index <- chrom_idx+1
    line <- x[index]
    l <- length(strsplit(x = line, split = sep)[[1]])
    header <- strsplit(x = line, split = sep)
    chromlst <- list()

    for(i in seq_along(header)) {
      curhead <- header[[i]]
      curind <- index[[i]]
      headcols <- c()

      while (l > 1) {
        curind <- curind+1
        line <- strsplit(x = x[curind], split = sep)[[1]]
        l <- length(line)
        if(!any(mode %in% "gcms")) {
          if (l == 1 | suppressWarnings(!is.na(as.numeric(line[1])))) {
            headcols <- curhead[nrow(curhead),]
            curhead <- curhead[-nrow(curhead),]
            curind <- curind-1
            break
          }
        } else {
          if(l == 3 & line[1]==chromcols[1]) {
            headcols <- line
            break
          }
        }
        curhead <- rbind(curhead, line)
      }
      curhead <- list(curhead, headcols, curind)

      met <- curhead[[1]]
      if(decsep=="auto") decsep <- ifelse(grepl(",", met[2,2]),",",".")

      if(identical(sep, decsep)) stop("The decimal separator ('decsep') and text delimiter ('sep') must not be equal!")

      if (decsep == ","){
        met[,2] <- gsub(",", ".", met[,2])
      }

      #Retrieve chromatogram
      nrows <- as.numeric(met[grep(tags["pts"], met[,1]),2])
      if(curhead[[3]]+2+nrows <= length(x)) {
        if(any(length(grep("\\[*\\]", x[curhead[[3]]+c(1,2)+nrows]))>0) | any(x[curhead[[3]]+c(1,2)+nrows]=="")) nrows <- nrows-1
      }
      ncols <- 1

      xx <- read.csv(file, skip = curhead[[3]], sep = sep, colClasses="numeric",
                     row.names = 1, nrows = nrows, dec = decsep)

      xx <- cbind.data.frame(as.numeric(rownames(xx)[!is.na(xx[,1])]),
                             xx[!is.na(xx[,1]),])
      rownames(xx) <- NULL

      #Retrieve extra data from channels (e.g. different Wavelengths from DAD or SIM data from GC-MS)
      if(!any(mode %in% "fid")) {
        channel_id <- met[grep(tags["channel_id"], met[,1]),2]
        channel_id <- if(mode=="gcms") gsub("^(\\d+\\-\\d+ )", "", channel_id) else if(mode=="lc") round(as.numeric(channel_id), 2)
        chanvec <- c(chanvec, channel_id)
        colnames(xx) <- if(mode=="lc") c(tags["rt"], paste0(chromcols[2], " (", channel_id, ")")) else if(mode=="gcms") c(tags["rt"], paste0(chromcols[2:3], " (", channel_id, ")"))
      } else {
        channel_id <- "Chromatogram"
        colnames(xx) <- c(tags["rt"], chromcols[2])
      }
      chromlst[[i]] <- xx
      names(chromlst)[i] <- channel_id

      #Truncate chromatogram(s) according to 'trange'
      if(!all(is.na(trange))) {
        if(length(trange)==1) trange[2] <- max(chromlst[[i]][,tags["rt"]], na.rm = TRUE)
        chromlst[[i]] <- chromlst[[i]][chromlst[[i]][,tags["rt"]] >= trange[1] & chromlst[[i]][,tags["rt"]] <= trange[2],]
      }

      #Make column names syntactically correct
      if(fix_names) colnames(chromlst[[i]]) <- gsub("^_|_$", "", gsub("\\.+", "_", make.names(colnames(chromlst[[i]]))))
    }

    #Remove duplicates from chromatograms (by name)
    chromlst <- chromlst[!duplicated(names(chromlst))]

    #Merge chromatograms by retention time
    #my_merge <- function(df1, df2, byval = tags["rt"]) { # Create own merging function
    #  merge(df1, df2, by = byval, all = TRUE)
    #}
    #xx <- Reduce(my_merge, chromlst)

  } else {
    if (!ptable & !simtable) stop("Chromatogram not found!") else warning("Chromatogram not found!")
    chromlst <- "No chromatogram was found."
  }

  #Extract peak table (optionally)
  if(ptable) {
    if (length(peaktab_idx) != 0) {

      cat("\nRetrieving peak table(s)...")

      pind <- if(mode=="gcms") 2:4 else 1:3

      peak_tab <- lapply(peaktab_idx, function(idx){
        nrows <- as.numeric(strsplit(x = x[idx+1], split = sep)[[1]][2])
        if (!is.na(nrows) && nrows > 0){
          time_column <- grep(tags["rt"], strsplit(x = x[[idx+pind[2]]], split = sep)[[1]])
          t1 <- strsplit(x = x[[idx+pind[3]]], split = sep)[[1]][time_column]
          decsep <- ifelse(grepl(".", t1), ".", ",")

          peak_tab <- read.csv(file, skip = idx+pind[1], sep = sep, nrows = nrows,
                               dec = decsep)
        } else NA
      })

      #Assign names to the peak table
      if(length(chanvec)>0 & length(chanvec)==length(peak_tab)) {
        names(peak_tab) <- paste0(gsub("\\[|\\]","", x[peaktab_idx]), ": ", chanvec, if(mode=="gcms") " m/z" else if(mode=="lc") " nm")
      } else names(peak_tab) <- gsub("\\[|\\]","", x[peaktab_idx])
      #peak_tab <- peak_tab[[1]]

      for(i in seq_along(peak_tab)) {
        curptab <- peak_tab[[i]]

        #Flag items in the peak table where Compound Name is not unknown but CAS is not available
        if(mode=="gcms") {
          curptab[,"CAS_Flag"] <- ifelse((curptab[,tags["pname"]]!="Unknown" | nchar(curptab[,tags["pname"]])>0) & curptab[,grep("CAS", colnames(curptab))]=="0-00-0", 1, 0)
        }

        #Optionally filter peak table columns to leave only the relevant ones
        if(fil_cols) {
          cat("\nFiltering peak table columns to leave the most relevant ones...")
          curptab <- curptab[,cols]
        }

        #Change any empty compound names to "Unknown" and/or take care of duplicated compound names/identifications
        if(mode=="gcms") {
          if(rm_dups) {
            cat("\nRemoving duplicate compound names/identifications...")
            dupchk <- (duplicated(curptab[,tags["pname"]]) | duplicated(curptab[,tags["pname"]], fromLast = TRUE))
            empty_length <- length(which(dupchk))
            curptab[dupchk, c(tags["pname"], "SI", "CAS..")] <- list(rep("Unknown", empty_length),
                                                                     rep(0, empty_length),
                                                                     rep("0-00-0", empty_length))
          } #else {
          #dupchk <- nchar(curptab[,tags["pname"]])==0
          #empty_length <- length(which(dupchk))
          #curptab[dupchk, c(tags["pname"],"SI")] <- list(rep("Unknown", empty_length),
          #                                                rep(0, empty_length))
          #}
        }

        #(Optionally) retrieve peak names and/or CAS numbers
        peak_names <- if(pnames) curptab[,tags["pname"]] else "Peak names were not retrieved."
        peak_cas <- if(pcas & mode=="gcms") curptab[,tags["cas"]] else "Peak CAS numbers were not retrieved."

        #Truncate peak table according to 'trange'
        if(!all(is.na(trange))) {
          if(length(trange)==1) trange[2] <- max(curptab[,tags["rt"]], na.rm = TRUE)
          curptab <- curptab[curptab[,tags["rt"]] >= trange[1] & curptab[,tags["rt"]] <= trange[2],]
        }

        #Adjust column names to be "prettier"
        colnames(curptab)[grep("Area.", colnames(curptab))] <- "Area_Percent"
        colnames(curptab) <- gsub(" ", "_", gsub("\\.", "", colnames(curptab)))

        #Make column names syntactically correct
        if(fix_names) colnames(curptab) <- gsub("^_|_$", "", gsub("\\.+", "_", make.names(colnames(curptab))))

        #Calculate Area_Percentages where none are present
        if(!any(colnames(curptab) %in% "Area_Percent")) {
          cat("\nCalculating peak area percentages of all peaks via standard Area Normalisation...")
          curptab[,"Area_Percent"] <- curptab[,grep("^Area$", colnames(curptab))]/sum(curptab[,grep("Area", colnames(curptab))], na.rm = TRUE)*100
        }
        peak_tab[[i]] <- curptab
      }

    } else {
      if(!simtable) stop("Peak table not found!") else warning("Peak table not found!")
      peak_tab <- peak_names <- "No peak table was found."
    }
  } else peak_tab <- peak_names <- "The peak table was not retrieved."

  #Retrieve MS similarity search table
  if(simtable & mode == "gcms" & length(simtab_idx)>0) {

    cat("\nRetrieving MS Similary Search Results table...")

    #Retrieve first correct line of MS Similarity table
    simind <- simtab_idx+1
    nrows <- as.numeric(strsplit(x = x[simind], split = sep)[[1]][2])

    #Double check that the heading index+2 is not the chromatogram
    if(any(tags["chrom"] %in% x[simind+2]) | simind+2 == chrom_idx[1]) nrows <- 1

    sim_tab <- read.csv(file, skip = simind, sep = sep, dec = decsep)

    #Filter out irrelevant columns
    sim_tab <- sim_tab[,1:7]

    #Filter out irrelevant rows
    sim_tab <- sim_tab[sim_tab[,1] %in% 1:nrows,]

    if(nrow(sim_tab) > 0) { #Only carry out the following operations if the Similarity Table is NOT empty!
      #Delete spaces in CAS numbers and Molecular Formulae
      sim_tab[,grep("CAS", colnames(sim_tab))] <- gsub(" ", "", sim_tab[,grep("CAS", colnames(sim_tab))])
      sim_tab[,grep("Mol.Form", colnames(sim_tab))] <- gsub(" ", "", sim_tab[,grep("Mol.Form", colnames(sim_tab))])

      #Delete synonyms to compound names, only leaving the first name variant
      sim_tab[,tags["pname"]] <- sapply(strsplit(sim_tab[,tags["pname"]], " \\$\\$ "), function(x) x[1])

      #If peak table was also retrieved, confirm CAS numbers and SI values while adding Molecular Weight (MolWeight) and Formulae (MolForm)
      if(is.data.frame(peak_tab)) {

        cat("\nConfirming correctness of CAS and other identifiers in the Peak Table...")

        peak_tab[,c("MolWeight", "MolForm")] <- NA

        #Define relevant columns
        colvec <- c("Peak", "SI", "CAS", "Name", "Spectrum", "Weight", "Form")
        sc <- sapply(colvec, function(x) grep(paste0(".*", x, ".*"), colnames(sim_tab)))
        pc <- sapply(colvec, function(x) grep(paste0(".*", x, ".*"), colnames(peak_tab)))
        names(sc) <- names(pc) <- c("pk", "si", "cas", "pname", "spec", "mw", "mf")
        sc <- unlist(sc[lapply(sc,length)>0])
        pc <- unlist(pc[lapply(pc,length)>0])

        #Identify target rows (where non-duplicated compound identifications are present)
        tarows <- which(!(peak_tab[,tags["pname"]] %in% "Unknown" | nchar(peak_tab[,tags["pname"]])==0))

        #Loop through the identified rows
        for(i in tarows) {
          pnum <- peak_tab[i, pc["pk"]] #chromcols[1]
          pnam <- peak_tab[i,pc["pname"]]
          p_si <- peak_tab[i,pc["si"]]
          p_cas <- peak_tab[i,pc["cas"]]
          specs <- sim_tab[sim_tab[,sc["spec"]] %in% pnum,]
          whichit <- which(specs[,sc["pname"]] %in% pnam)[1]
          if(length(whichit)>0) {
            spechit <- specs[whichit,]
            peak_tab[i,pc[c("si","cas","mw","mf")]] <- spechit[,sc[c("si","cas","mw","mf")]]
          }
        }
      }

      #Adjust column names to be "prettier"
      colnames(sim_tab) <- gsub(" ", "_", gsub("\\.", "", colnames(sim_tab)))

      #Make column names syntactically correct
      if(fix_names) colnames(sim_tab) <- gsub("^_|_$", "", gsub("\\.+", "_", make.names(colnames(sim_tab))))
    }
  } else sim_tab <- "MS Similarity Table was not retrieved."

  #Unify common column names (useful when two or more chromatograms from different instruments/software are to be compared later...)
  if(!any(is.na(unify_cols))) {
    cat("\nAssigning universal column names for key variables... see 'unify_cols' argument.")
    if(!is.list(peak_tab) & is.data.frame(peak_tab)) {
      peak_tab <- list(peak_tab)
    }
    if(all(sapply(peak_tab, is.data.frame))) peak_tab <- lapply(peak_tab, function(x) {
      colnames(x)[1:8] <- unify_cols
      return(x)
    })
    if(is.data.frame(chromlst[[1]])) {
      chromnm_bckp <- names(chromlst)
      chromlst <- lapply(seq_along(chromlst), function(x) setNames(chromlst[[x]], replace(colnames(chromlst[[x]]), 1, unify_cols[2])))
      names(chromlst) <- chromnm_bckp
    }
  }

  #Read/extract metadata
  if(metadata){

    cat("\nRetrieving metadata...")

    idx <-  which(x[headings] %in%
                    c("[Header]", "[File Information]", "[Sample Information]",
                      "[Original Files]", "[File Description]", "[Configuration]") #"[Configuration]" optional, not present in GC-MS ASCII
    )
    meta_start <- headings[min(idx)]
    meta_end <- headings[max(idx) + 1]
    meta <- x[(meta_start+1):(meta_end-1)]
    meta <- meta[meta!=""]
    meta <- meta[-grep("\\[", meta)]
    meta <- stringr::str_split_fixed(meta, pattern = sep, n = 2)
    if (exists("met")){
      meta <- rbind(meta, met)
    }
    rownames(meta) <- meta[, 1]
    meta <- as.list(meta[,2])

  } else meta <- "Metadata was not retrieved."

  return(list(metadata = meta, chromatogram = chromlst, ptable = peak_tab, simtable = sim_tab, pnames = peak_names, pcas = peak_cas))
}
