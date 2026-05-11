# Set Up ----------------------------------------------------------------------

pacman::p_load(
  data.table, tidyverse, foreach, rneos, doParallel
)

## Define functions -----------------------------------------------------------

my_savematch <- function(x, set, include = TRUE) {
  if(include == TRUE) {
    tmp <- x[which(x %in% set)]
  } else if(include == FALSE) {
    tmp <- x[which(!(x %in% set))]
  }
  
  return(tmp)
}

options(RCurlOptions = list(ssl.verifypeer = FALSE))

my_NgetSolverTemplate <- function (
    category, solvername, inputMethod, nc = CreateNeosComm()
) {
  if (!(class(nc) == "NeosComm")) {
    stop("\nObject provided for 'nc' must be of class 'NeosComm'.\n")
  }
  
  call <- match.call()
  
  ans <- xml.rpc(
    url = nc@url, 
    method = "getSolverTemplate", 
    .args = list(
      category = category, solvername = solvername, inputMethod = inputMethod
    ), 
    .convert = TRUE, 
    .opts = nc@curlopts, 
    .curl = nc@curlhandle
  )
  
  email.xmlcode <- "\n\n<email><![CDATA[...Insert Value Here...]]></email>"
  
  ans_rev <- if(
    grepl("email", ans)
  ) {
    ans
  } else {
    paste0(
      substr(ans, 1, nchar(ans)-13), 
      email.xmlcode, 
      substr(ans, (nchar(ans)-13), nchar(ans))
    )
  }
  
  xml <- xmlRoot(xmlTreeParse(ans_rev, asText = TRUE))
  
  res <- new("NeosXml", xml = xml, method = "getSolverTemplate",
             call = call, nc = nc)
  
  return(res)
}

## Send AMPL jobs to NEOS -----------------------------------------------------

tmp <-my_NgetSolverTemplate(
  category = "lp", solvername = "CPLEX", inputMethod = "AMPL"
)
## setting path to model and data files
if(exists("IncludeUncertainty")) {
  if(IncludeUncertainty == TRUE) {
    cmdf <- paste0(OutPath,  "STcommand_dynamic_uncertainty.amp")
    modf <- paste0(OutPath,  "STmodel_dynamic_uncertainty.mod")
    datf <- paste0(OutPath, "STdata_dynamic_uncertainty.dat")
  } else if(IncludeUncertainty == FALSE) {
    cmdf <- paste0(OutPath,  "STcommand_seasonal.amp")
    modf <- paste0(OutPath,  "STmodel_seasonal.mod")
    datf <- paste0(OutPath, "STdata_seasonal.dat")
  } else {
    print(
      "Only allowable options are 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'. You cannot have both, only one or the other."
    )
  }
} else {
  print(
    "Did you delete the line of code that says 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'?"
  )
}


## import file contents
comc <- paste(paste(readLines(cmdf), collapse = "\n"), "\n")
modc <- paste(paste(readLines(modf), collapse = "\n"), "\n")
datc <- paste(paste(readLines(datf), collapse = "\n"), "\n")

# Submit request to NEOS and retrieve results
  
options(RCurlOptions = list(ssl.verifypeer = FALSE))
  
## create list object
argslist <- list(
  model = modc, data = datc, commands = comc, comments = comments, email = email
)

## create XML string
xmls <- CreateXmlString(neosxml = tmp, cdatalist = argslist)
  
## Submit job
job.xml.text <- NsubmitJob(
    xmlstring = xmls, user = "rneos", interface = "", id = 0
)

## Job info - ND added
print("jobnumber = ")
print(job.xml.text@jobnumber)
print("password = ")
print(job.xml.text@password)

## Retrieve results
resultfile <- NgetFinalResults(obj = job.xml.text, convert = TRUE)

writeLines(
#  resultfile@ans, con = paste0(working_dir, NEOSresults,"NEOSresult_", paramname, ".txt")
# Changed to format for PS program
  resultfile@ans, con = paste0(NEOSresults,"NEOSresult_", paramname, ".txt")  
)

print("NEOS results have been saved to the output folder.")
