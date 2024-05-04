#'
bumpVersion <- function(ver='0.0.0.0000', which='dev'){
  v=as.integer(strsplit(ver, split = '.', fixed = T)[[1]])
  v <- switch(which,
              major={c(v[1]+1, 0, 0, 0)},
              minor={c(v[1], v[2]+1, 0, 0)},
              patch={c(v[1:2], v[3]+1, 0)},
              dev={c(v[1:3], v[4]+1)})
  sprintf('%d.%d.%d.%04d', v[1], v[2], v[3], v[4])
}

#' @param bumpVestion determines the part of version to increase:
#'    "major"."minor"."patch"."dev"
build_this <- function(bumpVersion='dev', BothBinary=TRUE, addSource=FALSE){
  if(bumpVersion!='')
    bumpVersion = match.arg(tolower(bumpVersion), c("major", "minor", "patch", "dev"))
  cat('Read Description ...\n')
  # Description
  desc <- read.dcf("DESCRIPTION")#
  oldDesc <- desc
  #
  PRJ_Name <- desc[1, "Package"]
  # Bump ver
  if(bumpVersion %in% c("major", "minor", "patch", "dev")){
    desc[1, "Version"] <- bumpVersion(desc[1, "Version"], bumpVersion)
  }
  LIB_Name <- paste0(PRJ_Name, '_', desc[1, "Version"])
  cat('Write New Description ...\n', LIB_Name, '\n')
  write.dcf(desc, "DESCRIPTION")

  tryCatch({
    cat('Rebuild the Documentation ...\n')
    # Documentation
    devtools::document()
    cat('Build the Documentation ... Completed!\n')
    # Source
    if(addSource){
      cat('Build the Source ...\n')
      devtools::build(path = './build', binary = FALSE, vignettes = TRUE)
      cat('Build the Source ... Completed!\n')
    }
    # Binary:
    cat('Build the Binary ...\n')
    devtools::build(path = './build', binary = TRUE, vignettes = TRUE)
    cat('Build the Binary ... Completed!\n')

    zipName <- paste0(getwd(), '/build/', LIB_Name, '.zip') #file.choose()
    tgzName <- paste0(getwd(), '/build/', LIB_Name, '.tgz') #file.choose()
    # If windows, create tgz format
    if(BothBinary & .Platform$OS.type=='windows'){
      cat('Build the Binary for Windows ...\n')

      Sys.setenv(PATH=paste0(
        "C:\\SharedDocs\\APPS\\7z64;", Sys.getenv("PATH")
      ))
      # choose the binary version (.zip)
      zips <- paste0(getwd(), '/build/', LIB_Name, '.zip') #file.choose()
      system2('7z', paste0(' x ', zipName), stdout = "out.txt")
      system2('7z', paste0('a -ttar ', PRJ_Name, ' ', PRJ_Name), stdout = "out.txt")
      system2('7z', paste0('a ', LIB_Name, '.tgz ', PRJ_Name, '.tar'), stdout = "out.txt")
      unlink(paste0(PRJ_Name, '.tar'))
      unlink(PRJ_Name, recursive = TRUE)
      file.rename(from = paste0(LIB_Name, '.tgz'), to = paste0('build/', LIB_Name, '.tgz'))
      cat('Build the Binary for Mac ... Completed!\n')
    } else if(BothBinary & Sys.info()["sysname"]=='Darwin'){
      # Creating Zip file from tgz in Mac
      cat('Build the Binary for Windows ...\n')
      system2('tar', paste0('-xzf ', tgzName))
      system2('zip', paste0('-r ', zipName, ' ', PRJ_Name))
      unlink(PRJ_Name, recursive = TRUE)
      #system2('bsdtar -cvf ', zipName, ' ', PRJ_Name)
      ## or Just uncompress and compress in finder

    }
  }, error = function(e){
    cat('Error! Return to prior version ...\n')
    write.dcf(oldDesc, "DESCRIPTION")
    print(e)
  })
}
#build_this(bumpVersion = 'min', BothBinary = TRUE)
build_this(bumpVersion = '', BothBinary = TRUE)
#build_this()
