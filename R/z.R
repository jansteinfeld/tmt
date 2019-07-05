.onAttach <- function(libname,pkgname){
  p <- utils::packageDescription("tmt")
  packageStartupMessage(
    paste("- " , p$Package," " , p$Version ," (",p$Date,")",sep="")  )
}


# version <- function(pkg="tmt"){
#   # lib <- dirname( system.file(package = pkg))
#   p <- utils::packageDescription(pkg)
#   #return( paste(p$Package,p$Version,p$Date,lib))
#   return( paste(p$Package,p$Version,p$Date))
# }
