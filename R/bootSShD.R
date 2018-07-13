bootSShD <- function(Coords, Sex, Size = NULL, Zeroed = T) { ## Hoping to make this a hidden function
  Female <- as.character(Sex)=="f"
  Male <- as.character(Sex)=="m" ## If you are sure your sex vector will always be formatted correctly, !Female might be marginally faster
  n <- length(Sex)
  FemaleIndices <- which(Female)
  MaleIndices <- which(Male)
  if (length(MaleIndices) == 0 & length(FemaleIndices) == 0) {
    warning("No individuals found of either sex.")
    return(NA)
  }
  else if (length(MaleIndices) == 0 | length(FemaleIndices) == 0) {
    warning("No individuals found for one sex.")
    return(NA)
  }
  else {
    nF <- sum(Female)
    nM <- sum(Male)
    BootF <- sample(FemaleIndices, size = nF, replace = T)
    BootM <- sample(MaleIndices, size = nM, replace = T)
    CoordArray <- array(dim = dim(Coords))
    CoordArray[1:nF, ] <- Coords[BootF, , drop = F]
    CoordArray[(nF+1):n, ] <- Coords[BootM, , drop = F]
    SexVec <- c(rep('f', nF), rep('m', nM))
    SShD <- SShDLM(CoordArray, SexVec, Zeroed)
    if (!is.null(Size)) { ## If you want to use log instead, one can easily adapt this function for your purposes.
      SizeF <- mean(Size[BootF])
      SizeM <- mean(Size[BootM])
      if (SizeF>=SizeM) {
        SSD <- SizeF/SizeM - 1
      }
      else if (SizeM>SizeF) {
        SSD <- -(SizeM/SizeF - 1)
      }
    return(list(SShD = SShD, SSD = SSD))
    }
    SShD
  }
}