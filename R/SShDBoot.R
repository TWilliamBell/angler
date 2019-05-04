SShDBoot <- function(Sex, Indices, Coords) { ## Statistic for SShD bootstrap
  SShD <- SShDLM(Coords = Coords[Indices, , drop = F], Sex = Sex[Indices], Zeroed = F)
  SShD
}
