SSDLM <- function(Size, Sex) {
  Data <- data.frame(Size, Sex)
  DiffFM <- lm(Size ~ Sex, Data)$coef[2]
  FSize <- mean(Size[Sex == 'f'])
  MSize <- mean(Size[Sex == 'm'])
  if (FSize >= MSize & DiffFM >= 0) {
    MSize <- FSize - DiffFM
    SSD <- FSize/MSize - 1
  }
  else if (FSize >= MSize & DiffFM < 0) {
    MSize <- FSize + DiffFM
    SSD <- FSize/MSize - 1
  }
  else if (MSize >= FSize & DiffFM >= 0) {
    MSize <- FSize + DiffFM
    SSD <- -(MSize/FSize - 1)
  }
  else if (MSize >= FSize & DiffFM < 0) {
    MSize <- FSize - DiffFM
    SSD <- -(MSize/FSize - 1)
  }
  else {warning("Something went wrong when calculating SSD.  Is data formatted correctly (see ConvertSexFormat())? Does the data have both 'm' and 'f' individuals?")}
  unname(SSD)
}