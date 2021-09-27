library(tidyverse)
library(copula)

# Generación de cópulas
normCopula0.9 <- normalCopula(param = 0.9, dim = 2)
normCopula0.2 <- normalCopula(param = 0.2, dim = 2)

# Plots

contour(normCopula0.9,
        pCopula,
        main = "CDF contour 0.9")
persp(normCopula0.9,
      pCopula,
      main = "CDF 0.9")
contour(normCopula0.9,
        dCopula,
        main = "Density contour 0.9")
persp(normCopula0.9,
      dCopula,
      main = "Density 0.9")

contour(normCopula0.2,
        pCopula,
        main = "CDF contour 0.2")
persp(normCopula0.2,
      pCopula,
      main = "CDF 0.2")
contour(normCopula0.2,
        dCopula,
        main = "Density contour 0.2")
persp(normCopula0.2,
      dCopula,
      main = "Density 0.2")