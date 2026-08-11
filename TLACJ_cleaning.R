library(dplyr)

yoke_TLACJ = read.csv('/Users/maddiethall/R Repos/SCI_data/YokeTLACJ.csv')

TLY_proximity = yoke_TLACJ %>%
  filter(Focal == "Terri-Lynn") %>%
  select(Focal, ART.1, ART.2, ART.3, ART.4, ART.5, ART.6, ART.7, ART.8,
         CJA.1, CJA.2, CJA.3, CJA.4, CJA.5, CJA.6, CJA.7, CJA.8)

ART_proximity = yoke_TLACJ %>%
  filter(Focal == "Artemis") %>%
  select(Focal, TLY.1, TLY.2, TLY.3, TLY.4, TLY.5, TLY.6, TLY.7, TLY.8,
         CJA.1, CJA.2, CJA.3, CJA.4, CJA.5, CJA.6, CJA.7, CJA.8)

CJA_proximity = yoke_TLACJ %>%
  filter(Focal == "Calamity Jane") %>%
  select(Focal, TLY.1, TLY.2, TLY.3, TLY.4, TLY.5, TLY.6, TLY.7, TLY.8,
         ART.1, ART.2, ART.3, ART.4, ART.5, ART.6, ART.7, ART.8)
