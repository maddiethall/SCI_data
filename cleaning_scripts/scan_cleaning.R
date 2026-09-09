library(dplyr)

AAS = YokeAAS_raw
AAS = AAS[-(80:91), ]

East_Road = EastRoad_raw
TLACJ = YokeTLACJ_raw
Windmill = Windmill_raw
Yankee = YankeeBridge_raw

aas_proximity_cols <- names(AAS)[grepl(" [1-8]$", names(AAS))]
east_proximity_cols <- names(East_Road)[grepl(" [1-8]$", names(East_Road))]
tlacj_proximity_cols <- names(TLACJ)[grepl(" [1-8]$", names(TLACJ))]
yankee_proximity_cols <- names(Yankee)[grepl(" [1-8]$", names(Yankee))]
windmill_proximity_cols <- names(Windmill)[grepl(" [1-8]$", names(Windmill))]

Yankee <- Yankee %>%
  mutate(
    `MAY 1` = if_else(
      Focal == "May Lillie" &
        `Date-Time` == "06/01/2026 10:16 AM",
      NA_character_,
      `MAY 1`
    )
  )


AAS_activity <- AAS %>%
  select(Focal, `Date-Time`, `1`:`8`) %>%
  pivot_longer(
    cols = `1`:`8`,
    names_to = "scan",
    values_to = "activity"
  )
AAS_proximity <- AAS %>%
  select(Focal, `Date-Time`, `AST 1`:`SEL 8`) %>%
  pivot_longer(
    cols = `AST 1`:`SEL 8`,
    names_to = "partner_scan",
    values_to = "proximity"
  )
AAS_proximity <- AAS_proximity %>%
  separate(
    partner_scan,
    into = c("partner_id", "scan"),
    sep = " "
  )
AAS_proximity <- AAS_proximity %>%
  filter(
    !(Focal == "Selene" & partner_id == "SEL"),
    !(Focal == "Ash" & partner_id == "ASH"),
    !(Focal == "Asteria" & partner_id == "AST")
  )
AAS_scan <- AAS_proximity %>%
  left_join(
    AAS_activity,
    by = c("Focal", "Date-Time", "scan")
  )
AAS_scan <- AAS_scan %>%
  mutate(
    focal_id = case_when(
      Focal == "Asteria" ~ "AST",
      Focal == "Ash" ~ "ASH",
      Focal == "Selene" ~ "SEL"
    )
  )
saveRDS(AAS_scan, "clean_data/AAS_scan.rds")







East_Road_activity <- East_Road %>%
  select(Focal, `Date-Time`, `1`:`8`) %>%
  pivot_longer(
    cols = `1`:`8`,
    names_to = "scan",
    values_to = "activity"
  )
East_Road_proximity <- East_Road %>%
  select(Focal, `Date-Time`, `DEL 1`:`JJO 8`) %>%
  pivot_longer(
    cols = `DEL 1`:`JJO 8`,
    names_to = "partner_scan",
    values_to = "proximity"
  )
East_Road_proximity <- East_Road_proximity %>%
  separate(
    partner_scan,
    into = c("partner_id", "scan"),
    sep = " "
  )
East_Road_proximity <- East_Road_proximity %>%
  filter(
    !(Focal == "Delilah" & partner_id == "DEL"),
    !(Focal == "Janis Joplin" & partner_id == "JJO"),
    !(Focal == "Laurel" & partner_id == "LAU"),
    !(Focal == "Pearl Hart" & partner_id == "PHA")
  )
East_Road_scan <- East_Road_proximity %>%
  left_join(
    East_Road_activity,
    by = c("Focal", "Date-Time", "scan")
  )
East_Road_scan <- East_Road_scan %>%
  mutate(
    focal_id = case_when(
      Focal == "Delilah" ~ "DEL",
      Focal == "Janis Joplin" ~ "JJO",
      Focal == "Laurel" ~ "LAU",
      Focal == "Pearl Hart" ~ "PHA"
    )
  )
saveRDS(East_Road_scan, "clean_data/East_Road_scan.rds")




TLACJ_activity <- TLACJ %>%
  select(Focal, `Date-Time`, `1`:`8`) %>%
  pivot_longer(
    cols = `1`:`8`,
    names_to = "scan",
    values_to = "activity"
  )
TLACJ_proximity <- TLACJ %>%
  select(Focal, `Date-Time`, `TLY 1`:`CJA 8`) %>%
  pivot_longer(
    cols = `TLY 1`:`CJA 8`,
    names_to = "partner_scan",
    values_to = "proximity"
  )
TLACJ_proximity <- TLACJ_proximity %>%
  separate(
    partner_scan,
    into = c("partner_id", "scan"),
    sep = " "
  )
TLACJ_proximity <- TLACJ_proximity %>%
  filter(
    !(Focal == "Artemis" & partner_id == "ART"),
    !(Focal == "Calamity Jane" & partner_id == "CJA"),
    !(Focal == "Terri-Lynn" & partner_id == "TLY")
  )
TLACJ_proximity <- TLACJ_proximity %>%
  mutate(
    focal_id = case_when(
      Focal == "Artemis" ~ "ART",
      Focal == "Calamity Jane" ~ "CJA",
      Focal == "Terri-Lynn" ~ "TLY"
    )
  )
TLACJ_scan <- TLACJ_proximity %>%
  left_join(
    TLACJ_activity,
    by = c("Focal", "Date-Time", "scan")
  )
saveRDS(TLACJ_scan, "clean_data/TLACJ_scan.rds")



Windmill <- Windmill %>%
  filter(
    !(`Focal` == "Polli" &
        `Date-Time` == "06/06/2026 09:55 AM" &
        `Added Time` == "06/07/2026 11:57:29"),
    !(`Focal` == "Chagall" &
        `Date-Time` == "07/10/2026 11:00 AM" &
        `Added Time` == "07/10/2026 11:53:22")
  )
Windmill_activity <- Windmill %>%
  select(Focal, `Date-Time`, `1`:`8`) %>%
  pivot_longer(
    cols = `1`:`8`,
    names_to = "scan",
    values_to = "activity"
  )
Windmill_proximity <- Windmill %>%
  select(Focal, `Date-Time`, `AUT 1`:`TCH 8`) %>%
  pivot_longer(
    cols = `AUT 1`:`TCH 8`,
    names_to = "partner_scan",
    values_to = "proximity"
  )
Windmill_proximity <- Windmill %>%
  select(Focal, `Date-Time`, `AUT 1`:`TCH 8`) %>%
  pivot_longer(
    cols = `AUT 1`:`TCH 8`,
    names_to = "partner_scan",
    values_to = "proximity"
  )
Windmill_proximity <- Windmill_proximity %>%
  separate(
    partner_scan,
    into = c("partner_id", "scan"),
    sep = " "
  )
Windmill_proximity <- Windmill_proximity %>%
  filter(
    !(Focal == "Autumn" & partner_id == "AUT"),
    !(Focal == "Chagall" & partner_id == "CHA"),
    !(Focal == "Polli" & partner_id == "POL"),
    !(Focal == "Tracy Champan" & partner_id == "TCH")
  ) %>%
  mutate(
    focal_id = case_when(
      Focal == "Autumn" ~ "AUT",
      Focal == "Chagall" ~ "CHA",
      Focal == "Polli" ~ "POL",
      Focal == "Tracy Champan" ~ "TCH"
    )
  )
Windmill_scan <- Windmill_proximity %>%
  left_join(
    Windmill_activity,
    by = c("Focal", "Date-Time", "scan")
  )
saveRDS(Windmill_scan, "clean_data/Windmill_scan.rds")



Yankee_activity <- Yankee %>%
  select(Focal, `Date-Time`, `1`:`8`) %>%
  pivot_longer(
    cols = `1`:`8`,
    names_to = "scan",
    values_to = "activity"
  )
Yankee_proximity <- Yankee %>%
  select(
    Focal,
    `Date-Time`,
    `MAR 1`:`MAR 8`,
    `MAY 1`:`MAY 8`,
    `DDA 1`:`DDA 8`
  ) %>%
  pivot_longer(
    cols = -c(Focal, `Date-Time`),
    names_to = "partner_scan",
    values_to = "proximity"
  ) %>%
  separate(
    partner_scan,
    into = c("partner_id", "scan"),
    sep = " "
  )
Yankee_proximity <- Yankee_proximity %>%
  filter(
    !(Focal == "Delta Dawn" & partner_id == "DDA"),
    !(Focal == "Marla" & partner_id == "MAR"),
    !(Focal == "May Lillie" & partner_id == "MAY")
  ) %>%
  mutate(
    focal_id = case_when(
      Focal == "Delta Dawn" ~ "DDA",
      Focal == "Marla" ~ "MAR",
      Focal == "May Lillie" ~ "MAY"
    )
  )
Yankee_scan <- Yankee_proximity %>%
  left_join(
    Yankee_activity,
    by = c("Focal", "Date-Time", "scan")
  )
saveRDS(Yankee_scan, "clean_data/Yankee_scan.rds")




# adding troop identifiers
AAS_scan <- AAS_scan %>%
  mutate(troop = "AAS")
East_Road_scan <- East_Road_scan %>%
  mutate(troop = "East Road")
TLACJ_scan <- TLACJ_scan %>%
  mutate(troop = "TLACJ")
Windmill_scan <- Windmill_scan %>%
  mutate(troop = "Windmill")
Yankee_scan <- Yankee_scan %>%
  mutate(troop = "Yankee")



# combining datasets
lemur_scan <- bind_rows(
  AAS_scan,
  East_Road_scan,
  TLACJ_scan,
  Windmill_scan,
  Yankee_scan
)
saveRDS(lemur_scan, "clean_data/lemur_scan.rds")
