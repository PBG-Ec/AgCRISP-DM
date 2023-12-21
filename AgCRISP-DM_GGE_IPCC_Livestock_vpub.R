###############################################################################
# Adapting CRISP-DM to Model Enteric Fermentation Emission: ###################
# Farm Level Application ######################################################
###############################################################################
pks <- c("tidyverse", "car")
lapply(pks, require, character.only = TRUE)
###############################################################################
####### I. HERD MODULE : Typology Mean weight Tier2 param Milk avg#############
###############################################################################
###### Farm db: cattle breeding farms #########################################
# Farm types db: cattle breeding class (clau) + Prod Sys Type
qw <- readRDS("./dbb/qori_atxt/qw.rds")
qw <- qw[!duplicated(qw[, c("idsto", "yr")]), ] # filter duplicates
qw <- qw[qw$sum_ganad != 0, ] # Filter out non cattle farms
qw <- qw[qw$idsto != "", ] # Filter out non identified farms

## BUILD GLEAM EC  Parameters Mean protein produced Mean reference weights
# Nomenclature: _m:mean weight _s:mean weight at sell, _awg:mean average gain
# From National sample data: keep "EMPRESARIAL"
inac <- read.csv("./BreedingSyst/data/input_national_data.csv")
names(inac) <- tolower(names(inac))
inac$m_ternera <- inac$ckg
inac$m_vaca_d <- NA
inac$afc_yr <- inac$afc_months / 12
inac$llac_day <- inac$lact_per_months * 30.4167
inac <- inac[, c(
  "region", "estrato", "producto", "afkg", "amkg", "ckg", "m_ternera",
  "mfskg", "mmskg", "m_vaca_d", "milk_yield", "llac_day",
  "milk_fat", "milk_protein", "afc_yr"
)]
inac <- inac[order(inac$region, inac$producto, inac$estrato), ]
names(inac) <- c(
  "region", "clauclass", "producto", "m_vaca_m", "m_toro_m", "m_ternero_m",
  "m_ternera_m", "m_vacona_s", "m_novillo_s", "m_vaca_s",
  "lpv", "llac_day", "fat", "prot", "afc_yr"
)
# Out lier to impute with lower value (MERCANTIL)
inac$m_vaca_m[inac$region == "COSTA" &
  inac$clauclass == "EMPRESARIAL" &
  inac$producto == "Leche"] <- NA
# From National indepht sample data: keep "MARGINAL" & "
iner <- as.data.frame(
  readxl::read_xlsx("./BreedingSyst/data/archivopreguntas.xlsx"))
iner$afc_yr <- iner$edad_primer_parto_years
iner <- iner[, c(
  "finca", "region", "sistema_productivo", "producto",
  "peso_vacas", "peso_toros", "peso_terneros", "peso_terneras",
  "peso_sacrificio_vaconas", "peso_sacrificio_toretes",
  "peso_descarte_vacas", "produccion_leche_litro_animal_dia",
  "periodo_lactancia_dias", "grasa_leche", "proteina_leche", "afc_yr"
)]
iner[iner == 0] <- NA
# Aggregate duplicated observacion per year?
iner <- iner %>%
  group_by(finca, region, sistema_productivo, producto) %>%
  summarise_all(mean, na.rm = T)
iner <- iner[, -grep("finca", names(iner))] %>%
  group_by(region, sistema_productivo, producto) %>%
  summarise_all(mean, na.rm = T)
iner <- as.data.frame(iner)
names(iner) <- names(inac)
iner <- rbind(iner, inac[inac$clauclass == "EMPRESARIAL", ])
iner$clauclass <- Recode(
  iner$clauclass,
  "'MARGINAL'=1;'MERCANTIL'=2;'EMPRESARIAL'=3"
)
iner$region <- Recode(iner$region, "'COSTA'=1;'SIERRA'=2;'AMAZONIA'=3")
iner$producto <- Recode(iner$producto, "'Leche'=1;'Carne'=2")
## GLEAM 2.0 average weight and gain
iner$m_vacona_m <- (iner$m_vacona_s - iner$m_ternera_m) / 2 +
  iner$m_ternera_m
iner$m_novillo_m <- (iner$m_novillo_s - iner$m_ternero_m) / 2 +
  iner$m_ternero_m
iner$m_ternera_s <- (iner$m_vacona_m - iner$m_ternera_m) / 2 +
  iner$m_ternera_m
iner$m_ternero_s <- (iner$m_novillo_m - iner$m_ternero_m) / 2 +
  iner$m_ternero_m
iner$m_vaca_awg <- (iner$m_vaca_m - iner$m_vacona_m) / (iner$afc_yr * 365)
iner$m_toro_awg <- (iner$m_toro_m - iner$m_novillo_m) / (iner$afc_yr * 365)
iner$m_toro_awg[iner$m_toro_awg < 0] <- 0
iner$m_vaca_awg[iner$m_vaca_awg < 0] <- 0
iner$m_vacona_awg <- (iner$m_vacona_m - iner$m_ternera_m) /
  ((iner$afc_yr - 1) * 365)
iner$m_novillo_awg <- (iner$m_novillo_m - iner$m_ternero_m) /
  ((iner$afc_yr - 1) * 365)
iner$m_ternera_awg <- (iner$m_ternero_s - iner$m_ternera_m) / 365
iner$m_ternero_awg <- (iner$m_ternero_s - iner$m_ternero_m) / 365
iner$m_toro_s <- iner$m_toro_m + iner$m_toro_awg * 365 / 2
# Approx. missing weights
iner$m_vaca_s[is.na(iner$m_vaca_s)] <- iner$m_vaca_m[is.na(iner$m_vaca_s)] +
  iner$m_vaca_awg[is.na(iner$m_vaca_s)] * 365 / 2
# Add mercantil = Empresarial in Amazon
tp <- iner[iner$region == 3 & iner$clauclass == 2, ]
tp$clauclass <- 3
iner <- rbind(iner, tp)
# Mean protein produced: vleche:1 / vcarne2 : prot_venta_carn > prot_venta_l
mpp_tp <- iner %>%
  group_by(region, clauclass) %>%
  summarise_all(mean) # Mean weights
mpp_tp[nrow(mpp_tp), "clauclass"] <- 3
mpp_tp <- mpp_tp[, grep("m_.*_s|reg|clau|llac_day|fat|prot", names(mpp_tp))]
names(mpp_tp) <- gsub("m_", "m_w", names(mpp_tp))
# GLEAM reference  weigths parameters
iner[, gsub("vaca", "nvlt", grep("vaca", names(iner), value = T))] <-
  iner[, grep("vaca", names(iner))] # nvlt vlt
names(iner)[grep("vaca", names(iner))] <- gsub(
  "vaca", "vlt",
  grep("vaca", names(iner), value = T)
)
names(iner) <- gsub("(.*)_(.*)_(.*)", "\\1_\\3_\\2", names(iner))

###### ADD Mean protein produced: prot_venta_carn > prot_venta_l##############
# Milk prot:costa=3.42,Sierra=3.01,amazónica=2.91
# lact prom day: costa 174.8 sierra 225.7 oriente 225.1
# qw$m_llac_day<-Recode(qw$region,"1=174.8;2=225.7;3=225.1")
qw <- merge(qw, mpp_tp,by=c("region","clauclass"), all.x = T)
qw$m_mlkprot <- Recode(qw$region, "1=3.42;2=3.01;3=2.91")
qw$lpv[which(qw$lpv > 40)] <- qw$lpv[which(qw$lpv > 40)] / 10 # Some cases 2019
qw$m_mlkprot_tot <- qw$m_tvlt * qw$lpv * qw$prot * qw$llac_day
qw$m_mlkprot_tot[is.na(qw$m_mlkprot_tot)] <- 0
# Beef protein:0.23kg/kg * 50% of weight is produced in kg of meat
su(qw[, grep("^m_w", names(qw))])
qw$m_meaprot_tot <- 0.23 * 0.5 * (
  qw$m_wvaca_s * (qw$m_vvaca + qw$m_avaca) +
    qw$m_wtoro_s * (qw$m_vtoro + qw$m_atoro) +
    qw$m_wternero_s * (qw$m_vternero + qw$m_aternero) +
    qw$m_wternera_s * (qw$m_vternera + qw$m_aternera) +
    qw$m_wvacona_s * (qw$m_vvacona + qw$m_avacona) +
    qw$m_wnovillo_s * (qw$m_vnovillo + qw$m_anovillo))
qw$producto <- as.numeric(qw$m_meaprot_tot > qw$m_mlkprot_tot) + 1
# By default no clear production orientation asign lower management : meat
qw$producto[qw$m_meaprot_tot == 0 & qw$m_mlkprot_tot == 0] <- 2

###### ADD Mean Weights ######################################################
# Merge parameters : add vlt nvlt
qw <- merge(qw, iner[, -grep("lpv", names(iner))],
            by = c("region", "clauclass", "producto")
            )

###### ADD Other Parameters Compiled with Tier 2 reference##################
names(bs) <- tolower(names(bs))
names(bs)[grep("de", names(bs))] <- c("de_n", "de")
names(bs)[grep("clau", names(bs))] <- "clauclass"
bs <- bs[, c("clauclass", "subcat", "cfi", "ca", "cpreg", "c")]
bs <- bs[-grep("vacona_lac", bs$subcat), ] # remove : m_tvacona_lactante
bs$subcat <- gsub("m_vacona_nolactante", "m_vacona", bs$subcat)
bs$subcat <- gsub("m_vaca_nolactante", "m_nvlt", bs$subcat)
bs$subcat <- gsub("m_vaca_lactante", "m_vlt", bs$subcat)
bs <- bs %>%
  pivot_wider(names_from = subcat, values_from = c(cfi, ca, cpreg, c))
names(bs) <- gsub("(.*_)(.*_)(.*)", "\\2\\1\\3", nm(bs))
# Add castrated animal c coef for male meat system
bs <- rbind(bs, bs)
bs$producto <- c(1, 1, 1, 2, 2, 2)
bs$m_c_toro[bs$producto == 2] <- 1
# Merge iner and bs #rename some
qw <- merge(qw, bs, all.x = T)

###### ADD average milk production per db by Canton, class & yr###############
# When lactating cow exist by report of production doesnt...
# Get mean milk production per cow + cattle sys type + upm + yr
lpvcant <- aggregate(
  lpv ~ clauclass + yr + cant,
  data = qw, FUN = mean, na.rm = T
)
names(lpvcant)[grep("lpv", names(lpvcant))] <- "lpvcant"
qw <- merge(qw, lpvcant, all.x = T)
saveRDS(qw,"./BreedingSyst/qw_qbstier2_ready.rds")

################################################################################
###### II.CROP / RATION AND INTAKE  MODULE  :  farm digestibility db ###########
################################################################################
# Observing pasture characteristics from agricultural survey:
# average digestibility of feed per variety, age
###### Load GAEZ Parameters, GLEAM Parameters ##################################
# Key for merge between gz4 and espac
gzr <- readxl::read_xlsx(
  "./BreedingSyst/data/GLEAM_2.0_Supplement_S1_short.xlsx",
                         sheet = "GAEZ_crop_ref"
)
# Gleam parameters: feed digestibility, dry matter, fue mafa slope intake adjust
glr <- as.data.frame(
  readxl::read_xlsx("./BreedingSyst/data/GLEAM_2.0_Supplement_S1_short.xlsx",
            sheet = "gleam_DMYld_param"
  )
)
glr$nlist <- Recode(glr$nlist,"'cut_pasture'='roughage';'diet'='diet';
       'main_pasture'='roughage';'mixture_pasture'='roughage'")

###### BUILD FPAT_TP FEED GROUP REFERENCE GLEAM EC GCI PARAMETERS #############
# Average diet composition per region, prod system, product, and main feed cat
farm_data <- readxl::read_xlsx("./BreedingSyst/data/archivopreguntas.xlsx")
farm_data$producto <- gsub(" $", "", farm_data$producto)
farm_data$alimento1_nombre <- car::Recode(
  farm_data$region,
  "'COSTA'='saboya';'SIERRA'='kikuyo';'AMAZONIA'='gramalote'"
)
food_list <- farm_data[, grep("NUM|DMI_OT|DMI_AF|^alim", names(farm_data),
                              value = T
)]
food_list <- tidyr::pivot_longer(food_list,
                                 cols = grep("^alim", names(food_list)),
                                 names_to = c("time", ".value"),
                                 names_pattern = "alimento(.*)_(.*)"
)
fltr <- food_list$vacas == 0 & food_list$otros == 0 # Remove empty records
food_list <- as.data.frame(food_list[!fltr, ])
food_list$name <- uacc(food_list$nombre)
# Food name to food list: mainpasture, mixture_pasture, cut_pasture, diet
food_list$nlist <- car::Recode(
  food_list$name,
  "'saboya'='main_pasture_list';'kikuyo'='main_pasture_list';
  'gramalote'='main_pasture_list';'pasto corte'='cut_pasture_list';
  'pasto con leguminosa'='mixture_pasture_list';
  'mezcla de pasto con leguminosas'='mixture_pasture_list';
  'mezcla forrajera'='mixture_pasture_list';
  'ensilaje pasto'='cut_pasture_list';'ensilaje planta maiz'='cut_pasture_list';
  'heno o ensilaje de alfalfa'='cut_pasture_list';'residuos arroz'='cut_pasture_list';
  'residuos cana azucar'='cut_pasture_list';'residuos maiz'='cut_pasture_list';
  'residuos otros granos'='cut_pasture_list';'residuos trigo'='cut_pasture_list';
  else='diet_list'"
)

# Make pattern of food pasture, mix pasture, cut pasture, feed, No ref. for :
food_list <- merge(
  food_list,
  farm_data[,grep("NUM|a_prod|^producto$|region", names(farm_data))],
  all.x = T
)
fpat_tp <- tidyr::pivot_wider(
  food_list[, grep("reg|time|nlist|NUM|prod|vaca|otro", names(food_list))],
  names_from = nlist,
  values_from = c(vacas, otros)
)
fpat_tp[is.na(fpat_tp)] <- 0
# Join some regions :
fltr <- (fpat_tp$region == "AMAZONIA" & fpat_tp$producto == "Leche") |
  (fpat_tp$region == "COSTA" & fpat_tp$producto == "Leche") |
  (fpat_tp$region == "SIERRA" & fpat_tp$producto == "Carne")
fpat_tp$sistema_productivo[fltr] <- "Ambos"
fpat_tp <- fpat_tp[, -grep("time", names(fpat_tp))] %>%
  group_by(NUM, producto, sistema_productivo, region) %>%
  summarise(across(everything(), list(sum)))
fpat_tp <- fpat_tp[, -grep("NUM", names(fpat_tp))] %>%
  group_by(region, sistema_productivo, producto) %>%
  summarise(across(everything(), list(mean)))
fpat_tp[, grep("vacas", names(fpat_tp))] <-
  fpat_tp[, grep("vacas", names(fpat_tp))] / rowSums(fpat_tp[, grep("vacas", names(fpat_tp))])
fpat_tp[, grep("otros", names(fpat_tp))] <-
  fpat_tp[, grep("otros", names(fpat_tp))] / rowSums(fpat_tp[, grep("otros", names(fpat_tp))])
names(fpat_tp) <- gsub("_1|_list", "", nm(fpat_tp))
fpat_tp <- as.data.frame(fpat_tp)
fpat_tp <- fpat_tp[order(fpat_tp$region, fpat_tp$producto, fpat_tp$sistema_productivo), ]
# Reshape for merge nc
fpat2 <- fpat1 <- fpat_tp[fpat_tp$sistema_productivo == "Ambos", ]
fpat1$sistema_productivo <- "MARGINAL"
fpat2$sistema_productivo <- "MERCANTIL"
fpat_tp <- rbind(
  fpat_tp[fpat_tp$sistema_productivo != "Ambos", ],
  fpat1, fpat2
)
fpat_tp[, grep("vaca", names(fpat_tp))] <- t(apply(fpat_tp[, grep("vaca", names(fpat_tp))], 1, adj1p))
fpat_tp[, grep("otro", names(fpat_tp))] <- t(apply(fpat_tp[, grep("otro", names(fpat_tp))], 1, adj1p))
# Prepare for merge
fpat_tp <- fpat_tp %>%
  gather(key = group, value = pct, -region, -sistema_productivo, -producto)
fpat_tp$a_group <- substr(fpat_tp$group, 1, 5)
fpat_tp$nlist <- substr(fpat_tp$group, 7, 21)
fpat_tp <- fpat_tp[, -grep("^group", names(fpat_tp))]
fpat_tp <- spread(fpat_tp, key = a_group, value = pct)
fpat_tp$region <- car::Recode(fpat_tp$region, "'AMAZONIA'=3;'COSTA'=1;'SIERRA'=2")
fpat_tp$sistema_productivo <- car::Recode(
  fpat_tp$sistema_productivo,
  "'MARGINAL'=1;'MERCANTIL'=2"
)
fpat_tp$producto <- car::Recode(fpat_tp$producto, "'Leche'=1;'Carne'=2")
# With temporal variation aggregation of roughage is necesary 
fpat_tp$nlist <- Recode(fpat_tp$nlist,"'cut_pasture'='roughage';'diet'='diet';
       'main_pasture'='roughage';'mixture_pasture'='roughage'")
fpat_tp <- fpat_tp %>% group_by(region,sistema_productivo,producto,nlist) %>%
  summarise(otros=sum(otros),vacas=sum(vacas))

###### PREP CROP DB : Select crop #############################################
# Build Pasture digestibility db by region, class & yr
nc <- readRDS("./dbb/qori_atxt/nx2.rds")
# Add basic farm data from qw
nc <- nc[nc$cdctv < 903, ]
nc$time <- as.numeric(nc$time )
qvars <- c("yr", "idsto", "producto", "sum_ganad", "m_vlt",
           "region", "clauclass","clau","class","fexpf1")
nc <- merge(nc,
            qw[, qvars],
            by = c("yr", "idsto"), all.x = T
)
# Filter clauclass and build cant
nc <- nc[!is.na(nc$clauclass), ]
nc$cant <- substr(nc$idsto, 1, 4)
# Seeded area correction 
nc$ssem[is.na(nc$ssem)] <- nc$scos[is.na(nc$ssem)]
# Assign other forage (799) to most cultivated forage per region
# Keep only gleam item feed groups 
nc <- merge(nc, np[, c("cdctv", "gleam_ec")], all.x = T)
nc <- nc[nc$cdctv %in%
           np$cdctv[!is.na(np$gleam_ec)], ] # Keep only potential feed crops
# Pasture Age: Decrease in Digestibility : -2.5 % DE per year
nc$apas[nc$gleam_ec!="Grass"] <- NA # Only grass
nc$apao <- nc$apas

###### EXTRACT GAEZ DATA FOR ESPAC 2000-2020 ####################################
# Get FMYGcrop : over raster data
gz4 <- readRDS("./BreedingSyst/data/gaezv4_db.rds")
# Wich crop are not available in RES02 Agro-climatic potential yield recover
# average attainable yield : yc.*
# Extracted data  in gaezv4_db.rds# Get FMYGcrop ref
# Filter out non Ec cattle feed crops
gz4$crop <- gsub("_", "", gz4$crop)
gz4 <- gz4[, -grep("region", names(gz4))]
# keep only crop of interest
# get "sugc" "oilp" "cott" "sugb" from res05...
# as: "sub", "olp", "cot", "suc"
gzr$crop <- gzr$res02_Agr_climPot_Yld_abv
gzr$crop[gzr$crop == "sugc"] <- "suc"
gzr$crop[gzr$crop == "cott"] <- "cot"
gz4 <- gz4[gz4$crop %in% gzr$crop, ] 

# Irrigation : Keep only wtr_sup==i&r irrigation  and Rainfed
gz4 <- gz4[gz4$wtr_sup == "i" | gz4$wtr_sup == "r", ]

# Merge with espac crop codes # Add cdctv to gz4
gzcr_ref <- gzr[, c("cdctv", "crop", "reg2")]
gzcr_ref <- gzcr_ref[!duplicated(gzcr_ref), ]
gz4 <- merge(gz4, gzcr_ref, all.x = T)
gz4$idpar <- as.numeric(gz4$DPA_PARROQ) # Parroquia Codes
gz4$rieg <- car::Recode(gz4$wtr_sup, "'i'=1;'r'=2") # other vars
gz4$input <- as.numeric(gz4$input == "H") # other vars
gz4$dw[gz4$crop %in% c("alfa", "napr", "grlg", "gras")] <-
  gz4$dw[gz4$crop %in% c("alfa", "napr", "grlg", "gras")] * 10

# Expand years & merge to only existing cross var in Espac: GZ
gz <- nc[, c("idpar", "cdctv", "rieg", "reg2", "input", "yr")]
gz <- gz[!duplicated(gz), ]
print(dim(gz))
gz <- split(gz, f = gz$yr)
for (i in unique(gz4$time)) {
  nb <- as.numeric(strsplit(i, "_")[[1]])
  tp <- gz4[gz4$time == i, c("idpar", "cdctv", "rieg", "reg2", "input", "dw")]
  yrref <- which(names(gz) %in% nb[1]:nb[2])
  gz[yrref] <-
    lapply(
      gz[yrref],
      function(x) {
        tp$yr <- x$yr[1]
        x <- merge(x, tp, all.x = T)
        return(x)
      }
    )
  print(paste0(i, " is done"))
}
gz <- do.call("rbind", gz)
print(dim(gz))

# Adjust some missing crops to default values 
# Sugarbeet : 25 ton/ ha ecuador 569
gz$dw[gz$cdctv == 569 & is.na(gz$dw)] <- 25000
# Sugarcane = Unsold Harvest (65 T/ha)  419,420,782
gz$dw[is.na(gz$dw) & gz$cdctv %in% c(419, 420, 782)] <- 65000
# oli palm : Harvested (11 t/ha)
gz$dw[is.na(gz$dw) & gz$cdctv %in% 456] <- 11000
# Cotton : 6.5 ton/ ha (Silanikove 1986) 504

###### MERGE GZ GLR & FPAT #####################################################
# Add Dry matter reference
# Merge with nc : duplicates (grain + straw) for Barley, beetroot, yuca
nc <- merge(nc, gz, 
            by = c("idpar", "cdctv", "rieg", "reg2", "input", "yr"), all.x = T)
# Add feed intake paramters
glr_var <- c(
  "cdctv", "calc", "digestibility_percentage", "nitrogen_content",
  "dry_matter", "nlist", "FUE", "MFA", "dm_pct",
  "splope_rsd_ha", "intcpt_rsd_ha"
)
nc <- merge(nc, glr[, glr_var], all.x = T)

# Add Group Fraction  ration reference..
nc <- merge(nc, fpat_tp, all.x = T) 
nc <- nc[order(nc$yr, nc$idsto, nc$nlist,nc$cdctv), ]

###### RUN GLEAM FEED RATION MODULE (chap 3) ###################################
nc$yld <- nc$ccos/nc$scos
nc$pven <- nc$cven/nc$ccos
##### 3.1 Crop Yield and pasture productivity
# Availability in Kg of crop2
# Equation 3.1 (Crops): Fresh matter yield
# if calc==Kg_hrv: ccos-cven to kg# if calc==ms_ref: ssem to kg
fltr <- nc$calc == "Kg_hrv"
nc$FMYGcrop <- NA
# Equation 3.16 – Cattle. share of by product by harvest
nc$FMYGcrop[fltr] <- (nc$ccos[fltr] - nc$cven[fltr]) * 1000 # tons to kg
nc$FMYGcrop[fltr & is.na(nc$cven)] <- 
  nc$ccos[fltr & is.na(nc$cven)]  * 1000 # mis harvest q

# Equation 3.16 – Cattle. share of by product by area
nc$FMYGcrop[!fltr] <- nc$ssem[!fltr] * nc$dw[!fltr] # tons to kg
# Gross dry matter yield
nc$DMYGcrop <- nc$FMYGcrop * nc$dry_matter / 100
# DMYGcrop = gross dry matter yield per crop, kg DM·ha-1
# FMYGcrop = fresh matter yield of each crop, kg DM·ha-1 : ccos - cven
# DMcrop = dry matter content of each crop, percentage. Table 3.1
# Equation 3.2 Crop residues adjusted ingestion
nc$DMYGcr <- nc$DMYGcrop * nc$splope_rsd_ha * nc$ssem +
  nc$intcpt_rsd_ha * nc$ssem
nc$DMYGcr[is.na(nc$DMYGcr)] <- nc$DMYGcrop[is.na(nc$DMYGcr)]
# DMYGcr = gross dry matter yield of the crop residues of each crop, kg DM·ha-1
# DMYGcrop = gross dry matter yield of each crop, kg DM·ha-1
# Slope-crop = slope from IPCC equation for each crop.Table 3.1
# Intercept-crop = intercept from IPCC equation

##### 3.2 Ruminant feed ration 
#### 3.2.1 – Calculation of the net dry matter yields 
# Gross dry matter yield  * Feed Use Efficiency (FUE) frac. efctivly used feed
#                         * Mass Fraction Allocation (MFA) factor
# Equation 6.10 for MFA : calc MFA when FUE is present : fro crop residue
# 1 as residual yield is reported separatly and crop residue depend on MS yields
nc$MFA[(!is.na(nc$FUE)) & is.na(nc$MFA)] <- 1

# Equation 3.3
nc$DMYN <- nc$DMYGcr * nc$FUE * nc$MFA
nc$DMYN[is.na(nc$DMYN)] <- nc$DMYGcr[is.na(nc$DMYN)]
# 1Grass,7:15:maiz_wheat_sugc,17sugb,20:23byprod:soy_rapeseed_cotton_oil_palm
# 27melasa,28Avg_grain_yld(wheat, maize, barley, millet, sorghum,rice,other)
# DMYNi = net dry matter yield of feed material i, kg DM·ha-1
# DMYGi = crop gross dry matter yield for feed material i, kg DM·ha-1.
# DMYGi = yield of crop residues , or Maiz Yield residu+Yield normal
# FUEi = feed use efficiency for feed material table 3.5
# MFAi = mass fraction allocation of feed_i : % feed material of total mass (no grass)
nc <- nc[order(nc$yr, nc$idsto, nc$nlist,nc$cdctv), ]
nc$compl_food <- as.numeric(is.na(nc$cdctv))

#### 3.2.3 – Feed rations in developing countries
# Fraction of Kg MS per food group
nc$n <- 1
nc <- nc %>% group_by(yr,idsto,nlist) %>% 
  mutate(f_frac1=DMYN / sum(DMYN, na.rm = T))
nc <- nc %>% group_by(yr,idsto) %>% mutate(n=sum(n))

nc$f_frac1[nc$n==2 & is.na(nc$f_frac1)] <- 1
# Average digestiblity 
# Adjust per Pct ref or # %roughage=1 - %byprod + %concentrate?
nc$xotcide <- nc$f_frac1 * nc$otros * nc$dig
nc$xafcide <- nc$f_frac1 * nc$vacas * nc$dig
nc <- nc %>% group_by(yr, idsto) %>%
                 mutate(xotcide=sum(xotcide, na.rm = T) / 100, 
                           xafcide=sum(xafcide, na.rm = T) / 100)

################################################################################
###### III. EMISSION  MODULE  :  Apply IPCC formula build animal  db ###########
################################################################################
###### ADD Mean Digestibility ##################################################
pdb <- nc
pdb %>% group_by(region,sistema_productivo,producto) %>% 
  summarise(xotcide=mean(xotcide,na.rm=T),
            xafcide=mean(xafcide,na.rm=T),n=n()) %>% adf(.)

qw <- merge(qw, pdb[,c("yr","idsto","xafcide","xotcide")], all.x = T)
####### Compute final Energy parameters ########################################
nvar <- c(
  "^id","fexpf1","^m_","region","clauclass","producto",
  "lpv","llac_day","fat","prot","afc_yr","xafcide","xotcide","yr",
  "sexo_pp", "edad_pp", "educ", "tenencia", "empl_upa",
  "empl_ha", "lab_fam", "lab_prm", "auha", "sum_ganad"
)
qbs <- qw[, grep(paste0(nvar, collapse = "|"), names(qw))]
# Rename some stuff and Remove compra "c" "p" "a" "v"
names(qbs) <- gsub("nvlt", "nxlt", names(qbs))
avar <- c(
  "ternero", "ternera", "novillo", "vacona",
  "toro", "vaca", "vlt", "nxlt"
)
hvar <- paste0("m_(c|p|a|v)(", paste0(avar, "$", collapse = "|"), ")")
qbs <- qbs[, -grep(hvar, names(qbs))] # remove
qbs <- qbs[, -grep("m_tvaca|m_evaca|^m_vlt$|m_ll", names(qbs))] # remove
gvar <- paste0("(.*)(", paste0(avar, "$", collapse = "|"), ")") # global rename
names(qbs) <- gsub("__", "_", gsub(gvar, "\\1_\\2", names(qbs)))
qbs <- cbind(
  qbs[, -grep("m_.*_.*", names(qbs))],
  qbs[, grep("m_.*_.*", names(qbs))]
)
# Reshape db for emission calculations
# t=total ax; e=existing; ax m=muerto ax; s=sacrificed ax; awg=weight ax;
# cfi=maintenance coef ca=activity coef cpreg=pregnancy coef c=growth coef
qbs <- qbs %>% pivot_longer(
  cols = grep("m_.*_.*", names(qbs))[1]:ncol(qbs),
  names_to = c(".value", "time"),
  names_pattern = "m_(.*)_(.*)"
)
# Complete Milk per cow
qbs$lpv[is.na(qbs$lpv)] <- qbs$lpvcant[is.na(qbs$lpv)]
qbs$lpv[qbs$time != "vlt"] <- 0
# Rebuild digestibility 
qbs$de <- NA
qbs$de <- qbs$xotcide
qbs$de[qbs$time == "vlt"] <- qbs$xafcide[qbs$time == "vlt"]
qbs$de <- qbs$de * 100
qbs$ded <- 60

# NEm net energy for maintenance
# Animal category Cfi(MJ*d^-1*kg^-1)
# Cattle: non-lactating 0.322 || lactating 0.386 (+20% in lactation)
# Bulls 0.370(+15% for intact males)|| Sheep_<1yr 0.236 (+15% for intact males)
# >1yr 0.217 (+15% for intact males) #Source: NRC (1996) and AFRC (1993).
qbs$nem <- qbs$cfi * (qbs$m)^.75
# NEm net energy for activity
# Cattle (unit for C a is dimensionless)
# Pasture:confined_areas sufficient_forage modest_energy expense_acquire_food_0.17
# Grazing large areas: open_range_land hilly_terrain significant_energy_0.36
# Grazing flat pasture: <=1000m/day very_little_energy to acquire feed_0.0107
# Sheep (Ca=MJ*d^-1*kg^-1)
qbs$nea <- qbs$ca * qbs$nem
# NEg net energy for growth
# C=coefficient: 0.8_females, 1.0_castrates, 1.2_bulls (NRC, 1996)
# Castrated: adult male meat:
qbs$neg <- 22.02 * (qbs$m / (qbs$c * qbs$s))^.75 * qbs$awg^1.097
# NEp net energy for pregnancy #Cpreg=.1 pregnancy coef
qbs$nep <- qbs$cpreg * qbs$nem
# REM Ratio:net_ener_available_in_diet_for_maintenance to digestible_Ener_Consumed
# DE% = digestible energy expressed as a percentage of gross energy
qbs$rem <- (1.123 - (4.092 * 10^-3 * qbs$de) +
  (1.126 * 10^-5 * qbs$de^2) - (25.4 / qbs$de))
qbs$rem[qbs$de==0 | qbs$rem<0] <- NA
# REG ratio: net_energy_available for growth in diet to digestible_ener_consumed
qbs$reg <- (1.164 - (5.160 * 10^-3 * qbs$de) +
  (1.308 * 10^-5 * qbs$de^2) - (37.4 / qbs$de))
qbs$reg[qbs$de==0 | qbs$reg<0] <- NA
# NEl net energy for lactation: based on local (upm+class) average for milk/day
qbs$nel <- qbs$lpv * (1.47 + 0.40 * qbs$fat)
qbs$nel[is.na(qbs$nel)] <- 0
# GE Gross energy intake: Selective assignation
# Energy component:|_____NEM_NEA_NEP_NEL_NEG
# Yng.male_meat+dairy:___x___x___o___o___x__
# Yng.female_meat+dairy:_x___x___o___o___x__
# Adt.male_meat:_________x___x___o___o___x__
# Adt.male_dairy:________x___x___o___o___o__
# Adt.female_meat:_______x___x___o___o___x__
# Adt.female_dairy_lact:_x___x___x___x___o__
# Adt.female_d+m_nolac:__x___x___x___x___o__
qbs$nep[qbs$time == "vlt" & qbs$producto == 2] <- 0
# Add for nxlt
qbs$nel[qbs$time == "vlt" & qbs$producto == 2] <- 0
# Add for nxlt
qbs$neg[qbs$time == "toro" & qbs$producto == 1] <- 0
qbs$neg[qbs$time == "vlt" & qbs$producto == 1] <- 0
qbs$neg[qbs$time == "nxlt"] <- 0
# Energy 
qbs$ge <- ((qbs$nem + qbs$nea + qbs$nep + qbs$nel) / qbs$rem +
  qbs$neg / qbs$reg) / (qbs$de/100)

# EMISSION :
# Y m = methane conversion factor, per cent of gross energy in feed converted
qbs$ym <- 9.75 - 0.05 * qbs$de
# to methane 55.65 MJ/kg CH4 Enery content of methane : (kg CH4 head-1 year-1)
qbs$ef <-  (qbs$ge * qbs$ym/100 * 365) / 55.65
# qbs$subcat<-gsub("m_","m_t",qbs$subcat) #rename soem cow

# Manure Managment:no data then...  1.2 milkin cow 1 other
qbs$mm_ef <- 1
qbs$mm_ef[qbs$time == "vlt"] <- 1.2
#lpv=2.2 Ym=6.5 ef_ot=56 ef_af=72 de=60%
qbs$ef_d <- car::Recode(qbs$time,"'toro'=61;'vlt'=72;'nxlt'=64;else=49")
qbs$eft_ge_T1 <- qbs$t * qbs$ef_d # No model Tier 1 (table 10.11)
qbs$eft_ge_Hm <- qbs$e * qbs$ef_d # Herd model only
qbs$eft_ge_Im <- qbs$t * qbs$ef  # Intake Model Only
qbs$eft_ge_HmIm <- qbs$e * qbs$ef # Herd & Intake Model
qbs$eft_mm <- qbs$mm_ef * qbs$e # w emission manure managment
qbs$eft <- rowSums(qbs[, c("eft_ge", "eft_mm")], na.rm = T)
