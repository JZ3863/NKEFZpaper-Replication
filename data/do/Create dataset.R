#Work directory
setwd("Output directory")
data_dir <- path.expand("Data directory")
getwd()

# load packages
packages <- c("haven", "dplyr", "expss", "psych", "tidyr", "readxl", "labelled")
#install.packages(packages, dependencies = TRUE)
lapply(packages, require, character.only = TRUE)

#==============================================================================#
# Import and merge datasets ####
#==============================================================================#
ha_o <- read_dta(file.path(data_dir,"H_CHARLS_D_Data.dta"))

## wave 5 ####
wave5 <- read_dta(file.path(data_dir, "Sample_Infor.dta"))
# Helper function to safely full_join and coalesce overlapping columns
safe_full_join <- function(x, y, by) {
  out <- full_join(x, y, by = by, suffix = c("", ".new"))
  
  dup_vars <- grep("\\.new$", names(out), value = TRUE)
  
  for (v in dup_vars) {
    base <- sub("\\.new$", "", v)
    
    # Align classes
    if (!is.null(out[[base]])) {
      if (class(out[[base]])[1] != class(out[[v]])[1]) {
        out[[base]] <- as.character(out[[base]])
        out[[v]]    <- as.character(out[[v]])
      }
    }
    out[[base]] <- coalesce(out[[base]], out[[v]])
    out[[v]] <- NULL
  }
  out
}

# List of files to merge
files <- c(
  "Demographic_Background.dta",
  "Work_Retirement.dta",
  "Weights.dta",
  "Individual_Income.dta",
  "Health_Status_and_Functioning.dta",
  "Exit_Module.dta",
  "COVID_Module.dta",
  "Family_Information.dta",
  "Household_Income.dta"
)

# Initial dataset
wave5 <- read_dta(file.path(data_dir, "Sample_Infor.dta"))

# Loop to merge all files
for (f in files) {
  dat <- read_dta(file.path(data_dir, f))
  # Decide join keys depending on file
  if (f %in% c("Family_Information.dta", "Household_Income.dta")) {
    join_keys <- c("householdID")
  } else {
    join_keys <- c("ID")
  }
  wave5 <- safe_full_join(wave5, dat, by = join_keys)
}
names(wave5)[grepl("\\.(x|y)$", names(wave5))]

## psu ####
psu <- read_dta(file.path(data_dir,"2011_#psu.dta"))
psu <- psu %>% 
  full_join(read_dta(file.path(data_dir, "2013_#psu.dta")) %>% 
              select(-versionID), 
            by = c("communityID", "province", "city", "urban_nbs", "areatype"))
anyDuplicated(psu$communityID)

## sub datasets: 2011-2018 ####
a2011_I_Housing_characteristics <- read_dta(file.path(data_dir,"2011_I_Housing_characteristics.dta"))
a2013_I_Housing_characteristics <- read_dta(file.path(data_dir,"2013_I_Housing_characteristics.dta"))
a2015_I_Housing_characteristics <- read_dta(file.path(data_dir,"2015_I_Housing_characteristics.dta"))
a2018_Ha_I_Housing <- read_dta(file.path(data_dir,"2018_Ha_I_Housing.dta"))

a2011_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2011_D_Health_status_and_functioning.dta"))
a2013_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2013_D_Health_status_and_functioning.dta"))
a2015_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2015_D_health_status_and_functioning.dta"))
a2018_D_Health_status_and_functioning <- read_dta(file.path(data_dir,"2018_D_Health_Status_and_Functioning.dta"))

a2011_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2011_E_Health_care_and_insurance.dta"))
a2013_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2013_E_Health_care_and_insurance.dta"))
a2015_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2015_E_health_care_and_insurance.dta"))
a2018_E_Health_care_and_insurance <- read_dta(file.path(data_dir,"2018_E_Health_Care_and_Insurance.dta"))

a2011_Family_information <- read_dta(file.path(data_dir,"2011_C1_Family_information.dta"))
a2013_Family_information <- read_dta(file.path(data_dir,"2013_A_Family_information.dta"))
a2015_Family_information <- read_dta(file.path(data_dir,"2015_A_Ca_Cd_Family_information.dta"))
a2018_Family_information <- read_dta(file.path(data_dir,"2018_C2_Family_Transfer.dta"))

## merge ####
ha <- ha_o %>%
  full_join(wave5, by = c("ID", "householdID", "communityID")) %>%
  mutate(
    merge1 = case_when(
      !is.na(ID) & ID %in% ha_o$ID & ID %in% wave5$ID ~ "matched",
      !is.na(ID) & ID %in% ha_o$ID & !(ID %in% wave5$ID) ~ "only_ha_o",
      !is.na(ID) & !(ID %in% ha_o$ID) & ID %in% wave5$ID ~ "only_wave5"
    )
  ) %>% 
  full_join(psu, by = "communityID")
names(ha)[grepl("\\.(x|y)$", names(ha))]
table(ha$merge1)
nrow(ha) # n = 25,882

# delete used datasets
rm(ha_o, wave5, psu, dat, packages)
gc()

#==============================================================================#
# Create variables 
#==============================================================================#

# Birth year ####
sum(is.na(ha$rabyear) & !is.na(ha$ba003_1)) # n = 143
sum(is.na(ha$rabyear) & !is.na(ha$zrbirthyear)) # n = 238
ha <- ha %>% 
  mutate(
    birth_year = coalesce(rabyear, ba003_1, zrbirthyear)
  )
describe(ha[, c("rabyear", "birth_year")], fast = TRUE)

# Sex ####
sum(is.na(ha$ragender) & !is.na(ha$xrgender)) # n = 288
sum(is.na(ha$ragender) & !is.na(ha$ba001)) # n = 288
ha <- ha %>%
  mutate(
    ragender_num = as.double(ragender),  # strip labels safely
    sex = coalesce(ragender_num, xrgender),
    sex = factor(sex, levels = c(2, 1), labels = c("Female", "Male"))
  ) %>%
  select(-ragender_num)
table(ha$sex, useNA = "always") # missing = 16  

# Education ####
ha <- ha %>%
  mutate(
    zredu_num = case_when(
      !is.na(zredu) ~ zredu,
      !is.na(ba010) ~ ba010,
      is.na(zredu) & is.na(ba010) & ba010_1 == 2 ~ 1
    )
  )
table(ha$zredu, useNA = "always")
table(ha$zredu_num, useNA = "always")
ha <- ha %>%
  mutate(
    educ = coalesce(raeduc_c, zredu_num),
    educ = factor(
      educ,
      levels = 1:11,
      labels = c(
        "Illiterate",
        "Didn't finish primary school but can read",
        "Sishu",
        "Elementary school",
        "Middle school",
        "High school",
        "Vocational school",
        "Two/three-year college",
        "College graduate",
        "Postgraduate degree",
        "PhD"
      )
    )
  )
table(ha$educ, useNA = "always") # missing = 41

ha <- ha %>%
  mutate(
    educl = raeducl,
    edu_numeric = as.numeric(educ),  # get numeric codes from factor
    educl = case_when(
      is.na(educl) & edu_numeric %in% 1:5  ~ 1,
      is.na(educl) & edu_numeric %in% 6:7  ~ 2,
      is.na(educl) & edu_numeric %in% 8:10 ~ 3,
      TRUE ~ educl
    ),
    educl = factor(
      educl,
      levels = 1:3,
      labels = c(
        "Less than lower secondary education",
        "Upper secondary & vocational training",
        "Tertiary education"
      )
    )
  ) %>%
  select(-edu_numeric)
table(ha$educl, useNA = "always") # missing = 14

# Marital status ####
sum(ha$r1mnev == 1 & (ha$r1mstat != 8 | ha$r1mstath != 8), na.rm = TRUE)
subset(ha, r2mnev == 1 & (r2mstat != 8 | r2mstath != 8),
       select = c(r2mstat, r2mstath, r2mnev))
sum(ha$r3mnev == 1 & (ha$r3mstat != 8 | ha$r3mstath != 8), na.rm = TRUE)
subset(ha, r4mnev == 1 & (r4mstat != 8 | r4mstath != 8),
       select = c(r4mstat, r4mstath, r4mnev))
## create raw 6 categories of marital status
ha <- ha %>%
  mutate(
    maritalc2011 = case_when(
      !is.na(r1mstat) ~ r1mstat,
      !is.na(r1mstath) ~ r1mstath,
      r1mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2013 = case_when(
      !is.na(r2mstat) ~ r2mstat,
      !is.na(r2mstath) ~ r2mstath,
      r2mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2015 = case_when(
      !is.na(r3mstat) ~ r3mstat,
      !is.na(r3mstath) ~ r3mstath,
      r3mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2018 = case_when(
      !is.na(r4mstat) ~ r4mstat,
      !is.na(r4mstath) ~ r4mstath,
      r4mnev == 1 ~ 8,
      TRUE ~ NA_real_
    ),
    maritalc2020 = case_when(
      ba011 == 1 ~ 1,  # Married, spouse present
      ba011 == 2 ~ 3,  # Married, spouse absent
      ba011 == 3 ~ 4,  # Separated
      ba011 == 4 ~ 5,  # Divorced
      ba011 == 5 ~ 7,  # Widowed
      ba011 == 6 ~ 8,  # Never married
      TRUE ~ NA_real_
    )
  )

labels <- c(
  "Married, spouse present" = 1,
  "Married, spouse absent" = 3,
  "Separated" = 4,
  "Divorced" = 5,
  "Widowed" = 7,
  "Never married" = 8
)
vars <- paste0("maritalc20", c("11", "13", "15", "18", "20"))
ha <- ha %>%
  mutate(across(all_of(vars), ~ factor(., levels = labels, labels = names(labels))))

# Hukou ####
# Live in urban or rural
# Mapping original variable names to years
vars <- c(h1rural = 2011, h2rural = 2013, h3rural = 2015, h4rural = 2018, urban_nbs = 2020)
ha <- ha %>%
  mutate(across(
    all_of(names(vars)),
    ~ {
      x <- as.numeric(.)                # convert from haven_labelled to numeric
      if (cur_column() != "urban_nbs") x <- 1 - x   # flip coding for h1-h4
      factor(x, levels = c(0, 1), labels = c("Rural communities", "Urban communities"))
    },
    .names = "urban{vars[.col]}"
  ))

# Household size ####
ha <- ha %>%
  mutate(
    hhsize2011 = as.numeric(h1hhres),
    hhsize2013 = as.numeric(h2hhres),
    hhsize2015 = as.numeric(h3hhres),
    hhsize2018 = as.numeric(h4hhres),
    spouse2020 = case_when(
      maritalc2020 == "Married, spouse present" ~ 2,
      !is.na(maritalc2020) ~ 1,
      TRUE ~ NA_real_
    ),
    hhsize2020 = case_when(
      is.na(xhhmembernum) & is.na(spouse2020) ~ NA_real_,
      TRUE ~ rowSums(cbind(xhhmembernum, spouse2020), na.rm = TRUE)
    )
  )
table(ha$hhsize2020) # 0 means the respondent died

## Contact with children ####
# 2011
vars2011_cd003 <- paste0("cd003_", 1:14, "_")
vars2011_cd004 <- paste0("cd004_", 1:14, "_")
a2011_Family_information <- a2011_Family_information %>%
  mutate(child2011 = ha$child2011[match(ID, ha$ID_w1)]) %>%
  mutate(
    across(all_of(vars2011_cd003), ~ na_if(as.numeric(.), 10)),
    across(all_of(vars2011_cd004), ~ na_if(as.numeric(.), 10))
  ) %>%
  mutate(
    childcontactface2011 = do.call(pmin, c(select(., all_of(vars2011_cd003)), na.rm = TRUE)),
    childcontactvirtual2011 = do.call(pmin, c(select(., all_of(vars2011_cd004)), na.rm = TRUE)),
    childcontactface2011 = ifelse(is.infinite(childcontactface2011), NA_real_, childcontactface2011),
    childcontactvirtual2011 = ifelse(is.infinite(childcontactvirtual2011), NA_real_, childcontactvirtual2011),
    childcontactface2011 = ifelse(is.na(childcontactface2011) & child2011 == 0, 9, childcontactface2011),
    childcontactvirtual2011 = ifelse(is.na(childcontactvirtual2011) & child2011 == 0, 9, childcontactvirtual2011)
  )
# 2013
vars2013_cd003 <- paste0("cd003_", 1:11, "_")
vars2013_cd004 <- paste0("cd004_", 1:11, "_")
a2013_Family_information <- a2013_Family_information %>%
  mutate(child2013 = ha$child2013[match(ID, ha$ID)]) %>%
  mutate(
    across(all_of(vars2013_cd003), ~ na_if(as.numeric(.), 10)),
    across(all_of(vars2013_cd004), ~ na_if(as.numeric(.), 10))
  ) %>%
  mutate(
    childcontactface2013 = do.call(pmin, c(select(., all_of(vars2013_cd003)), na.rm = TRUE)),
    childcontactvirtual2013 = do.call(pmin, c(select(., all_of(vars2013_cd004)), na.rm = TRUE)),
    childcontactface2013 = ifelse(is.infinite(childcontactface2013), NA_real_, childcontactface2013),
    childcontactvirtual2013 = ifelse(is.infinite(childcontactvirtual2013), NA_real_, childcontactvirtual2013),
    childcontactface2013 = ifelse(is.na(childcontactface2013) & child2013 == 0, 9, childcontactface2013),
    childcontactvirtual2013 = ifelse(is.na(childcontactvirtual2013) & child2013 == 0, 9, childcontactvirtual2013)
  )
# 2015
vars2015_cd003 <- paste0("cd003_", 1:16, "_")
vars2015_cd004 <- paste0("cd004_", 1:16, "_")

a2015_Family_information <- a2015_Family_information %>%
  mutate(child2015 = ha$child2015[match(ID, ha$ID)]) %>%
  mutate(
    across(all_of(vars2015_cd003), ~ na_if(as.numeric(.), 10)),
    across(all_of(vars2015_cd004), ~ na_if(as.numeric(.), 10))
  ) %>%
  mutate(
    childcontactface2015 = do.call(pmin, c(select(., all_of(vars2015_cd003)), na.rm = TRUE)),
    childcontactvirtual2015 = do.call(pmin, c(select(., all_of(vars2015_cd004)), na.rm = TRUE)),
    childcontactface2015 = ifelse(is.infinite(childcontactface2015), NA_real_, childcontactface2015),
    childcontactvirtual2015 = ifelse(is.infinite(childcontactvirtual2015), NA_real_, childcontactvirtual2015),
    childcontactface2015 = ifelse(is.na(childcontactface2015) & child2015 == 0, 9, childcontactface2015),
    childcontactvirtual2015 = ifelse(is.na(childcontactvirtual2015) & child2015 == 0, 9, childcontactvirtual2015)
  )
# 2018
vars2018_cd003 <- paste0("cd003_", 1:15, "_")
vars2018_cd004 <- paste0("cd004_", 1:9, "_")
a2018_Family_information <- a2018_Family_information %>%
  # bring in child2018 from ha
  mutate(child2018 = ha$child2018[match(ID, ha$ID)]) %>%
  # convert labelled to numeric and replace 10 with NA
  mutate(
    across(all_of(vars2018_cd003), ~ na_if(as.numeric(.), 10)),
    across(all_of(vars2018_cd004), ~ na_if(as.numeric(.), 10))
  ) %>%
  # compute rowwise minimum in a vectorized way
  mutate(
    childcontactface2018 = do.call(pmin, c(select(., all_of(vars2018_cd003)), na.rm = TRUE)),
    childcontactvirtual2018 = do.call(pmin, c(select(., all_of(vars2018_cd004)), na.rm = TRUE))
  ) %>%
  # replace -Inf (result of all NA) with NA
  mutate(
    childcontactface2018 = ifelse(is.infinite(childcontactface2018), NA_real_, childcontactface2018),
    childcontactvirtual2018 = ifelse(is.infinite(childcontactvirtual2018), NA_real_, childcontactvirtual2018)
  ) %>%
  # apply missing rules
  mutate(
    childcontactface2018 = ifelse(is.na(childcontactface2018) & child2018 == 0, 9, childcontactface2018),
    childcontactvirtual2018 = ifelse(is.na(childcontactvirtual2018) & child2018 == 0, 9, childcontactvirtual2018)
  )
# 2020
vars2020_ca015 <- paste0("ca015_", 1:17, "_")
vars2020_ca016 <- paste0("ca016_", 1:17, "_")
ha <- ha %>%
  # convert labelled to numeric and replace 10 with NA
  mutate(
    across(all_of(vars2020_ca015), ~ na_if(as.numeric(.), 10)),
    across(all_of(vars2020_ca016), ~ na_if(as.numeric(.), 10))
  ) %>%
  # compute rowwise minimum efficiently using vectorized pmin
  mutate(
    childcontactface2020 = do.call(pmin, c(select(., all_of(vars2020_ca015)), na.rm = TRUE)),
    childcontactvirtual2020 = do.call(pmin, c(select(., all_of(vars2020_ca016)), na.rm = TRUE)),
    # if all values are NA, pmin returns Inf → convert to NA
    childcontactface2020 = ifelse(is.infinite(childcontactface2020), NA_real_, childcontactface2020),
    childcontactvirtual2020 = ifelse(is.infinite(childcontactvirtual2020), NA_real_, childcontactvirtual2020),
    # apply missing rules: if child2020 == 0 and min is NA, set to 9
    childcontactface2020 = ifelse(is.na(childcontactface2020) & child2020 == 0, 9, childcontactface2020),
    childcontactvirtual2020 = ifelse(is.na(childcontactvirtual2020) & child2020 == 0, 9, childcontactvirtual2020)
  )


# Income, wealth & consumption ####
## wealth. Better No use due to large missing and negative values ####
#Note. Wave 1&2 = household wealth. Wave 3&4 = wealth at the couple level
ha <- ha %>%
  mutate(
    fam_wealth2011 = as.numeric(hh1atotb),
    fam_wealth2013 = as.numeric(hh2atotb),
    fam_wealth2015 = as.numeric(h3atotb),
    fam_wealth2018 = as.numeric(h4atotb),
    # personal wealth for 2011 and 2013
    pcwealth2011 = fam_wealth2011 / hhsize2011,
    pcwealth2013 = fam_wealth2013 / hhsize2013,
    # personal wealth for 2015 and 2018, adjusted by marital status
    pcwealth2015 = case_when(
      maritalc2015 %in% c("Married, spouse present", "Married, spouse absent") ~ fam_wealth2015 / 2,
      maritalc2015 %in% c("Separated", "Divorced", "Widowed", "Never married") ~ fam_wealth2015,
      TRUE ~ NA_real_
    ),
    pcwealth2018 = case_when(
      maritalc2018 %in% c("Married, spouse present", "Married, spouse absent") ~ fam_wealth2018 / 2,
      maritalc2018 %in% c("Separated", "Divorced", "Widowed", "Never married") ~ fam_wealth2018,
      TRUE ~ NA_real_
    )
  )
describe(ha[, c("hhsize2015", "fam_wealth2015", "pcwealth2015",
                "hhsize2018", "fam_wealth2018", "pcwealth2018")], fast = TRUE)

## household income ####
ha <- ha %>% 
  mutate(
    hhincome2011 = as.numeric(hh1itot),
    hhincome2013 = as.numeric(hh2itot),
    hhincome2015 = as.numeric(hh3itot),
    hhincome2018 = as.numeric(hh4itot)
  )

# 2020
## individual income
replace_with_mean <- function(x, minx, maxx) {
  case_when(
    !is.na(x) & x != -1 ~ x, 
    TRUE ~ rowMeans(
      cbind(
        ifelse(minx == -1, NA, minx),
        ifelse(maxx == -1, NA, maxx)
      ), 
      na.rm = TRUE
    )
  )
}

ha$ga002 <- replace_with_mean(ha$ga002, ha$ga002_min, ha$ga002_max)

ha$ga005_1 <- replace_with_mean(ha$ga005_1, ha$ga005_1_min, ha$ga005_1_max)
ha$ga005_1 <- case_when(
  !is.na(ha$ga005_1)                    ~ ha$ga005_1,
  ha$ga005_s1 == 0 | ha$ga005_s10 == 0  ~ 0,
  TRUE                                  ~ NA_real_
)

vars <- paste0("ga005_", 2:9)
for (v in vars) {
  s_col <- paste0(v, "_s")  # corresponding _s column
  ha[[v]] <- ifelse(
    !is.na(ha[[v]]) & ha[[v]] != -1,          # keep if not missing and not -1
    ha[[v]],
    ifelse(ha[[s_col]] == 0 | ha$ga005_s10 == 0, 0, NA_real_)  # check _s column and constant s10
  )
}
bef_vars <- paste0("ga005_", 1:9)
ha <- ha %>%
  mutate(
    inbef = if_else(
      rowSums(!is.na(select(., all_of(bef_vars)))) == 0,  # all missing?
      NA_real_,                                           # then NA
      rowSums(select(., all_of(bef_vars)), na.rm = TRUE) # else sum ignoring missing
    )
  )
describe(ha[, c("inbef", "ga005_1", "ga005_2", "ga005_3", "ga005_4", "ga005_5",
                "ga005_6", "ga005_7", "ga005_8")], fast = TRUE)

## individual earned income
ha <- ha %>%
  mutate(across(c(fc033:fc039, -fc035),
                ~ replace_with_mean(.x,
                                    get(paste0(cur_column(), "_min")),
                                    get(paste0(cur_column(), "_max")))))
describe(ha[, c("fc033", "fc034", "fc035", "fc036", "fc037", "fc038", "fc039")], fast = TRUE)
ha$inearned <- case_when(
  !is.na(ha$fc037) ~ ha$fc037 * 40 * 52, # No. of hours/week * No. of weeks/year
  !is.na(ha$fc036) ~ ha$fc036 * 5 * 52, # No. of days/week * No. of weeks.year
  !is.na(ha$fc035) ~ ha$fc035 * 52,
  !is.na(ha$fc034) ~ ha$fc034 * 12,
  !is.na(ha$fc033) ~ ha$fc033,
  TRUE             ~ NA_real_
)
ha <- ha %>%
  mutate(
    inearned = case_when(
      is.na(inearned) & is.na(fc039) ~ NA_real_,
      TRUE ~ rowSums(cbind(inearned, fc039), na.rm = TRUE)
    )
  )


ha <- ha %>%
  mutate(across(c(fc042_1:fc042_4),
                ~ replace_with_mean(.x,
                                    get(paste0(cur_column(), "_min")),
                                    get(paste0(cur_column(), "_max")))))
ha <- ha %>%
  mutate(
    fc042_1 = coalesce(fc042_1,
                       if_else(fc042_s5 == 0 | fc042_s1 == 0, 0, NA_real_)),
    fc042_2 = coalesce(fc042_2,
                       if_else(fc042_s5 == 0 | fc042_s2 == 0, 0, NA_real_)),
    fc042_3 = coalesce(fc042_3,
                       if_else(fc042_s5 == 0 | fc042_s3 == 0, 0, NA_real_)),
    fc042_3 = coalesce(fc042_4,
                       if_else(fc042_s5 == 0 | fc042_s4 == 0, 0, NA_real_))
  )
ha <- ha %>%
  mutate(
    inotherbef = case_when(
      rowSums(!is.na(select(., fc042_1:fc042_4))) == 0 ~ NA_real_,
      TRUE ~ rowSums(select(., fc042_1:fc042_4), na.rm = TRUE)
    )
  )
describe(ha[, c("fc042_1", "fc042_2", "fc042_3", "fc042_4", "inotherbef")], fast = TRUE)

ha$idunmploy <- case_when(
  !is.na(ha$fg010_1) & !is.na(ha$fg010_2) ~ ha$fg010_1 * ha$fg010_2,
  ha$fg009 == 2                           ~ 0,
  TRUE                                    ~ NA_real_
)

## household wage
hhnet_list <- vector("list", 11)
for (i in 1:11) {
  # Variable names
  gb003_var <- paste0("gb003_", i, "_")
  gb004_var <- paste0("gb004_", i, "_")
  gb005_1_var <- paste0("gb005_1_", i, "_")
  gb005_2_var <- paste0("gb005_2_", i, "_")
  gb005_3_var <- paste0("gb005_3_", i, "_")
  # Initialize ave as NA vector
  ave <- rep(NA, nrow(ha))
  # Only compute average if min/max exist
  if (i %in% c(1:7, 11)) {
    min_var <- paste0("gb003_min_", i, "_")
    max_var <- paste0("gb003_max_", i, "_")
    if (all(c(min_var, max_var) %in% names(ha))) {
      ave <- rowMeans(cbind(
        ifelse(ha[[min_var]] == -1, NA, ha[[min_var]]),
        ifelse(ha[[max_var]] == -1, NA, ha[[max_var]])
      ), na.rm = TRUE)
    }
  }
  # Wage
  hhwage <- ifelse(
    !is.na(ha[[gb003_var]]) & ha[[gb003_var]] != -1,
    ha[[gb003_var]],
    ave
  )
  # Excluded wage
  hhex <- ifelse(
    ha[[gb004_var]] == 2 & ha[[gb005_1_var]] == 2 & !is.na(ha[[gb005_2_var]]) & ha[[gb005_2_var]] != -1,
    ha[[gb005_2_var]],
    ifelse(
      ha[[gb004_var]] == 2 & ha[[gb005_1_var]] == 1 & !is.na(ha[[gb005_1_var]]) & ha[[gb005_1_var]] != -1,
      12 * ha[[gb005_1_var]],
      ifelse(
        ha[[gb004_var]] == 2 & ha[[gb005_1_var]] == 3 & !is.na(ha[[gb005_3_var]]) & ha[[gb005_3_var]] != -1,
        ha[[gb005_3_var]] * hhwage,
        NA
      )
    )
  )
  # Net wage
  hhnet <- ifelse(!is.na(hhwage) & !is.na(hhex), hhwage - hhex, hhwage)
  
  # Ensure hhnet is the correct length
  if (length(hhnet) != nrow(ha)) {
    hhnet <- rep(NA, nrow(ha))
  }
  
  # Store
  hhnet_list[[i]] <- hhnet
  ha[[paste0("hhwagenet", i)]] <- hhnet
}
## Combine and sum
hhnet_matrix <- ha[paste0("hhwagenet", 1:11)]
ha$hhwage_total <- rowSums(hhnet_matrix, na.rm = TRUE)
ha$hhwage_total[apply(is.na(hhnet_matrix), 1, all)] <- NA

## household transfer: hhtransfer
# Step 1: Define all variables to sum
transfer_vars <- c(
  # gb006
  "gb006_1_1_", "gb006_1_2_", "gb006_1_3_", "gb006_1_4_", "gb006_1_5_", "gb006_1_6_", "gb006_1_7_",
  "gb006_2_1_", "gb006_2_2_", "gb006_2_3_", "gb006_2_4_",
  "gb006_3_1_", "gb006_3_2_", "gb006_3_3_", "gb006_3_4_",
  "gb006_4_1_", "gb006_4_2_", "gb006_4_3_", "gb006_4_4_", "gb006_4_5_", "gb006_4_6_",
  "gb006_5_1_", "gb006_5_2_", "gb006_5_3_", "gb006_5_4_",
  "gb006_6_1_", "gb006_6_2_", "gb006_6_3_", "gb006_6_4_",
  "gb006_7_1_", "gb006_7_2_", "gb006_7_3_", "gb006_7_4_", "gb006_7_6_",
  "gb006_8_1_", "gb006_8_2_", "gb006_8_3_", "gb006_8_4_", "gb006_8_5_", "gb006_8_6_", "gb006_8_8_",
  "gb006_9_1_", "gb006_9_2_", "gb006_9_3_", "gb006_9_4_",
  # gb008
  "gb008_7_1_", "gb008_7_2_", "gb008_7_3_", "gb008_7_4_", "gb008_7_5_",
  "gb008_8_1_", "gb008_8_2_", "gb008_8_3_", "gb008_8_4_", "gb008_8_5_", "gb008_8_6_", "gb008_8_7_",
  "gb008_9_1_", "gb008_9_2_", "gb008_9_3_", "gb008_9_4_", "gb008_9_5_", "gb008_9_6_"
)
# Step 2: Replace -1 with NA
ha[transfer_vars] <- lapply(ha[transfer_vars], function(x) ifelse(x == -1, NA, x))
# Step 3: Calculate hhtransfer
ha$hhtransfer <- rowSums(ha[transfer_vars], na.rm = TRUE)
# Step 4: Set hhtransfer to NA if all components are NA
ha$hhtransfer[!rowSums(!is.na(ha[transfer_vars]))] <- NA
s10_flags <- c(
  "gb008_1__s10", "gb008_2__s10", "gb008_3__s10", "gb008_4__s10", "gb008_5__s10", "gb008_6__s10",
  "gb008_7__s10", "gb008_8__s10", "gb008_9__s10", "gb008_10__s10", "gb008_11__s10", "gb008_12__s10",
  "gb006_1__s10", "gb006_2__s10", "gb006_3__s10", "gb006_4__s10", "gb006_5__s10", "gb006_6__s10",
  "gb006_7__s10", "gb006_8__s10", "gb006_9__s10", "gb006_10__s10", "gb006_11__s10", "gb006_12__s10",
  "gb006_13__s10", "gb006_14__s10"
)
# Step 6: If hhtransfer is NA and any _s10 flag == 10, set hhtransfer to 0
ha$hhtransfer <- ifelse(
  is.na(ha$hhtransfer) & rowSums(ha[s10_flags] == 10, na.rm = TRUE) > 0,
  0,
  ha$hhtransfer
)

## household farm, livestock, bussiness: hhflb
ha$gc004_1 <- replace_with_mean(ha$gc004_1, ha$gc004_1_min, ha$gc004_1_max)
ha$gc004_2 <- replace_with_mean(ha$gc004_2, ha$gc004_2_min, ha$gc004_2_max)
ha$gc006_1 <- replace_with_mean(ha$gc006_1, ha$gc006_1_min, ha$gc006_1_max)
ha$gc006_2 <- replace_with_mean(ha$gc006_2, ha$gc006_2_min, ha$gc006_2_max)
ha$gd004_1 <- replace_with_mean(ha$gd004_1, ha$gd004_1_min, ha$gd004_1_max)
ha$gd004_2 <- replace_with_mean(ha$gd004_2, ha$gd004_2_min, ha$gd004_2_max)

make_hhvar <- function(x1, x2, x) {
  case_when(
    !is.na(x1) & x1 != -1 ~ x1,
    !is.na(x2) & x2 != -1 ~ -x2,
    !is.na(x)  & x  == 3  ~ 0,
    TRUE                  ~ NA_real_
  )
}

ha <- ha %>%
  mutate(
    hhfarm = make_hhvar(gc004_1, gc004_2, gc004),
    hhlive = make_hhvar(gc006_1, gc006_2, gc006),
    hhbus  = make_hhvar(gd004_1, gd004_2, gd004)
  )
ha <- ha %>%
  mutate(
    hhflb = if_else(
      rowSums(!is.na(across(c(hhfarm, hhlive, hhbus)))) == 0,
      NA_real_,
      rowSums(across(c(hhfarm, hhlive, hhbus)), na.rm = TRUE)
    )
  )
describe(ha[, c("hhfarm", "hhlive", "hhbus", "hhflb")], fast = TRUE)

## houseld public benefits: hhpublic
ha$hhwubao <- replace_with_mean(ha$ge004_1_, ha$ge004_min_1_, ha$ge004_max_1_)
ha$hhdibao <- replace_with_mean(ha$ge004_2_, ha$ge004_min_2_, ha$ge004_max_2_)
ha$hhtekun <- replace_with_mean(ha$ge004_3_, ha$ge004_min_3_, ha$ge004_max_3_)
ha$hhjiandang <- replace_with_mean(ha$ge004_4_, ha$ge004_min_4_, ha$ge004_max_4_)
ha$hhotherben <- replace_with_mean(ha$ge004_5_, ha$ge004_min_5_, ha$ge004_max_5_)
ha$hhcovid <- replace_with_mean(ha$ge008_1, ha$ge008_1_min, ha$ge008_1_max)
ha$hhland <- replace_with_mean(ha$ge012, ha$ge012_min, ha$ge012_max)
ha$hhhouse <- replace_with_mean(ha$ge013, ha$ge013_min, ha$ge013_max)

replace_ge006 <- function(x, s, s9) {
  case_when(
    !is.na(x) & x != -1 ~ x,
    s == 0 | s9 == 0    ~ 0,
    TRUE                ~ NA_real_
  )
}
ha <- ha %>%
  mutate(across(
    .cols = ge006_1:ge006_8,
    .fns = ~ replace_ge006(.x, get(sub("ge006_", "ge006_s", cur_column())), ge006_s9)
  ))
ha <- ha %>%
  mutate(
    hhothertransfer = case_when(
      rowSums(!is.na(across(ge006_1:ge006_8))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(ge006_1:ge006_8), na.rm = TRUE)
    ),
    hhinsure = case_when(
      !is.na(ge007) & ge007 != -1 ~ ge007,
      TRUE ~ NA_real_
    ),
    hhphot = case_when(
      !is.na(ge011) & ge011 != -1 ~ ge011,
      TRUE ~ NA_real_
    ),
    hhrent = case_when(
      !is.na(ge014_1) & ge014_1 != -1 ~ ge014_1,
      is.na(ge014_1) & ge014 == 2     ~ 0,
      TRUE ~ NA_real_
    )
  )

public_vars <- c(
  "hhwubao", "hhdibao", "hhtekun", "hhjiandang", "hhotherben",
  "hhcovid", "hhland", "hhhouse", "hhothertransfer", "hhinsure",
  "hhphot", "hhrent"
)
ha <- ha %>%
  mutate(
    hhpublic = case_when(
      rowSums(!is.na(across(all_of(public_vars)))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(all_of(public_vars)), na.rm = TRUE)
    )
  )
describe(ha$hhpublic, fast = TRUE)

## household income
hhincome_var <- c("ga002", "inbef", "fc039", "inearned", "inotherbef", "idunmploy",
                  "hhwage_total", "hhtransfer", "hhflb", "hhpublic")
ha <- ha %>%
  mutate(
    hhincome2020 = case_when(
      rowSums(!is.na(across(all_of(hhincome_var)))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(all_of(hhincome_var)), na.rm = TRUE)
    )
  )
describe(ha[, c("hhincome2011", "hhincome2013", "hhincome2015", "hhincome2018", "hhincome2020")], fast = TRUE)


## annual consumption ####
ha <- ha %>% 
  mutate(
    hhconsumption2011 = as.numeric(hh1ctot),
    hhconsumption2013 = as.numeric(hh2ctot),
    hhconsumption2015 = as.numeric(hh3ctot),
    hhconsumption2018 = as.numeric(hh4ctot),
    pcconsumption2011 = as.numeric(hh1cperc),
    pcconsumption2013 = as.numeric(hh2cperc),
    pcconsumption2015 = as.numeric(hh3cperc),
    pcconsumption2018 = as.numeric(hh4cperc)
  )
# 2020
cons_vars <- c("gf001", "gf008", paste0("gf011_", 1:7), paste0("gf013_", 1:16))
ha <- ha %>%
  mutate(across(all_of(cons_vars), ~ na_if(., -1))) %>%
  mutate(gf008 = gf008 * 52) %>%
  mutate(
    hhconsumption2020 = rowSums(across(all_of(cons_vars)), na.rm = TRUE),
    hhconsumption2020 = if_else(rowSums(!is.na(across(all_of(cons_vars)))) == 0, NA_real_, hhconsumption2020),
    pcconsumption2020 = hhconsumption2020 / hhsize2020
  )

rm(list = ls()[!ls() %in% c("ha", ls()[startsWith(ls(), "a20")])])
gc()

# Self-rated health ####
# version 1
ha <- ha %>%
  mutate(
    selfhealth2011 = r1shlta,
    selfhealth2013 = r2shlta,
    selfhealth2015 = r3shlta,
    selfhealth2018 = r4shlta,
    selfhealth2020 = if_else(da001 == 997, NA_real_, da001)
  ) %>%
  # then apply consistent factor labels to all new variables
  mutate(across(
    c(selfhealth2011, selfhealth2013, selfhealth2015, selfhealth2018, selfhealth2020),
    ~ factor(.x,
             levels = 5:1,
             labels = c("Very poor", "Poor", "Fair", "Good", "Very good"))
  ))
# version 2: only wave 1-3
ha <- ha %>%
  mutate(
    selfhealth22011 = r1shlt,
    selfhealth22013 = r2shlt,
    selfhealth22015 = r3shlt
  ) %>%
  mutate(across(
    c(selfhealth22011, selfhealth22013, selfhealth22015),
    ~ factor(.x,
             levels = 5:1,
             labels = c("Poor", "Fair", "Good", "Very good", "Excellent"))
  ))

# ADLs - 6 items ####
adl_vars <- list(
  "2011" = c("r1dressa", "r1batha", "r1eata", "r1beda", "r1toilta", "r1urina"),
  "2013" = c("r2dressa", "r2batha", "r2eata", "r2beda", "r2toilta", "r2urina"),
  "2015" = c("r3dressa", "r3batha", "r3eata", "r3beda", "r3toilta", "r3urina"),
  "2018" = c("r4dressa", "r4batha", "r4eata", "r4beda", "r4toilta", "r4urina")
)
for (year in names(adl_vars)) {
  vars <- adl_vars[[year]]
  # Replace tagged NA ("a: Healthy and young") with 0
  ha <- ha %>%
    mutate(across(all_of(vars),
                  ~ if_else(is_tagged_na(.) & na_tag(.) == "a", 0, .)))
  # Create ADL variable using apply
  ha[[paste0("ADL", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}
# 2020
adl_var <- c("db001", "db003", "db005", "db007", "db009", "db011")
iadl_var <- c("db022", "db020", "db016", "db014", "db012", "db018")
dl_var <- c(adl_var, iadl_var)

for (v in dl_var) {
  ha[[paste0(v, "_d")]] <- ifelse(ha[[v]] == 1, 0,
                                  ifelse(ha[[v]] %in% 2:4, 1, NA))
  ha[[paste0(v, "_d")]] <- ifelse(
    is.na(ha[[paste0(v, "_d")]]) & ha$birth_year > 1970 & 
      ha$selfhealth2020 %in% c("Good", "Very good"), 
    0,
    ha[[paste0(v, "_d")]]
  )
}

adl_d_vars <- paste0(adl_var, "_d")
ha$ADL2020 <- ifelse(
  rowSums(!is.na(ha[adl_d_vars])) == 0,
  NA,
  rowSums(ha[adl_d_vars] == 1, na.rm = TRUE)
)
iadl_d_vars <- paste0(iadl_var, "_d")
ha$IADL2020 <- ifelse(
  rowSums(!is.na(ha[iadl_d_vars])) == 0,
  NA,
  rowSums(ha[iadl_d_vars] == 1, na.rm = TRUE)
)

# IADLs - 6 items ####
iadl_vars <- list(
  "2011" = c("r1moneya", "r1medsa", "r1shopa", "r1mealsa", "r1housewka"),
  "2013" = c("r2moneya", "r2medsa", "r2shopa", "r2mealsa", "r2housewka", "r2phonea"),
  "2015" = c("r3moneya", "r3medsa", "r3shopa", "r3mealsa", "r3housewka", "r3phonea"),
  "2018" = c("r4moneya", "r4medsa", "r4shopa", "r4mealsa", "r4housewka", "r4phonea")
)
for (year in names(iadl_vars)) {
  vars <- iadl_vars[[year]]
  # Create IADL variable using apply
  ha[[paste0("IADL", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}


# Chronic diseases ####
chr_vars <- list(
  "2011" = c("r1hibpe", "r1dyslipe", "r1diabe", "r1cancre", "r1lunge","r1livere",
             "r1hearte", "r1stroke", "r1kidneye", "r1digeste", "r1psyche",
             "r1memrye", "r1arthre", "r1asthmae"),
  "2013" = c("r2hibpe", "r2dyslipe", "r2diabe", "r2cancre", "r2lunge","r2livere",
             "r2hearte", "r2stroke", "r2kidneye", "r2digeste", "r2psyche",
             "r2memrye", "r2arthre", "r2asthmae"),
  "2015" = c("r3hibpe", "r3dyslipe", "r3diabe", "r3cancre", "r3lunge","r3livere",
             "r3hearte", "r3stroke", "r3kidneye", "r3digeste", "r3psyche",
             "r3memrye", "r3arthre", "r3asthmae"),
  "2018" = c("r4hibpe", "r4dyslipe", "r4diabe", "r4cancre", "r4lunge","r4livere",
             "r4hearte", "r4stroke", "r4kidneye", "r4digeste", "r4psyche",
             "r4memrye", "r4arthre", "r4asthmae")
)

for (year in names(chr_vars)) {
  vars <- chr_vars[[year]]
  
  ha[[paste0("chronic", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}
# 2020
for (i in 1:15) {
  da003 <- paste0("da003_", i, "_")
  da002 <- paste0("da002_", i, "_")
  ha[[da003]] <- case_when(
    ha[[da003]] == 2 ~ 0,
    is.na(ha[[da003]]) & ha[[da002]] %in% c(1, 2, 3) ~ 1,
    is.na(ha[[da003]]) & ha[[da002]] == 99 ~ 0,
    TRUE ~ ha[[da003]]
  )
}
ha <- ha %>%
  mutate(
    chronic2020 = case_when(
      rowSums(!is.na(across(da003_1_:da003_15_))) == 0 ~ NA_real_,
      TRUE ~ rowSums(across(da003_1_:da003_15_), na.rm = TRUE)
    )
  )

# Cognition ####
# MMSE
## 2020
im_vars <- paste0("dc012_s", 1:10)
ha[im_vars] <- lapply(ha[im_vars], function(x) case_when(
  !is.na(x) & x > 0 ~ 1,
  !is.na(x)          ~ 0,
  TRUE               ~ NA_real_
))
ha <- ha %>%
  mutate(
    imrecall = case_when(
      rowSums(!is.na(across(all_of(im_vars)))) == 0 ~ NA_real_,
      is.na(rowSums(across(all_of(im_vars)), na.rm = TRUE)) & dc012_s11 == 11 ~ 0,
      TRUE ~ rowSums(across(all_of(im_vars)), na.rm = TRUE)
    )
  )

de_vars <- paste0("dc028_s", 1:10)
ha[de_vars] <- lapply(ha[de_vars], function(x) case_when(
  !is.na(x) & x > 0 ~ 1,
  !is.na(x)          ~ 0,
  TRUE               ~ NA_real_
))
ha <- ha %>%
  mutate(
    derecall = case_when(
      rowSums(!is.na(across(all_of(de_vars)))) == 0 ~ NA_real_,
      is.na(rowSums(across(all_of(de_vars)), na.rm = TRUE)) & dc028_s11 == 11 ~ 0,
      TRUE ~ rowSums(across(all_of(de_vars)), na.rm = TRUE)
    )
  )

ha <- ha %>%
  mutate(
    dc007_1 = case_when(
      dc007_1_1 == 93                   ~ 1,
      dc007_1_1 != 93 | dc007_1 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_2 = case_when(
      dc007_2_1 == 86                   ~ 1,
      dc007_2_1 != 86 | dc007_2 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_3 = case_when(
      dc007_3_1 == 79                   ~ 1,
      dc007_3_1 != 79 | dc007_3 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_4 = case_when(
      dc007_4_1 == 72                   ~ 1,
      dc007_4_1 != 72 | dc007_4 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    dc007_5 = case_when(
      dc007_5_1 == 65                   ~ 1,
      dc007_5_1 != 65 | dc007_5 == 997 ~ 0,
      TRUE                              ~ NA_real_
    ),
    serial = ifelse(
      rowSums(!is.na(across(c(dc007_1, dc007_2, dc007_3, dc007_4, dc007_5)))) == 0,
      NA_real_,
      rowSums(across(c(dc007_1, dc007_2, dc007_3, dc007_4, dc007_5)), na.rm = TRUE)
    )
  )

ha <- ha %>%
  mutate(
    across(c(dc001, dc003, dc004, dc005),
           ~ case_when(
             . == 1           ~ 1,
             . %in% c(2, 997) ~ 0,
             TRUE             ~ NA_real_
           )),
    naming = rowSums(across(c(dc001, dc003, dc004, dc005)), na.rm = TRUE),
    naming = ifelse(rowSums(!is.na(across(c(dc001, dc003, dc004, dc005)))) == 0, NA, naming)
  )

ha$draw <- case_when(
  ha$dc009 == 1           ~ 1,
  ha$dc009 %in% c(2, 997) ~ 0,
  TRUE                    ~ NA_real_
)

## 2011-2020 sum
vars_list <- list(
  "2011" = c("r1imrc", "r1dlrc", "r1ser7", "r1orient", "r1draw"),
  "2013" = c("r2imrc", "r2dlrc", "r2ser7", "r2orient", "r2draw"),
  "2015" = c("r3imrc", "r3dlrc", "r3ser7", "r3orient", "r3draw"),
  "2018" = c("r4imrc", "r4dlrc", "r4ser7", "r4orient", "r4draw"),
  "2020" = c("imrecall", "derecall", "serial", "naming", "draw")
)
for (year in names(vars_list)) {
  vars <- vars_list[[year]]
  ha[[paste0("cognition", year)]] <- ifelse(
    apply(ha[vars], 1, function(x) all(is.na(x))),
    NA_real_,
    rowSums(ha[vars], na.rm = TRUE)
  )
}

ha <- ha %>%
  # 2011
  mutate(
    imrecall2011 = as.numeric(r1imrc),
    derecall2011 = as.numeric(r1dlrc),
    serial2011 = as.numeric(r1ser7),
    orient2011 = as.numeric(r1orient),
    draw2011   = as.numeric(r1draw),
    
    # 2013
    imrecall2013 = as.numeric(r2imrc),
    derecall2013 = as.numeric(r2dlrc),
    serial2013 = as.numeric(r2ser7),
    orient2013 = as.numeric(r2orient),
    draw2013   = as.numeric(r2draw),
    
    # 2015
    imrecall2015 = as.numeric(r3imrc),
    derecall2015 = as.numeric(r3dlrc),
    serial2015 = as.numeric(r3ser7),
    orient2015 = as.numeric(r3orient),
    draw2015   = as.numeric(r3draw),
    
    # 2018
    imrecall2018 = as.numeric(r4imrc),
    derecall2018 = as.numeric(r4dlrc),
    serial2018 = as.numeric(r4ser7),
    orient2018 = as.numeric(r4orient),
    draw2018   = as.numeric(r4draw),
    
    # 2020
    imrecall2020 = as.numeric(imrecall),
    derecall2020 = as.numeric(derecall),
    serial2020 = as.numeric(serial),
    orient2020 = as.numeric(naming),
    draw2020   = as.numeric(draw)
  )


# Physical activity or exercise ####
#Wave 1-3, only half of the respondents were asked about PA questions
ha <- ha %>%
  mutate(
    # vigorous activity
    physicalvg2011 = as.numeric(r1vgact_c),
    physicalvg2013 = as.numeric(r2vgact_c),
    physicalvg2015 = as.numeric(r3vgact_c),
    physicalvg2018 = as.numeric(r4vgact_c),
    physicalvg2020 = ifelse(is.na(da032_1_), NA, as.numeric(da032_1_ == 1)),
    physicalvgday2011 = as.numeric(r1vgactx_c),
    physicalvgday2013 = as.numeric(r2vgactx_c),
    physicalvgday2015 = as.numeric(r3vgactx_c),
    physicalvgday2018 = as.numeric(r4vgactx_c),
    physicalvgday2020 = as.numeric(da033_1_),
    # moderate activity
    physicalmd2011 = as.numeric(r1mdact_c),
    physicalmd2013 = as.numeric(r2mdact_c),
    physicalmd2015 = as.numeric(r3mdact_c),
    physicalmd2018 = as.numeric(r4mdact_c),
    physicalmd2020 = ifelse(is.na(da032_2_), NA, as.numeric(da032_2_ == 1)),
    physicalmdday2011 = as.numeric(r1mdactx_c),
    physicalmdday2013 = as.numeric(r2mdactx_c),
    physicalmdday2015 = as.numeric(r3mdactx_c),
    physicalmdday2018 = as.numeric(r4mdactx_c),
    physicalmdday2020 = as.numeric(da033_2_),
    # light activity
    physicallt2011 = as.numeric(r1ltact_c),
    physicallt2013 = as.numeric(r2ltact_c),
    physicallt2015 = as.numeric(r3ltact_c),
    physicallt2018 = as.numeric(r4ltact_c),
    physicallt2020 = ifelse(is.na(da032_3_), NA, as.numeric(da032_3_ == 1)),
    physicalltday2011 = as.numeric(r1ltactx_c),
    physicalltday2013 = as.numeric(r2ltactx_c),
    physicalltday2015 = as.numeric(r3ltactx_c),
    physicalltday2018 = as.numeric(r4ltactx_c),
    physicalltday2020 = as.numeric(da033_3_)
  )

# Social activities ####
# 2011-2018
ha <- ha %>%
  mutate(
    socialact2011 = r1socwk,
    socialact2013 = r2socwk,
    socialact2015 = r3socwk,
    socialact2018 = r4socwk
  ) %>%
  mutate(
    across(starts_with("socialact20"),
           ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes")))
  )

## helper function to generate social activity vars for a given dataset and year
make_social_vars <- function(df, year) {
  acts <- c("socialfriend", "socialmajong", "socialhelp", "socialclub",
            "socialcom", "socialcharity", "socialsick", "socialedu", "socialother")
  
  base_prefix <- if (year == 2018) "da056_s" else "da056s"
  
  for (i in seq_along(acts)) {
    col <- paste0(base_prefix, if (i == 9) "11" else i)
    # extract number after the last non-digit character
    num <- as.numeric(sub(".*?(\\d+)$", "\\1", col))
    newvar <- paste0(acts[i], year)
    df[[newvar]] <- ifelse(!is.na(df[[col]]) & df[[col]] == num, 1, 0)
  }
  
  # frequency vars
  for (i in seq_along(acts)) {
    col <- paste0("da057_", if (i == 9) "11" else i, "_")
    newvar <- paste0(acts[i], "_f", year)
    df[[newvar]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  df
}
## apply to each dataset
a2011_D_Health_status_and_functioning <- make_social_vars(a2011_D_Health_status_and_functioning, 2011)
a2013_D_Health_status_and_functioning <- make_social_vars(a2013_D_Health_status_and_functioning, 2013)
a2015_D_Health_status_and_functioning <- make_social_vars(a2015_D_Health_status_and_functioning, 2015)
a2018_D_Health_status_and_functioning <- make_social_vars(a2018_D_Health_status_and_functioning, 2018)

# 2020
social_var <- paste0("da038_s", 1:8)
ha <- ha %>%
  mutate(
    across(all_of(social_var),
           ~ ifelse(.x > 0, 1,
                    ifelse(is.na(.x) & da038_s9 == 9, 0, .x))),
    socialact2020 = case_when(
      if_all(all_of(social_var), is.na) ~ NA_character_,
      if_any(all_of(social_var), ~ .x == 1) ~ "Yes",
      TRUE ~ "No"
    ),
    socialact2020 = factor(socialact2020, levels = c("No", "Yes")),
    # explicit numeric versions
    socialfriend2020  = as.numeric(da038_s1),
    socialmajong2020  = as.numeric(da038_s2),
    socialhelp2020    = as.numeric(da038_s3),
    socialclub2020    = as.numeric(da038_s4),
    socialcom2020     = as.numeric(da038_s5),
    socialcharity2020 = as.numeric(da038_s6),
    socialedu2020     = as.numeric(da038_s7),
    socialother2020   = as.numeric(da038_s8),
    # frequency vars
    socialfriend_f2020  = as.numeric(da039_1_),
    socialmajong_f2020  = as.numeric(da039_2_),
    socialhelp_f2020    = as.numeric(da039_3_),
    socialclub_f2020    = as.numeric(da039_4_),
    socialcom_f2020     = as.numeric(da039_5_),
    socialcharity_f2020 = as.numeric(da039_6_),
    socialedu_f2020     = as.numeric(da039_7_),
    socialother_f2020   = as.numeric(da039_8_)
  )

# Smoking & drinking ####
ha <- ha %>%
  mutate(
    # ever smoke
    smokeever2011 = as.numeric(r1smokev),
    smokeever2013 = as.numeric(r2smokev),
    smokeever2015 = as.numeric(r3smokev),
    smokeever2018 = as.numeric(r4smokev),
    smokeever2020 = case_when(
      da046 == 2 ~ 0,
      da046 == 1 ~ 1,
      TRUE       ~ NA_real_
    ),
    # smoke now
    smoke2011 = as.numeric(r1smoken),
    smoke2013 = as.numeric(r2smoken),
    smoke2015 = as.numeric(r3smoken),
    smoke2018 = as.numeric(r4smoken),
    smoke2020 = case_when(
      da047 %in% c(2, 3) ~ 0,
      da047 == 1         ~ 1,
      TRUE               ~ NA_real_
    ),
    # ever drink
    drinkever2011 = as.numeric(r1drinkev),
    drinkever2013 = as.numeric(r2drinkev),
    drinkever2015 = as.numeric(r3drinkev),
    drinkever2018 = as.numeric(r4drinkev),
    # drink last year
    drinkl2011 = as.numeric(r1drinkl),
    drinkl2013 = as.numeric(r2drinkl),
    drinkl2015 = as.numeric(r3drinkl),
    drinkl2018 = as.numeric(r4drinkl),
    drinkl2020 = case_when(
      da051 %in% c(1, 2)                             ~ 1,
      da051 == 3 | (is.na(da051) & vc015 == 6)      ~ 0,
      TRUE                                           ~ NA_real_
    )
  )

# CESD ####
cesd_var <- c("dc016","dc017","dc018","dc019","dc020","dc021","dc022","dc023","dc024","dc025")
reverse_var <- c("dc020","dc023")

ha <- ha %>%
  mutate(
    # c2011-2018
    depress2011 = as.numeric(r1cesd10),
    depress2013 = as.numeric(r2cesd10),
    depress2015 = as.numeric(r3cesd10),
    depress2018 = as.numeric(r4cesd10),
    # 2020: reverse-score dc020 and dc023 for 2020, set 997/999 to NA
    across(all_of(reverse_var), ~ ifelse(.x %in% c(997, 999), NA, ifelse(.x %in% 1:4, 4 - .x, .x))),
    # 2020: recode other CESD items to 0-3 for 2020, set 997/999 to NA
    across(setdiff(cesd_var, reverse_var), ~ ifelse(.x %in% c(997, 999), NA, ifelse(.x %in% 1:4, .x - 1, .x))),
    # 2020: compute total depression score for 2020 in one line
    depress2020 = ifelse(rowSums(!is.na(across(all_of(cesd_var)))) == 0, NA,
                         rowSums(across(all_of(cesd_var)), na.rm = TRUE))
  )

## loneliness and sleep quality ####
ha <- ha %>%
  mutate(
    # loneliness
    lone2011 = as.numeric(r1flonel) - 1,
    lone2013 = as.numeric(r2flonel) - 1,
    lone2015 = as.numeric(r3flonel) - 1,
    lone2018 = as.numeric(r4flonel) - 1,
    lone2020 = as.numeric(dc024),
   # sleep quality
   sleepquality2011 = as.numeric(r1sleeprl) - 1,
   sleepquality2013 = as.numeric(r2sleeprl) - 1,
   sleepquality2015 = as.numeric(r3sleeprl) - 1,
   sleepquality2018 = as.numeric(r4sleeprl) - 1,
   sleepquality2020 = as.numeric(dc022)
  )

## sleep duration ####
a2011_D_Health_status_and_functioning <- a2011_D_Health_status_and_functioning %>%
  mutate(
    nightsleep2011 = ifelse(da049 < 0 , NA, as.numeric(da049)),
    napsleep2011   = ifelse(da050 < 0, NA, as.numeric(da050))
  )
a2013_D_Health_status_and_functioning <- a2013_D_Health_status_and_functioning %>%
  mutate(
    nightsleep2013 = ifelse(da049 < 0 , NA, as.numeric(da049)),
    napsleep2013   = ifelse(da050 < 0, NA, as.numeric(da050))
  )
a2015_D_Health_status_and_functioning <- a2015_D_Health_status_and_functioning %>%
  mutate(
    nightsleep2015 = ifelse(da049 < 0 , NA, as.numeric(da049)),
    napsleep2015   = ifelse(da050 < 0, NA, as.numeric(da050))
  )
a2018_D_Health_status_and_functioning <- a2018_D_Health_status_and_functioning %>%
  mutate(
    nightsleep2018 = ifelse(da049 < 0 , NA, as.numeric(da049)),
    napsleep2018   = ifelse(da050 < 0, NA, as.numeric(da050))
  )
ha <- ha %>%
  mutate(
    nightsleep2020 = ifelse(da030 == -1, NA, as.numeric(da030)),
    napsleep2020   = ifelse(da031 == -1, NA, as.numeric(da031))
  )

# Interview status ####
ha <- ha %>% 
  mutate(
    inw1 = ifelse(!is.na(r1iwstat) & r1iwstat == 1, "Yes", "No"),
    inw2 = ifelse(!is.na(r2iwstat) & r2iwstat == 1, "Yes", "No"),
    inw3 = ifelse(!is.na(r3iwstat) & r3iwstat == 1, "Yes", "No"),
    inw4 = ifelse(!is.na(r4iwstat) & r4iwstat == 1, "Yes", "No"),
    inw5 = ifelse(!is.na(iyear) & iyear == 2020, "Yes", "No")
  )

#==============================================================================#
# Variables not available from the Harmonized dataset
#==============================================================================#
# fuel ####
a2011_I_Housing_characteristics <- a2011_I_Housing_characteristics %>%
  mutate(
    cookfuel2011 = case_when(
      i022 == 7 ~ 8,
      TRUE ~ as.numeric(i022)
    ),
    heatfuel2011 = as.numeric(i021)
  )

a2013_I_Housing_characteristics <- a2013_I_Housing_characteristics %>%
  mutate(
    cookfuel2013 = case_when(
      i022 == 7 ~ 8,
      TRUE ~ as.numeric(i022)
    ),
    heatfuel2013 = as.numeric(i021)
  )

a2015_I_Housing_characteristics <- a2015_I_Housing_characteristics %>%
  mutate(
    cookfuel2015 = case_when(
      i022 == 7 ~ 8,
      TRUE ~ as.numeric(i022)
    ),
    heatfuel2015 = as.numeric(i021)
  )

a2018_Ha_I_Housing <- a2018_Ha_I_Housing %>%
  mutate(
    cookfuel2018 = case_when(
      i022_w4 == 8 ~ 9,
      i022_w4 == 7 ~ 8,
      TRUE ~ as.numeric(i022_w4)
    ),
    heatfuel2018 = as.numeric(i021_w4)
  )

ha <- ha %>%
  mutate(
    cookfuel2020 = as.numeric(i021),
    heatfuel2020 = case_when(
      i020 == 8 ~ 9,   # Don't heat
      i019 == 1 ~ 8,   # Concentrated heating 统一供暖
      TRUE ~ as.numeric(i020)
    )
  )


heatfuel_labels <- c(
  "Solar",                         # 1
  "Coal",                          # 2
  "Natural gas",                   # 3
  "Liquefied Petroleum Gas",       # 4
  "Electric",                      # 5
  "Crop residue/Wood burning",     # 6
  "Other",                         # 7
  "Concentration heating",         # 8
  "Don't heat"                     # 9
)
cookfuel_labels <- c(
  "Coal",                         # 1
  "Natural gas",                  # 2
  "Marsh gas",                    # 3
  "Liquefied Petroleum Gas",      # 4
  "Electric",                     # 5
  "Crop residue/Wood burning",    # 6
  "Solar",                        # 7
  "Other",                        # 8
  "Don't cook"                    # 9
)

a2011_I_Housing_characteristics$heatfuel2011 <- factor(
  a2011_I_Housing_characteristics$heatfuel2011,
  levels = 1:9,
  labels = heatfuel_labels
)
a2013_I_Housing_characteristics$heatfuel2013 <- factor(
  a2013_I_Housing_characteristics$heatfuel2013,
  levels = 1:9,
  labels = heatfuel_labels
)
a2015_I_Housing_characteristics$heatfuel2015 <- factor(
  a2015_I_Housing_characteristics$heatfuel2015,
  levels = 1:9,
  labels = heatfuel_labels
)
a2018_Ha_I_Housing$heatfuel2018 <- factor(
  a2018_Ha_I_Housing$heatfuel2018,
  levels = 1:9,
  labels = heatfuel_labels
)
ha$heatfuel2020 <- factor(
  ha$heatfuel2020,
  levels = 1:9,
  labels = heatfuel_labels
)

a2011_I_Housing_characteristics$cookfuel2011 <- factor(
  a2011_I_Housing_characteristics$cookfuel2011,
  levels = 1:9,
  labels = cookfuel_labels
)
a2013_I_Housing_characteristics$cookfuel2013 <- factor(
  a2013_I_Housing_characteristics$cookfuel2013,
  levels = 1:9,
  labels = cookfuel_labels
)
a2015_I_Housing_characteristics$cookfuel2015 <- factor(
  a2015_I_Housing_characteristics$cookfuel2015,
  levels = 1:9,
  labels = cookfuel_labels
)
a2018_Ha_I_Housing$cookfuel2018 <- factor(
  a2018_Ha_I_Housing$cookfuel2018,
  levels = 1:9,
  labels = cookfuel_labels
)
ha$cookfuel2020 <- factor(
  ha$cookfuel2020,
  levels = 1:9,
  labels = cookfuel_labels
)

# merge with each wave's dataset ####
# keep using variables
ha_subset <- ha %>% 
  select(ID, householdID, communityID, hhid, hhidc, ID_w1, householdID_w1, r1iwm, r1iwy,
         birth_year, sex, educ, educl, inw1, inw2, inw3, inw4, inw5, province, city,
         ends_with(c("2011", "2013", "2015", "2018", "2020")))

# merge with variables unavailable in the Harmonized dataset
ha_subset <- ha_subset %>%
  # housing
  left_join(a2011_I_Housing_characteristics %>% select(householdID, ends_with("2011")), join_by(householdID_w1 == householdID)) %>%
  left_join(a2013_I_Housing_characteristics %>% select(ID, ends_with("2013")), join_by(ID)) %>%
  left_join(a2015_I_Housing_characteristics %>% select(ID, ends_with("2015")), join_by(ID)) %>%
  left_join(a2018_Ha_I_Housing %>% select(householdID, ends_with("2018")), join_by(householdID)) %>%
  # health and functioning
  left_join(a2011_D_Health_status_and_functioning %>% select(ID, ends_with("2011")), join_by(ID_w1 == ID)) %>%
  left_join(a2013_D_Health_status_and_functioning %>% select(ID, ends_with("2013")), join_by(ID)) %>%
  left_join(a2015_D_Health_status_and_functioning %>% select(ID, ends_with("2015")), join_by(ID)) %>%
  left_join(a2018_D_Health_status_and_functioning %>% select(ID, ends_with("2018")), join_by(ID)) %>% 
  # health insurance
  left_join(a2011_E_Health_care_and_insurance %>% select(ID, ends_with("2011")), join_by(ID_w1 == ID)) %>%
  left_join(a2013_E_Health_care_and_insurance %>% select(ID, ends_with("2013")), join_by(ID)) %>%
  left_join(a2015_E_Health_care_and_insurance %>% select(ID, ends_with("2015")), join_by(ID)) %>%
  left_join(a2018_E_Health_care_and_insurance %>% select(ID, ends_with("2018")), join_by(ID)) %>%
  # family information
  left_join(
    a2011_Family_information %>% select(ID, ends_with("2011"), -starts_with("child201")),
    join_by(ID_w1 == ID)
  ) %>%
  left_join(
    a2013_Family_information %>% select(ID, ends_with("2013"), -starts_with("child201")),
    join_by(ID)
  ) %>%
  left_join(
    a2015_Family_information %>% select(ID, ends_with("2015"), -starts_with("child201")),
    join_by(ID)
  ) %>%
  left_join(
    a2018_Family_information %>% select(ID, ends_with("2018"), -starts_with("child201")),
    join_by(ID)
  )
# check duplicates
anyDuplicated(ha_subset$ID) # it should = 0
names(ha_subset)[grepl("\\.(x|y)$", names(ha_subset))] # character(0)

# order variable names
fixed_order <- c("ID", "householdID", "communityID", "birth_year", "sex", "educ", "educl")
all_vars <- names(ha_subset)
year_vars <- all_vars[grepl("2011$|2013$|2015$|2018$|2020$", all_vars)]
sorted_year_vars <- year_vars[order(substr(year_vars, 1, nchar(year_vars) - 4))]
end_vars <- c("province", "city", "ID_w1", "householdID_w1", "r1iwm", "r1iwy",
              "inw1", "inw2", "inw3", "inw4", "inw5", "hhidc")
final_order <- c(fixed_order, sorted_year_vars, end_vars)
ha_subset <- ha_subset[, final_order]

## Reshape dataset ####
year_suffixes <- c("2011","2013","2015","2018","2020")
long <- ha_subset %>%
  pivot_longer(
    cols = ends_with(year_suffixes),
    names_to = c(".value", "year"),   # .value keeps the base variable name
    names_pattern = "(.*)(20\\d{2})"  # splits variable into name + year
  ) %>%
  mutate(year = as.integer(year))

long <- long %>% 
  mutate(age = year - birth_year)

# remove used datasets
rm(list = ls()[!ls() %in% c("long", "ha", ls()[startsWith(ls(), "a20")])])
gc()

# City code and covariates ####
sum(is.na(long$city))
sum(is.na(long$province))
long <- rename(long,
               cityname = city,
               provincename = province)

# Create a named vector: names = cityname, values = city code
city_map <- c(
  "上海市" = 310000,
  "上饶市" = 361100,
  "临汾市" = 141000,
  "临沂市" = 371300,
  "临沧市" = 530900,
  "临沧"   = 530900,
  "丽水市" = 331100,
  "九江市" = 360400,
  "亳州市" = 341600,
  "佛山市" = 440600,
  "佳木斯市" = 230800,
  "保定市" = 130600,
  "保山市" = 530500,
  "信阳市" = 411500,
  "六安市" = 341500,
  "兰州市" = 620100,
  "兴安盟" = 152200,
  "内江市" = 511000,
  "凉山彝族自治州" = 513400,
  "北京"   = 110000,
  "南充市" = 511300,
  "南宁市" = 450100,
  "南昌市" = 360100,
  "台州市" = 331000,
  "吉安市" = 360800,
  "吉林市" = 220200,
  "周口市" = 411600,
  "呼伦贝尔市" = 150700,
  "呼和浩特市" = 150100,
  "哈尔滨市" = 230100,
  "哈尔滨"   = 230100,
  "嘉兴市" = 330400,
  "四平市" = 220300,
  "大连市" = 210200,
  "天津"   = 120000,
  "威海市" = 371000,
  "娄底市" = 431300,
  "宁德市" = 350900,
  "宁波市" = 330200,
  "安庆市" = 340800,
  "安阳市" = 410500,
  "定西市" = 621100,
  "宜宾市" = 511500,
  "宜春市" = 360900,
  "宝鸡市" = 610300,
  "宿州市" = 341300,
  "宿迁市" = 321300,
  "岳阳市" = 430600,
  "巢湖市" = 341400,
  "常德市" = 430700,
  "平凉市" = 620800,
  "平顶山市" = 410400,
  "广安市" = 511600,
  "广州市" = 440100,
  "张掖市" = 620700,
  "徐州市" = 320300,
  "德州市" = 371400,
  "忻州市" = 140900,
  "恩施土家族苗族自治州" = 422800,
  "成都市" = 510100,
  "扬州市" = 321000,
  "承德市" = 130800,
  "昆明市" = 530100,
  "昭通市" = 530600,
  "景德镇市" = 360200,
  "朝阳市" = 211300,
  "本溪市" = 210500,
  "杭州市" = 330100,
  "枣庄市" = 370400,
  "桂林市" = 450300,
  "楚雄彝族自治州" = 532300,
  "榆林市" = 610800,
  "汉中市" = 610700,
  "江门市" = 440700,
  "沧州市" = 130900,
  "河池市" = 451200,
  "泰州市" = 321200,
  "洛阳市" = 410300,
  "济南市" = 370100,
  "海东地区" = 630200,
  "淮南市" = 340400,
  "深圳市" = 440300,
  "清远市" = 441800,
  "渭南市" = 610500,
  "湖州市" = 330500,
  "滨州市" = 371600,
  "漳州市" = 350600,
  "潍坊市" = 370700,
  "潮州市" = 445100,
  "濮阳市" = 410900,
  "焦作市" = 410800,
  "玉林市" = 450900,
  "甘孜藏族自治州" = 513300,
  "益阳市" = 430900,
  "盐城市" = 320900,
  "眉山市" = 511400,
  "石家庄市" = 130100,
  "福州市" = 350100,
  "绵阳市" = 510700,
  "聊城市" = 371500,
  "苏州市" = 320500,
  "茂名市" = 440900,
  "荆门市" = 420800,
  "莆田市" = 350300,
  "襄樊市" = 420600,
  "资阳市" = 512000,
  "赣州市" = 360700,
  "赤峰市" = 150400,
  "运城市" = 140800,
  "连云港市" = 320700,
  "邵阳市" = 430500,
  "郑州市" = 410100,
  "重庆市" = 500000,
  "锡林郭勒盟" = 152500,
  "锦州市" = 210700,
  "长沙市" = 430100,
  "阜阳市" = 341200,
  "阳泉市" = 140300,
  "阿克苏地区" = 652900,
  "青岛市" = 370200,
  "鞍山市" = 210300,
  "鸡西市" = 230300,
  "黄冈市" = 421100,
  "黔东南苗族侗族自治州" = 522600,
  "黔南布依族苗族自治州" = 522700,
  "齐齐哈尔市" = 230200,
  "丽江市" = 530700
)
# Assign codes using lookup
long$city <- city_map[long$cityname]
# how many are missing?
sum(is.na(long$city))

## City-level covariates ####
macro <- read_dta("data directory/City_covariate_cn2021.dta") # Win directory

# make a correction table
corrections <- tribble(
  ~city,   ~year, ~PGDP_new,                     ~pop_new,                ~exp_base,
  152200, 2013,   415.34/168.37,                 1.6837,                  166.74/(100*1.6837),
  152200, 2015,   3.1391,                        1.5991,                  227.54/(100*1.5991),
  152200, 2018,   2.9419,                        1.6079,                  264.79/(100*1.6079),
  152200, 2020,   4.2702/(1+6.3/100),            1.4132,                  286.98/(100*1.4132),
  513400, 2013,   2.0463,                        2.65927,                 16.1559/2.65927,
  513400, 2015,   2.8276,                        5.1178,                  417.32/(100*5.1178),
  513400, 2018,   3.1472,                        5.2994,                  681.71/(100*5.2994),
  513400, 2020,   1733.15/(5.3312*100),          5.3312,                  708.09/(100*5.3312),
  341400, 2013,   6.1555,                        7.611,                   630.89/(100*7.611),
  341400, 2015,   3.6429,                        0.788,                   4.07601,
  341400, 2018,   5.898,                         0.7962,                  50.6823/(100*0.7962),
  341400, 2020,   (7.9369 - 1.0849),             (72.8 - 0.08)/100,       66.8/(100*0.7272),
  422800, 2013,   1.6697,                        4.0542,                  216.69/(100*4.0542),
  422800, 2015,   2.0191,                        3.327,                   332.5/(100*3.327),
  422800, 2018,   2.5848,                        3.378,                   391.76/(100*3.378),
  422800, 2020,   3.8011/(1+11.7/100),           4.0222,                  470.19/(100*4.0222),
  532300, 2013,   2436426/516555,                0.516555,                301758/516555,
  532300, 2015,   2.7942,                        2.733,                   216.23/(100*2.733),
  532300, 2018,   3.7303,                        2.748,                   275.91/(100*2.748),
  532300, 2020,   6.6893/(1+12.5/100),           2.391,                   293.61/(100*2.391),
  610700, 2015,   3.1407,                        3.4381,                  258.6/(100*3.4381),
  610700, 2018,   4.2754,                        3.4361,                  342/(100*3.4361),
  610700, 2020,   5.5279/(1+9.5/100),            3.1893,                  371.4/(100*3.1893),
  513300, 2013,   201.22/113.78,                 1.1378,                  273.33/(100*1.1378),
  513300, 2015,   1.8423,                        1.1649,                  316.43/(100*1.1649),
  513300, 2018,   2.4446,                        1.196,                   420.52/(100*1.196),
  513300, 2020,   3.6931,                        1.107,                   453.88/(100*1.107),
  152500, 2013,   8.679,                         1.0389,              185.5934/103.89,
  152500, 2015,   5.6843,                        1.0426,                  232.19/104.26,
  152500, 2018,   6.9413,                        1.0548,                  259.62/105.48,
  152500, 2020,   7.6097/(1+6.3/100),             1.108,                   304.42/110.8,
  652900, 2013,   3.1087,                        0.05092,                 273554/(0.05092*1000000),
  652900, 2015,   3.6762,                        0.508,                   266.17/(100*0.508),
  652900, 2018,   3.9845,                        0.551112,                425.02/(100*0.551112),
  652900, 2020,   4.2531,                        0.56588,                 551.27/100/0.56588,
  522600, 2013,   1.6838,                        3.4834,                  280.25/100/3.4834,
  522600, 2015,   2.3311,                        3.4854,                  353.68/348.54,
  522600, 2018,   2.9358,                        3.5383,                  403.78/353.83,
  522600, 2020,   3.3464/(1+5.3/100),            3.7603,                  464.57/376.03,
  522700, 2013,   1.9981,                        3.230769,                229.38/100/3.230769,
  522700, 2015,   2.7888,                        902.91 * 100 / 27888,    308.93/323.7629,
  522700, 2018,   3.9965,                        1313.46 * 100 / 39965,   416.61/328.6526,
  522700, 2020,   4.5654,                        1595.4 * 100 / 45654,    441.41/100/(1595.4 * 100 / 45654)
)

# join and update
macro <- macro %>%
  full_join(corrections, by = c("city", "year")) %>%
  mutate(
    PGDP = coalesce(PGDP_new, PGDP),
    pop = coalesce(pop_new, pop),
    expenditure = coalesce(exp_base, expenditure)
  ) %>%
  select(-PGDP_new, -pop_new, -exp_base)

## merge CHARLS and macro data ####
an <- long %>% 
  left_join(macro, by = c("city", "year"))
sum(is.na(an$ID))
sum(is.na(an$pop)) # still missing city covariates
table(an$year, useNA = "always")

any(table(an[c("ID","year")]) > 1) # duplicates r ID year
sum(is.na(an$ID) | trimws(an$ID) == "") # No. of missing ID

# interview status
an <- an %>%
  mutate(
    inw = case_when(
      year == 2011 ~ as.character(inw1),
      year == 2013 ~ as.character(inw2),
      year == 2015 ~ as.character(inw3),
      year == 2018 ~ as.character(inw4),
      year == 2020 ~ as.character(inw5),
      TRUE ~ NA_character_
    )
  )

# Macro-level data ####
## Policy timing ####
policy <- read_excel("policy.xls")
table(policy$NKEFZ, useNA = "always")
policy <- policy %>% 
  select(-ends_with("name"))
# extend missing year
policy$year <- 2011
policy <-  policy %>%
  complete(city, year = 2011:2020) %>%
  group_by(city) %>%
  fill(NKEFZ, lowcarbon, .direction = "downup") %>%
  ungroup()

# merge
an <- an %>% 
  left_join(policy, by = c("city", "year"))

# write dataset ####
# check unique ID and observations
nrow(an) # n = 129410
length(unique(an$ID)) # Total respondents = 25,882

# write data
write_dta(an, "an.dta")