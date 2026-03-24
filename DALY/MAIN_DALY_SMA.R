## Code to calculate YLL, YLD, DALY
## and extract summary tables for downstream analysis
# ----------------------------------------------------
# Setup env, var, functions from external files
source(file = "Setup_Env_Conf.R")
source(file = "Setup_Env.R")
source(file = "Functions_SMAProject.R")
# ----------------------------------------------------
# Load child tables
Table_main <- read_excel(BNMDR_SMA_PATH, sheet = 2)
Table_therapy <- read_excel(BNMDR_SMA_PATH, sheet = 3)
Table_CoughAssist <- read_excel(BNMDR_SMA_PATH, sheet = 4)
Table_nutrition <- read_excel(BNMDR_SMA_PATH, sheet = 8)
Table_ventilation_NIV <- read_excel(BNMDR_SMA_PATH, sheet = 9)
Table_hospitalisations <- read_excel(BNMDR_SMA_PATH, sheet = 12)
Table_comorbidities <- read_excel(BNMDR_SMA_PATH, sheet = 13)
Table_acitvlim <- read_excel(BNMDR_SMA_PATH, sheet = 14)
# ------------------------------------------------------------
# Format and impute nutrition table
Table_nutrition <- Table_nutrition %>%
  # Extract the dates from their string format
  mutate(DT_START_YR_Nutr = as.integer(substr(Table_nutrition[["DT_NGT_START"]], 6, 9)),
         DT_STOP_YR_Nutr = as.integer(substr(Table_nutrition[["DT_NGT_STP"]], 6, 9)) ) %>%
  # Remove duplicate entries
  group_by(across(all_of(c("IDC_PAT", "DT_START_YR_Nutr", "DT_STOP_YR_Nutr")))) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  distinct()
# ---------------------------------------------------------------
# Prepare ventilation sub-table for merging and DALY
Table_ventilation_NIV <- Table_ventilation_NIV %>%
  group_by(across(all_of(c("IDC_PAT", "newdate")))) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  ## Encode all non-informative entries as NA
  group_by(IDC_PAT) %>%
  arrange(TX_REGN_CD) %>%
  mutate(status = case_when(status == "Unknown" ~ NA,
                            .default = status)) %>%
  mutate(status_inv = case_when(status_inv == "Unknown" ~ NA,
                                .default = status_inv)) %>%
  # Impute first by using the "previous" entries
  dplyr::mutate(status = fillPrev(status,
                        prev_key = "Previously", yes_key = "Currently",
                        no_key = "Never")) %>%
  dplyr::mutate(status_inv = fillPrev(status_inv,
                        prev_key = "Previously", yes_key = "Currently",
                        no_key = "Never")) %>%
  # Impute by forward-propagating last known entry
  # ASSUMPTION: status is stable if physician does not enter new info
  fill(status, .direction = "down") %>%
  fill(status_inv, .direction = "down") %>%
  ungroup() %>%
  ## Remove variables wich are contained in the main table for joining purposes
  select(!c("TX_REGN_CD", "IDC_PAT")) %>%
  distinct()
# -----------------------------------------------------------------------
Table_main <- Table_main %>%
  # Define date of birth, death and age (at death)
  mutate(DT_BT_YR = as.integer(substr(DT_PAT_DOB, 6, 9))) %>%
  mutate(DT_DT_YR = as.integer(substr(DT_PAT_DOD, 6, 9))) %>%
  mutate(DT_PAT_AGE = - DT_BT_YR + TX_REGN_CD + 2016) %>%
  mutate(AAD = DT_DT_YR - DT_BT_YR) %>%
  # Linkage of variables which changed definition to keep 1 variable
  mutate(CD_PAT_HP_SIT_NO_SUPP = case_when(TX_REGN_CD < 6 ~ case_when(
  CD_SIT_WO_SUPP == 2 ~ 3,
  CD_SIT_WO_SUPP == 3 ~ 2
  , .default = CD_SIT_WO_SUPP),
  .default = CD_PAT_HP_SIT_NO_SUPP)) %>%
  mutate(CD_WALK_NO_ASSTNC = case_when(TX_REGN_CD < 6 ~ case_when(
    CD_WALK_ALONE == 2 ~ 1,
    CD_WALK_ALONE == 3 ~ 0,
    CD_WALK_ALONE == 1 ~ 0
    , .default = CD_WALK_ALONE),
    .default = CD_WALK_NO_ASSTNC)) %>%
  mutate(CD_EVR_NGT = case_when(TX_REGN_CD < 6 ~ case_when(
    FL_NGT_CEXCL | FL_NGT_CSUP ~ 3,
    (FL_NGT_PREXCL | FL_NGT_PRSUP) & !(FL_NGT_CEXCL | FL_NGT_CSUP) ~ 2,
    FL_NGT_NEVR & !(FL_NGT_CEXCL | FL_NGT_CSUP) & !(FL_NGT_PREXCL | FL_NGT_PRSUP) ~ 1,
    FL_NGT_UNK == T ~ 99
    , .default = NA), 
    .default = CD_EVR_NGT)) %>%
  mutate(CD_CAD = case_when(TX_REGN_CD < 6 ~ case_when(
    CD_ASSTNC_ACL_SECMOB == 1 ~ 3,
    CD_ASSTNC_ACL_SECMOB == 0 ~ 1
    , .default = CD_ASSTNC_ACL_SECMOB),
    .default = CD_CAD)) %>%
  
  # REDISTRIBUTION of SMA types to keep them stable over time
  ## First confirmed diagnosis is kept
  group_by(IDC_PAT) %>% 
  dplyr::mutate(CD_SMA_TPE = case_when(CD_SMA_TPE == 5 ~ min(CD_SMA_TPE),
                                       CD_SMA_TPE == 99 ~ min(CD_SMA_TPE),
                                       T ~ CD_SMA_TPE)) %>%
  dplyr::mutate(CD_SMA_TPE = ifelse(CD_SMA_TPE < 5, first(CD_SMA_TPE),
                                    CD_SMA_TPE)) %>%
  ungroup() %>%

  ## Imputation of 99s and NAs for analysis variables
  # group and sort data by patient and year
  group_by(IDC_PAT) %>%
  dplyr::arrange(TX_REGN_CD, .by_group = TRUE) %>%
  fill(CD_SMN2_CPY_NR, .direction = "updown") %>%
  # set all unknown values to NA for interpolation
  dplyr::mutate(CD_SMN2_CPY_NR = first(CD_SMN2_CPY_NR)) %>%
  dplyr::mutate(CD_ELBOW_CONTR = ifelse(CD_ELBOW_CONTR == 99, NA,
                                        CD_ELBOW_CONTR)) %>%
  dplyr::mutate(CD_SHLDR_CONTR = ifelse(CD_SHLDR_CONTR == 99, NA,
                                        CD_SHLDR_CONTR)) %>%
  dplyr::mutate(CD_WRIST_CONTR = ifelse(CD_WRIST_CONTR == 99, NA,
                                        CD_WRIST_CONTR)) %>%
  dplyr::mutate(CD_ANKLE_CONTR = ifelse(CD_ANKLE_CONTR == 99, NA,
                                        CD_ANKLE_CONTR)) %>%
  dplyr::mutate(CD_FINGR_CONTR = ifelse(CD_FINGR_CONTR == 99, NA,
                                        CD_FINGR_CONTR)) %>%
  dplyr::mutate(CD_HIP_CONTR = ifelse(CD_HIP_CONTR == 99, NA,
                                      CD_HIP_CONTR)) %>%
  dplyr::mutate(CD_DIAGS_SCOL = ifelse(CD_DIAGS_SCOL == 99, NA,
                                       CD_DIAGS_SCOL)) %>%
  dplyr::mutate(CD_DIAGS_SCOL = ifelse(CD_CAD == 99, NA,
                                       CD_CAD)) %>%
  dplyr::mutate(CD_WALK_NO_ASSTNC = ifelse(CD_WALK_NO_ASSTNC == 99, NA,
                                           CD_WALK_NO_ASSTNC)) %>%
  dplyr::mutate(CD_EVR_NGT = ifelse(CD_EVR_NGT == 99, NA,
                                    CD_EVR_NGT)) %>%
  dplyr::mutate(CD_SPEECH_DIFFCT = ifelse(CD_SPEECH_DIFFCT == 99, NA,
                                          CD_SPEECH_DIFFCT)) %>%
  # apply fillPrev function where "previous" exists
  dplyr::mutate(CD_EVR_NGT = fillPrev(CD_EVR_NGT,
                                      prev_key = 2, yes_key = 3, no_key = 1)) %>%
  dplyr::mutate(CD_CAD = fillPrev(CD_CAD,
                                  prev_key = 2, yes_key = 3, no_key = 1)) %>%
  # full remaining NAs using a downward scheme where possible
  # relies on properly sorted data
  fill(CD_SPEECH_DIFFCT, .direction = "down") %>%
  fill(CD_CAD, .direction = "down") %>%
  fill(CD_EVR_NGT, .direction = "down") %>%
  fill(CD_WALK_NO_ASSTNC, .direction = "down") %>%
  fill(CD_DIAGS_SCOL, .direction = "down") %>%
  fill(CD_ELBOW_CONTR, .direction = "down") %>%
  fill(CD_HIP_CONTR, .direction = "down") %>%
  fill(CD_WRIST_CONTR, .direction = "down") %>%
  fill(CD_FINGR_CONTR, .direction = "down") %>%
  fill(CD_SHLDR_CONTR, .direction = "down") %>%
  fill(CD_ANKLE_CONTR, .direction = "down") %>%
  ungroup()
# ------------------------------------------------------------
# Merging all necessary tables and in place DALY estimation
merge_lst <- list(Table_main, Table_therapy,
                  Table_nutrition, 
                  Table_ventilation_NIV)
# Merge tables by left join on main
T_lst <- merge_lst %>%
  reduce(left_join, by = "TXT_WRKFLOW_IDN") %>%
# Group by patient and sort to impute final NAs (where not possible in their own table)
  group_by(IDC_PAT) %>%
  arrange(TX_REGN_CD) %>%
  fill(status, .direction = "down") %>%
  fill(status_inv, .direction = "down") %>%
# ASSIGNMENT OF Severity weights
  dplyr::mutate(SV_PULM = case_when(
  (status == "Currently" | status_inv == "currently") ~ "Severe",
  !(status == "Currently" | status_inv == "currently") & CD_CAD == 3 ~ "Moderate"
  , .default = NA)) %>%
  dplyr::mutate(SV_SCOL = case_when(
    CD_DIAGS_SCOL == 1 ~ "Severe"
    , .default = NA)) %>%
  dplyr::mutate(SV_MOT = case_when(
    CD_WALK_NO_ASSTNC == 0 ~ "Severe"
    , .default = NA)) %>%
  dplyr::mutate(SV_MSK = case_when(
    (CD_ANKLE_CONTR == 1 & CD_HIP_CONTR == 0 &
       CD_SHLDR_CONTR == 0 & CD_ELBOW_CONTR == 0) ~ "Ankle",
    (CD_HIP_CONTR == 1 &
       CD_SHLDR_CONTR == 0 & CD_ELBOW_CONTR == 0) ~ "Leg",
    (CD_ANKLE_CONTR == 0 & CD_HIP_CONTR == 0 &
       CD_SHLDR_CONTR == 0 & CD_ELBOW_CONTR == 0 &
       (CD_FINGR_CONTR == 1 | CD_WRIST_CONTR == 1)) ~ "Hand",
    (CD_ANKLE_CONTR == 0 & CD_HIP_CONTR == 0 &
       (CD_SHLDR_CONTR == 1 | CD_ELBOW_CONTR == 1)) ~ "Arm",
    ((CD_ANKLE_CONTR == 1 | CD_HIP_CONTR == 1) &
       (CD_SHLDR_CONTR == 1 | CD_ELBOW_CONTR == 1) &
       (CD_ANKLE_CONTR + CD_HIP_CONTR +
          CD_SHLDR_CONTR + CD_ELBOW_CONTR < 3)) ~ "Body_m",
    (CD_ANKLE_CONTR + CD_HIP_CONTR +
       CD_SHLDR_CONTR + CD_ELBOW_CONTR >= 3) ~ "Body_s"
    
    , .default = NA)) %>%
  dplyr::mutate(SV_SPEECH = case_when(
    CD_SPEECH_DIFFCT > 2 ~ "Severe"
    , .default = NA)) %>%
  dplyr::mutate(SV_FEED = case_when(
    CD_EVR_NGT == 3 ~ "Severe"
    , .default = NA)) %>%
  
  # Estimate YLL, YLD, DALY in main table
  dplyr::mutate(YLL_PAT = BeBOD::rsle2021(AAD)) %>%
  dplyr::mutate(YLL_PAT = ifelse(is.na(YLL_PAT), 0, YLL_PAT)) %>%

  # Factorise and label key variables where needed
  mutate(CD_SMA_TPE = factor(x = CD_SMA_TPE,
                           levels = c(1, 2, 3, 4, 5, 99),
                           labels = c("SMA type 1", "SMA type 2", "SMA type 3", "SMA type 4", "Undifined screening", "Unknown"))) %>%
  # split in one table per year (to facilitate year-specific queries and estimates)
  ungroup() # %>%

  # Code to split by year in case this would facilitate analysis
#  group_by(TX_REGN_CD) %>%
#  group_split()
# Name each table by year (reference, year 1 should be 2017)
#names(T_lst) <- 2017:2024

# -------------------------------------------------------------------
# Apply functions for YLD and estimate DALY
T_lst <- funWeightSMA(T_lst)
T_lst <- funYLDSMA(T_lst)
T_lst <- T_lst %>% mutate(DALY_PAT = YLL_PAT + YLD_PAT)

# ------------------------------------------------------------
# TODO write new part to summarize and extract YLDs properly
