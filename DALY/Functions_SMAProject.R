## Define custom functions
#--------------------------------------
# Imputation using the "previously" entry
fillPrev <- function(x, prev_key, yes_key, no_key = NA) {
  ## This function assumes input data is grouped an sorted (descending time)
  # Make sure the correct keys in the dataset is given as argument
  try(if (is.na(prev_key)) stop("Input value pointing to previously"))
  try(if (is.na(yes_key)) stop("Input value pointing to positive result"))
  # Find entries for which "previous is put in - excluding entry 1
  yrs_prev <- which(x == prev_key)
  yrs_prev <- yrs_prev[yrs_prev!=1]
  # Exit if no entries are found
  if (length(yrs_prev) == 0) return(x)
  # for every key, impute previous entries until first non-NA value
  for (j in yrs_prev) { 
    for (i in j:1) {
      if (!is.na(x[i]) & (x[i] == yes_key | x[i] == no_key)) {break}
      if (is.na(x[i])) {x[i] <- yes_key}
    }}
  # Output the imputed vector back
  return(x)
}

# -----------------------------------------
# Function assigning SMA specific weights based on SV variables
funWeightSMA <- function(x) {
  # This function takes pre-specified weights to translate severity variables
  x %>% mutate(YLD_PULM = case_when(SV_PULM == "Severe" ~ 0.418,
                                    SV_PULM == "Moderate" ~ 0.284,
                                    SV_PULM == "Mild" ~ 0.025,
                                    .default = 0)) %>%
    mutate(YLD_SCOL = case_when(SV_SCOL == "Severe" ~ 0.372,
                                .default = 0)) %>%
    mutate(YLD_MOT = case_when(SV_MOT == "Severe" ~ 0.402,
                               SV_MOT == "Moderate" ~ 0.061,
                               SV_MOT == "mild" ~ 0.010,
                               .default = 0)) %>%
    mutate(YLD_MSK = case_when(SV_MSK == "Ankle" ~ 0.079,
                               SV_MSK == "Leg" ~ 0.165,
                               SV_MSK == "hand" ~ 0.028,
                               SV_MSK == "Arm" ~ 0.117,
                               SV_MSK == "Body_m" ~ 0.317,
                               SV_MSK == "Body_s" ~ 0.581,
                               .default = 0)) %>%
    mutate(YLD_SPEECH = case_when(SV_SPEECH == "Severe" ~ 0.051,
                                  .default = 0)) %>%
    mutate(YLD_FEED = case_when(SV_FEED == "Severe" ~ 0.163,    #NOTE: weight taken from EU-weights
                                .default = 0)) %>%
    # LOWER 95% CI                            
    mutate(YLD_PULM_L = case_when(SV_PULM == "Severe" ~ 0.273,
                                  SV_PULM == "Moderate" ~ 0.153,
                                  SV_PULM == "Mild" ~ 0.011,
                                  .default = 0)) %>%
    mutate(YLD_SCOL_L = case_when(SV_SCOL == "Severe" ~ 0.250,
                                  .default = 0)) %>%
    mutate(YLD_MOT_L = case_when(SV_MOT == "Severe" ~ 0.268,
                                 SV_MOT == "Moderate" ~ 0.040,
                                 SV_MOT == "mild" ~ 0.005,
                                 .default = 0)) %>%
    mutate(YLD_MSK_L = case_when(SV_MSK == "Ankle" ~ 0.054,
                                 SV_MSK == "Leg" ~ 0.112,
                                 SV_MSK == "hand" ~ 0.017,
                                 SV_MSK == "Arm" ~ 0.080,
                                 SV_MSK == "Body_m" ~ 0.216,
                                 SV_MSK == "Body_s" ~ 0.403,
                                 .default = 0)) %>%
    mutate(YLD_SPEECH_L = case_when(SV_SPEECH == "Severe" ~ 0.032,
                                    .default = 0)) %>%
    mutate(YLD_FEED_L = case_when(SV_FEED == "Severe" ~ 0.131,    #NOTE: weight taken from EU-weights
                                  .default = 0)) %>%
    # UPPER 95% CI
    mutate(YLD_PULM_U = case_when(SV_PULM == "Severe" ~ 0.556,
                                  SV_PULM == "Moderate" ~ 0.310,
                                  SV_PULM == "Mild" ~ 0.033,
                                  .default = 0)) %>%
    mutate(YLD_SCOL_U = case_when(SV_SCOL == "Severe" ~ 0.506,
                                  .default = 0)) %>%
    mutate(YLD_MOT_U = case_when(SV_MOT == "Severe" ~ 0.545,
                                 SV_MOT == "Moderate" ~  0.089,
                                 SV_MOT == "mild" ~ 0.019,
                                 .default = 0)) %>%
    mutate(YLD_MSK_U = case_when(SV_MSK == "Ankle" ~  0.110,
                                 SV_MSK == "Leg" ~  0.232,
                                 SV_MSK == "hand" ~  0.045,
                                 SV_MSK == "Arm" ~  0.163,
                                 SV_MSK == "Body_m" ~  0.440,
                                 SV_MSK == "Body_s" ~  0.739,
                                 .default = 0)) %>%
    mutate(YLD_SPEECH_U = case_when(SV_SPEECH == "Severe" ~ 0.078,
                                    .default = 0)) %>%
    mutate(YLD_FEED_U = case_when(SV_FEED == "Severe" ~ 0.198,    #NOTE: weight taken from EU-weights
                                  .default = 0))
}

weightLotSMA <- function(sv, state) {
  ## Helper function to extract disability weight by health state and severity
  ## Use this to acess the weight table to avoid getting NA output
  try(if (!(sv %in% 1:6)) stop("Input severity as an integer between 1-6"))
  try(if (!(state %in% c("MOT", "PULM", "MSK", "SCOL", "SPEECH", "FEED")))
                                     stop("Input valid state from:
                                     MOT, PULM, MSK, SCOL, SPEECH, FEED"))
  # Extract the weight from the table given the input
  if (state %in% c("SCOL", "SPEECH", "FEED")) {sv <- 1}
  if (state %in% c("MOT", "PULM") & sv > 3) {sv <- sv - 3}
  wt <- weightsSMA[sv, state]
  wt
}

      
funYLDSMA <- function(x) {
  x %>%
  # Calculate DALY based on weights (Takes all variables for SMA)
    mutate(YLD_PAT = 1 - (1-YLD_PULM)*(1-YLD_SCOL)*(1-YLD_MOT)*(1-YLD_MSK)*
           (1-YLD_SPEECH)*(1-YLD_FEED)) %>%
    
    mutate(YLD_PAT_NC = YLD_PULM + YLD_SCOL + YLD_MOT + YLD_MSK + YLD_SPEECH + YLD_FEED) %>%
    
    mutate(YLD_PAT_L = 1 - (1-YLD_PULM_L)*(1-YLD_SCOL_L)*(1-YLD_MOT_L)*
             (1-YLD_MSK_L)*(1-YLD_SPEECH_L)*(1-YLD_FEED_L)) %>%
    
    mutate(YLD_PAT_NC_L = YLD_PULM_L + YLD_SCOL_L + YLD_MOT_L + YLD_MSK_L
           + YLD_SPEECH_L + YLD_FEED_L) %>%
    
    mutate(YLD_PAT_U = 1 - (1-YLD_PULM_U)*(1-YLD_SCOL_U)*(1-YLD_MOT_U)*
             (1-YLD_MSK_U)*(1-YLD_FEED_U)*(1-YLD_SPEECH_U)) %>%
    
    mutate(YLD_PAT_NC_U = YLD_PULM_U + YLD_SCOL_U + YLD_MOT_U +
             YLD_MSK_U + YLD_SPEECH_U + YLD_FEED_U)
  
  
}



