# --- Load Packages ---
library(missForest)
library(dplyr)
library(readr)
library(mice)
library(corrplot)

# --- Set Global Options ---
options(digits = 10, scipen = 999)

# --- Load Dataset ---
df_raw <- read_csv("../../../../data/finaldatasets/testdata/finalfr/finalfrfr/jittered_eng_cleaned.csv")

# --- Select Relevant Columns (no 'year') ---
df_subset <- df_raw %>%
  select(-c(id,	Data.ID,	State.Region.County.Province,
	Continent,	Latitude..N.S.,	Longitude..E.W., Location.source, start_date,	end_date, group_id))


df_subset <- df_subset[, colMeans(is.na(df_subset)) <= 0.5]


summary(df_subset)

###########################Evaluation metrices ###########################################

#########Assessing missingness #####################

#assess missingness
par(cex =0.5)
md.pattern(df_subset)
par(cex = 1)

#choose only numeric values
numeric_df <- df_subset[, sapply(df_subset, is.numeric)]
#drop coords and year
numeric_df <- numeric_df[, !(names(numeric_df) %in% c("lon", "lat", "year"))]


# Compute correlation matrix
cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs", method = "pearson", )

# View it
print(cor_matrix)


corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


#######################################################################

## -------------------------
## CONFIG (tweak if needed)
## -------------------------
SEED <- 123
HI_CARD_THRESHOLD <- 100   # ≥ this many levels => treat as "high-cardinality"
MIN_COR <- 0.10            # quickpred: minimum absolute correlation to include a predictor
MIN_PUC <- 0.25            # quickpred: min proportion of usable cases

# RF (randomForest) speed/complexity knobs (passed through mice(...))
RF_NTREE  <- 200
RF_MTRY   <- NULL          # if NULL, we'll set to floor(sqrt(p))
RF_NODESZ <- 5
RF_MAXN   <- 100
RF_IMPORT <- FALSE         # importance=FALSE is faster

# Candidate names for spatial columns present in your data
SPATIAL_CANDIDATES <- c("Conversion.for.latitude","Conversion.for.longitude")

# Small set of "key predictors" we allow when imputing a high-card factor
KEY_PRED_CANDIDATES <- c(SPATIAL_CANDIDATES, "Grain.yield..tons.ha.1.")

## -------------------------
## 0) Type hygiene
## -------------------------
df_subset <- df_subset %>%
  mutate(across(where(is.character), ~ factor(.x)))

## -------------------------
## 1) Methods vector (RF everywhere that needs imputing)
## -------------------------
meth <- make.method(df_subset)
needs_imp <- names(which(colSums(is.na(df_subset)) > 0))

# Use RF for every variable that actually has NA; leave others as "" (no imputation)
meth[needs_imp] <- "rf"
meth[setdiff(names(meth), needs_imp)] <- ""

## -------------------------
## 2) Start with a pruned predictor matrix
##    (keeps the graph reasonable but preserves structure)
## -------------------------
pred <- quickpred(df_subset, mincor = MIN_COR, minpuc = MIN_PUC)

## -------------------------
## 3) Identify high-cardinality factor columns
## -------------------------
is_fac <- sapply(df_subset, is.factor)
nlev   <- if (any(is_fac)) sapply(df_subset[, is_fac, drop = FALSE], nlevels) else integer(0)
hi_card <- names(nlev[nlev >= HI_CARD_THRESHOLD])

## -------------------------
## 4) Restrict predictor roles to keep runtime sane
## -------------------------

# 4a) High-card factors remain in the dataset,
#     but we do NOT allow them to be used as predictors for *other* targets
if (length(hi_card)) {
  pred[, hi_card] <- 0
}

# 4b) Ensure spatial predictors are used for ALL targets that need imputing (keeps spatial patterns)
spatial_cols <- intersect(SPATIAL_CANDIDATES, colnames(df_subset))
if (length(spatial_cols)) {
  pred[needs_imp, spatial_cols] <- 1
}

# 4c) When imputing a high-card factor itself, only allow a compact, relevant predictor set
key_preds <- intersect(KEY_PRED_CANDIDATES, colnames(df_subset))
if (length(hi_card)) {
  for (v in hi_card) {
    # Start from a clean row: nothing predicts v...
    pred[v, ] <- 0
    # ...except the curated spatial + key covariates
    if (length(key_preds)) pred[v, key_preds] <- 1
  }
}

## -------------------------
## 5) Diagnostics (prints only)
## -------------------------
message("MICE (RF-only) setup:")
message(" - Variables with NA to impute: ", length(needs_imp))
message(" - High-card factors (≥ ", HI_CARD_THRESHOLD, " levels): ",
        ifelse(length(hi_card), paste0(length(hi_card), " [", paste(hi_card, collapse = ", "), "]"), "none"))
message(" - Spatial columns used: ", ifelse(length(spatial_cols), paste(spatial_cols, collapse = ", "), "none"))
message(" - Key predictors for high-card targets: ",
        ifelse(length(key_preds), paste(key_preds, collapse = ", "), "none"))

## -------------------------
## 6) Run MICE — 1 iteration, RF everywhere that needs imputing
## -------------------------
set.seed(SEED)

# If mtry not specified, default to sqrt(p) where p = number of predictors available
if (is.null(RF_MTRY)) {
  RF_MTRY <- max(1, floor(sqrt(ncol(df_subset))))
}

imputed_data <- mice(
  df_subset,
  m = 1,                    # single completed dataset
  maxit = 1,                # ONE pass 
  method = meth,
  predictorMatrix = pred,
  printFlag = TRUE,
  
  # ---- forwarded to randomForest::randomForest by method="rf" ----
  ntree = RF_NTREE,
  mtry = RF_MTRY,
  nodesize = RF_NODESZ,
  maxnodes = RF_MAXN,
  importance = RF_IMPORT
)

## -------------------------
## 7) Get the completed data (same variable names/order)
## -------------------------
completed_data <- complete(imputed_data, action = 1)


#save dataset 
write.csv(completed_data, "../../../data/finaldatasets/testdata/imputation/imputed_dataset.csv", row.names = FALSE)



