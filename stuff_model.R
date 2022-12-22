library(tidyverse)

data <- read.csv("statcast_20_21.csv")

data <- data %>%
  select(-spin_dir, -umpire, -sv_id, -ends_with("deprecated")) %>%
  # Give each pitch a unique ID
  mutate(pitch_id = row_number(),
         # Create pitch type groupings
         pitch_type_condensed = case_when(pitch_type %in% c("KC","CS") ~ "CU",
                                          pitch_type == "FS" ~ "CH",
                                          pitch_type == "FA" ~ "FF",
                                          pitch_type %in% c("KN","EP","SC","") ~ "NA",
                                          TRUE ~ pitch_type),
         # Combine ball and strike count
         count = paste(balls, strikes, sep = "-"),
         end_count = case_when(type == "X" ~ count,
                               type == "B" ~ paste((balls+1), strikes, sep = "-"),
                               type == "S" & strikes == 2 & description == "foul" ~ count,
                               TRUE ~ paste(balls, (strikes+1), sep = "-")),
         pfx_x = ifelse(p_throws == "L", pfx_x * -1, pfx_x),
         release_pos_x = ifelse(p_throws == "L", release_pos_x * -1, release_pos_x),
         plate_x = ifelse(p_throws == "L", plate_x * -1, plate_x),
         spin_axis = ifelse(p_throws == "L", 360 - spin_axis, spin_axis)) %>%
  filter(pitch_type_condensed != "NA") %>%
  select(pitch_id, everything())

# 80/20 train-test split
smp_size <- floor(0.8 * nrow(data))
set.seed(42)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

## ------------------------------------------------------------------------------------------------

# Select only BBE from training data
bbe_df <- train %>% filter(!is.na(estimated_woba_using_speedangle))
unique(bbe_df$count)

# Remove pitches with incorrect count due to umpire mistake
bbe_df <- bbe_df %>% filter(balls < 4 & strikes < 3)
unique(bbe_df$count)

count_xwOBA <- bbe_df %>% group_by(count) %>%
  summarize(xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE))

## ------------------------------------------------------------------------------------------------

# Create target y variable: delta_xwOBA
train <- train %>%
  # Create start_xwOBA
  left_join(count_xwOBA, by = "count") %>%
  rename(start_xwOBA = xwOBA) %>%
  # Create end_xwOBA
  left_join(count_xwOBA %>%
              rename(end_count = count), by = "end_count") %>%
  rename(end_xwOBA = xwOBA) %>%
  mutate(end_xwOBA = case_when(type == "X" ~ estimated_woba_using_speedangle,
                               events %in% c("walk","hit_by_pitch","catcher_interf",
                                             "strikeout","strikeout_double_play") ~ woba_value,
                               TRUE ~ end_xwOBA),
         delta_xwOBA = end_xwOBA - start_xwOBA) %>% filter(!is.na(delta_xwOBA))

## ------------------------------------------------------------------------------------------------

# Do the same with the testing set
test <- test %>%
  # Create start_xwOBA
  left_join(count_xwOBA, by = "count") %>%
  rename(start_xwOBA = xwOBA) %>%
  # Create end_xwOBA
  left_join(count_xwOBA %>%
              rename(end_count = count), by = "end_count") %>%
  rename(end_xwOBA = xwOBA) %>%
  mutate(end_xwOBA = case_when(type == "X" ~ estimated_woba_using_speedangle,
                               events %in% c("walk","hit_by_pitch","catcher_interf",
                                             "strikeout","strikeout_double_play") ~ woba_value,
                               TRUE ~ end_xwOBA),
         delta_xwOBA = end_xwOBA - start_xwOBA) %>% filter(!is.na(delta_xwOBA))

## ------------------------------------------------------------------------------------------------

library(xgboost)
# Features that go in the model
features <- c("pitch_type_condensed","release_speed",
              "pfx_x","pfx_z","release_extension")

# Prepare the Training set to train the model
train_feature <- train %>% select(pitch_id, all_of(features), delta_xwOBA)
anyNA(train_feature)
train_feature <- drop_na(train_feature)

# Prepare the Testing set to test the model
test_feature <- test %>% select(pitch_id, all_of(features), delta_xwOBA)
anyNA(test_feature)
test_feature <- drop_na(test_feature)

## ------------------------------------------------------------------------------------------------

for (pitch_type in unique(train_feature$pitch_type_condensed)) {
  
  train_pitch <- train_feature %>%
    filter(pitch_type_condensed == pitch_type) %>%
    select(-pitch_type_condensed)
  
  train_data <- as.matrix(train_pitch %>% select(-pitch_id, -delta_xwOBA))
  train_label <- as.matrix(train_pitch %>% select(delta_xwOBA))
  xgb_train <- xgb.DMatrix(data = train_data, label = train_label)
  
  #xgb_model <- xgb.cv(data = xgb_train, max.depth = 6, nrounds = 100, nfold = 5,
  #                    early_stopping_rounds = 3, objective = "reg:squarederror")
  
  xgb_model_pitch <- xgboost(data = xgb_train, max.depth = 6,
                             nrounds = 5, objective = "reg:squarederror")
  
  assign(paste0("xgb_model_", tolower(pitch_type)), xgb_model_pitch, envir = .GlobalEnv)
  
  train_pitch <- cbind(train_pitch, data.frame(pred_xwOBA = predict(xgb_model_pitch,
                                                                    newdata = train_data)))
  
  assign(paste0("train_", tolower(pitch_type)), train_pitch, envir = .GlobalEnv)
  
  test_pitch <- test_feature %>%
    filter(pitch_type_condensed == pitch_type) %>%
    select(-pitch_type_condensed)
  
  test_data <- as.matrix(test_pitch %>% select(-pitch_id, -delta_xwOBA))
  
  test_pitch <- cbind(test_pitch, data.frame(pred_xwOBA = predict(xgb_model_pitch,
                                                                  newdata = test_data)))
  
  assign(paste0("test_", tolower(pitch_type)), test_pitch, envir = .GlobalEnv)
  
}

## ------------------------------------------------------------------------------------------------

# Combine
train_all <- rbind(train_ff, train_si, train_fc, train_sl, train_cu, train_ch)
train <- train %>%
  left_join(train_all %>% select(pitch_id, pred_xwOBA), by = "pitch_id") %>%
  group_by(pitch_type_condensed) %>%
  mutate(stuff_plus = (((pred_xwOBA - mean(pred_xwOBA, na.rm = T)) / sd(pred_xwOBA, na.rm = T)) - 1) * -100) %>%
  ungroup() %>% filter(!is.na(pred_xwOBA))

averages <- train %>%
  group_by(pitch_type_condensed) %>%
  summarize(avg_stuff = mean(pred_xwOBA, na.rm = TRUE),
            sd_stuff = sd(pred_xwOBA, na.rm = TRUE))

write.csv(averages, file = "model_stats.csv", row.names = FALSE)

saveRDS(xgb_model_ff, "models/fff_model.rds")
saveRDS(xgb_model_si, "models/si_model.rds")
saveRDS(xgb_model_fc, "models/fc_model.rds")
saveRDS(xgb_model_sl, "models/sl_model.rds")
saveRDS(xgb_model_cu, "models/cu_model.rds")
saveRDS(xgb_model_ch, "models/ch_model.rds")
