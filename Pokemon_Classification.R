# This is a datacamp project 
# https://projects.datacamp.com/projects/712
# The goaal is to understand what makes a pokemon legendary

# Load the tidyverse
library(tidyverse)
# Import the dataset and convert variables
pokedex <- read_csv("datasets/pokedex.csv", 
                    col_types = cols(name = col_factor(), 
                                     type = col_factor(),
                                     is_legendary = col_factor()))

# Look at the first six rows
head(pokedex)

# Examine the structure
str(pokedex)

# Count legendary pokemons
# Prepare the data
legendary_pokemon <- pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / nrow(pokedex))

# Print the data frame
legendary_pokemon


# Prepare the plot
legend_by_heightweight_plot <- pokedex %>% 
  ggplot(aes(x = height_m, y = weight_kg)) +
  geom_point(aes(color = is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m > 7.5 | weight_kg > 600, as.character(name), '')), 
            vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x = 16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokémon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))

# Print the plot
legend_by_heightweight_plot



# Legendary pokemon by type
# Prepare the data
legend_by_type <- pokedex %>% 
  group_by(type) %>% 
  mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
  summarise(prop_legendary = mean(is_legendary)) %>% 
  ungroup() %>% 
  mutate(type = fct_reorder(type, prop_legendary))

# Prepare the plot
legend_by_type_plot <- legend_by_type %>% 
  ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
  geom_col() +
  labs(title = "Legendary Pokemon by type") +
  coord_flip() +
  guides(fill = FALSE)

# Print the plot
legend_by_type_plot


# Boxplot
# Prepare the data
legend_by_stats <- pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = "fght_stats", value = "value", -is_legendary) 
head(legend_by_stats)
# Prepare the plot
legend_by_stats_plot <- legend_by_stats %>% 
  ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap(~fght_stats) +
  labs(title = "Pokemon fight statistics",
       x = "Legendary status") +
  guides(fill = "none")
# suprisingly some legendary pokemons are really week

# Print the plot
legend_by_stats_plot




# Set seed for reproducibility
set.seed(1234)

# Save number of rows in dataset
n = nrow(pokedex)

# Generate 60% sample of rows
sample_rows <- sample(n, size = 0.6*n)

#Create training set
pokedex_train <- pokedex  %>% 
  filter(row_number() %in% sample_rows)

# Create test set
pokedex_test <- pokedex  %>% 
  filter(!row_number() %in% sample_rows)


# I will fit a random forest tree but before doing that I want to fit a simple classiication decision tree. 
# I will use that as my baseline to compare other results


# Load packages and set seed
library(rpart)
library(rpart.plot)
set.seed(1234)

# Fit decision tree
model_tree <- rpart(is_legendary ~ attack + defense + height_m + 
                      hp + sp_attack + sp_defense + speed + type + weight_kg,
                    data = pokedex_train,
                    method = "class",
                    na.action = na.omit)

# Plot decision tree
rpart.plot(model_tree)


# Load package and set seed
library(randomForest)
set.seed(1234)

# Fit random forest
model_forest <- randomForest(is_legendary ~ attack + defense + height_m + 
                               hp + sp_attack + sp_defense + speed + type + weight_kg,
                             data = pokedex_train,
                             importance = TRUE,
                             na.action = na.omit)

# Print model output
model_forest

# Looking at the model output, we can see that the random forest has an out-of-bag (OOB) error of ~7%. 
# However, since there are 24 true positives and 24 false negatives, the model only has a recall of 50%, 
# which means that it struggles to successfully retrieve every legendary Pokémon in the dataset.


# In order to allow direct comparison with the decision tree, we will plot the ROC curves for both models using the ROCR package, 
# which will visualize their true positive rate (TPR) and false positive rate (FPR) respectively. 
# The closer the curve is to the top left of the plot, the higher the area under the curve (AUC) and the better the model.

# Load the ROCR package
library(ROCR)

# Create prediction and performance objects for the decision tree
probs_tree <- predict(model_tree, pokedex_test, type = "prob")
pred_tree <- prediction(probs_tree[,2], pokedex_test$is_legendary)
perf_tree <- performance(pred_tree, "tpr", "fpr")

# Create prediction and performance objects for the random forest
probs_forest <- predict(model_forest, pokedex_test, type = "prob")
pred_forest <- prediction(probs_forest[,2], pokedex_test$is_legendary)
perf_forest <- performance(pred_forest, "tpr", "fpr")

# Plot the ROC curves: first for the decision tree, then for the random forest
plot(perf_tree, col = "red", main = "ROC curves")
plot(perf_forest, add = TRUE, col = "blue")
legend(x = "bottomright",  legend = c("Decision Tree", "Random Forest"), fill = c("red", "blue"))


# Analyze variable importance
# Print variable importance measures
importance_forest <- importance(model_forest)
importance_forest

# Create a dotchart of variable importance
varImpPlot_forest <- varImpPlot(model_forest)
varImpPlot_forest


# According to the MeanDecreaseAccuracy plot:

# Q1. Is the `attack` or `defense` variable more important?
answer1 <- "attack"

# Q2. Is the `weight_kg` or `height_m` variable more important?
answer2 <- "weight_kg"

# According to the MeanDecreaseGini plot:

# Q3. Is the `attack` or `defense` variable more important?
answer3 <- "defense"

# Q4. Is the `weight_kg` or `height_m` variable more important?
answer4 <- "weight_kg"