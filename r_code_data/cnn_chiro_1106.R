#load(if necessary install) packages

required_pkgs <- c(
  "tidyverse",
  "data.table",
  "mgcv",
  "nlme",
  "mblm",
  "rioja",
  "vegan",
  "viridis",
  "tidyr",
  "dplyr"
)

invisible(lapply(required_pkgs, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))


#performance of CNN/accuracy vs number of images from the latest version of the neural network

accuracy=c(94,96,93,97,60,62,85,67,25,66,86,62,82,53,82,71,85,80,56,51,78,94,100,57,76,50,60,79,83,60,33)
code <- c(
  "Acari spp","Background","Blurry specimens","Chaoborus mandible",
  "Chaoborus th","Chironomus spp","Corynoneura spp","Diatom",
  "Dicrotendipes spp","Fragmented hc","Heterotrissocladius marcidus",
  "Limnophyes type","Mandible","Mentum","Micropsectra insignilobus",
  "Micropsectra radialis","Microtendipes pedellus","Multiple",
  "Orthocladiinae Diamesinae","Paratanytarsus austriacus",
  "Particles","Pentaneurini spp","Pollen","Polypedilum spp",
  "Psectrocladius spp","Pseudochironomus spp",
  "Stictochironomus rosenschoeldi","Tanypodinae",
  "Tanytarsus lugens","Tanytarsus spp","others"
)

count <- c(
  242,387,626,317,23,249,1014,31,21,1000,539,41,163,73,553,997,
  544,223,525,223,245,91,321,33,1000,58,26,190,752,302,29
)
precision <- c(0.8490566037735849, 0.9736842105263158, 0.90625, 0.9682539682539683, 1.0, 0.6595744680851063, 0.8075117370892019, 1.0, 1.0, 0.6910994764397905, 0.7622950819672131, 0.5, 0.9310344827586207, 0.6153846153846154, 0.7647058823529411, 0.7580645161290323, 0.7948717948717948, 0.631578947368421, 0.6941176470588235, 0.6216216216216216, 0.8444444444444444, 0.7083333333333334, 1.0, 0.5, 0.7766497461928934, 0.5454545454545454, 0.6, 0.7692307692307693, 0.7515151515151515, 0.7058823529411765, 0.4)

recall <- c(0.9375, 0.961038961038961, 0.928, 0.9682539682539683, 0.6, 0.62, 0.8472906403940886, 0.6666666666666666, 0.25, 0.66, 0.8611111111111112, 0.625, 0.8181818181818182, 0.5333333333333333, 0.8198198198198198, 0.7085427135678392, 0.8532110091743119, 0.8, 0.5619047619047619, 0.5111111111111111, 0.7755102040816326, 0.9444444444444444, 1.0, 0.5714285714285714, 0.765, 0.5, 0.6, 0.7894736842105263, 0.8266666666666667, 0.6, 0.3333333333333333)

df_accu <- data.frame(
  accuracy = accuracy,
  precision,
  recall,
  count = count,
  code = code
)

mod_quad <- lm(accuracy ~ count + I(count^2), data = df_accu)
summary(mod_quad)

mod_log <- lm(accuracy ~ log(count), data = df_accu)
summary(mod_log)

mod_gam <- gam(accuracy ~ s(count), data = df_accu)
summary(mod_gam)

mod_sat <- nls(
  accuracy ~ (a * count) / (b + count),
  data = df_accu,
  start = list(a = 100, b = 100)
)

summary(mod_sat)

AIC(mod_quad, mod_log, mod_gam,mod_sat)

summary(mod_gam)$r.sq
summary(mod_quad)$r.squared
summary(mod_log)$r.squared


plot(mod_gam)

plot(df_accu$count, df_accu$accuracy, pch = 16)

# Create smooth prediction grid
new_x <- seq(min(df_accu$count), max(df_accu$count), length.out = 200)

# Add fits
lines(new_x, predict(mod_quad, newdata = data.frame(count = new_x)), lwd = 2)
lines(new_x, predict(mod_log,  newdata = data.frame(count = new_x)), lwd = 2, lty = 2)
lines(new_x, predict(mod_gam,  newdata = data.frame(count = new_x)), lwd = 2, lty = 3)

legend("bottomright",
       legend = c("Quadratic", "Log", "GAM"),
       lty = c(1,2,3), lwd = 2)


# =========================
# 1. Fit models
# =========================

mod_quad <- lm(accuracy ~ poly(count, 2), data = df_accu)

mod_log  <- lm(accuracy ~ log(count), data = df_accu)

mod_gam  <- gam(accuracy ~ s(count), data = df_accu)

mod_sat <- nls(
  accuracy ~ (a * count) / (b + count),
  data = df_accu,
  start = list(a = 100, b = 100)
)

# =========================
# 2. Model summaries
# =========================

summary(mod_sat)

AIC(mod_quad, mod_log, mod_gam, mod_sat)

summary(mod_gam)$r.sq
summary(mod_quad)$r.squared
summary(mod_log)$r.squared

# =========================
# 3. Prediction grid
# =========================

new_x <- seq(min(df_accu$count), max(df_accu$count), length.out = 200)

pred_df <- data.frame(
  count = new_x,
  quad = predict(mod_quad, newdata = data.frame(count = new_x)),
  log  = predict(mod_log,  newdata = data.frame(count = new_x)),
  gam  = predict(mod_gam,  newdata = data.frame(count = new_x)),
  sat  = predict(mod_sat,  newdata = data.frame(count = new_x))
)

# =========================
# 4. Reshape for ggplot
# =========================

pred_long <- pivot_longer(pred_df,
                          cols = -count,
                          names_to = "model",
                          values_to = "accuracy")

# =========================
# 5. Plot
# =========================

ggplot(df_accu, aes(x = count, y = accuracy)) +
  geom_point(alpha = 0.6) +
  geom_line(data = pred_long,
            aes(x = count, y = accuracy, linetype = model),
            linewidth = 1) +
  scale_linetype_manual(values = c(
    quad = "solid",
    log  = "dashed",
    gam  = "dotted",
    sat  = "dotdash"
  )) +
  labs(title = "Model Fits Comparison",
       x = "Count",
       y = "Accuracy",
       linetype = "Model") +
  theme_minimal()


coefs <- coef(mod_sat)
a <- coefs["a"]
b <- coefs["b"]

# 95% of asymptote
f <- 0.95

breakpoint <- (f * b) / (1 - f)
breakpoint




new_x <- seq(0, 2*breakpoint, length.out = 200)

# Create prediction data frame
pred_df <- data.frame(
  count = new_x,
  accuracy = predict(mod_sat, newdata = data.frame(count = new_x))
)

# Plot
ggplot(df_accu, aes(x = count, y = accuracy)) +
  geom_point() +
  geom_line(data = pred_df, aes(x = count, y = accuracy), color = "blue") +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red") +
  annotate("text", x = breakpoint, y = a*0.95, label = "95% saturation", hjust = -0.1, color = "red") +
  theme_minimal()

####################################################################
#1. Plots and models
#####################################################
#setwd("/home/jovyan/insPtemp")

#plot relations between manual and automatic measurments, where available

df <- read.csv("manual_aut_size_compare.csv")

df_long <- df %>%
  pivot_longer(
    cols = c(size_automatic, size_manual),
    names_to = "measurement_type",
    values_to = "value"
  ) %>%
  mutate(
    measurement_type = case_when(
      measurement_type == "size_automatic" ~ "automatic",
      measurement_type == "size_manual" ~ "manual"
    )
  )

melted_df<- aggregate(cbind(value)~T.WATER+Taxa+lake+measurement_type, data = df_long, FUN = median)


#Calculate correlation between manual and automatic measurments

r2_df <- df %>%
  filter(!is.na(size_manual), !is.na(size_automatic)) %>%
  group_by(lake, Taxa) %>%
  # only keep groups with 2+ data points
  summarise(
    model = list(lm(size_manual ~ size_automatic, data = cur_data())),
    .groups = 'drop'
  ) %>%
  mutate(
    r_squared = sapply(model, function(m) summary(m)$r.squared),
    label = paste0("R² = ", round(r_squared, 2))
  ) %>%
  select(lake, Taxa, r_squared, label)


r2_df <- r2_df %>%
  mutate(label = paste0("R² = ", round(r_squared, 2)))



# Filter out rows where size_manual is NA
df_filtered <- df %>%
  filter(
    !Taxa %in% c("Paratanytarsus austriacus", "Heterotrissocladius marcidus"),
    !is.na(size_manual)
  )


# Drop unwanted taxa and NAs

cor_df <- df_filtered %>%
  group_by(lake, Taxa) %>%
  filter(n() > 1) %>%
  summarise(
    correlation = tryCatch(
      round(cor.test(size_manual, size_automatic)$estimate, 2),
      error = function(e) NA_real_
    ),
    x = max(size_automatic, na.rm = TRUE),
    y = max(size_manual, na.rm = TRUE),
    .groups = "drop"
  )
#Plot correlations between the manual and automatic measurments
ggplot(df_filtered, aes(x = size_automatic, y = size_manual, color = Taxa)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Taxa), color = "black", linetype = "dashed") +
  geom_text(
    data = cor_df,
    aes(x = x, y = y, label = paste0("r = ", correlation)),
    inherit.aes = FALSE,
    size = 3.5,
    hjust = 1.1, vjust = 1.1
  ) +
  facet_wrap(~ lake, scales = "fixed") +
  theme_minimal() +
  labs(
    title = "Manual vs. Automatic Length Measurements",
    x = "Automatic Length (major_axis_um)",
    y = "Manual Length",
    color = "Taxa"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )


#=================================================================================================
#GAM Models
size_temperature_aut=read.csv("size_temperature_aut2.csv")
#remove chironomid containin classes with no taxonomic value or too few (<10) specimens
remove_vals <- c("Chaoborus th",
                 "Mandible",
                 "Mentum",
                 "Polypedilum spp",
                 "others"
)

size_temperature_aut <- size_temperature_aut %>%
  filter(!Taxa %in% remove_vals)



melted_df=size_temperature_aut
melted_df$Taxa_lake <- interaction(melted_df$Taxa, melted_df$Lake, drop = TRUE)
melted_df <- melted_df %>%
  group_by(Temperature, Taxa_lake,Lake,Taxa,data_type,Depth) %>%
  summarise(size = mean(size),
            count = mean(count),
            .groups = "drop")




#gamm-models of size relation from taxa vs temperature for all lakes
melted_df$Taxa <- as.factor(melted_df$Taxa)
melted_df <- melted_df |>
  dplyr::group_by(Lake) |>
  dplyr::mutate(obs = dplyr::row_number())

melted_df$series <- interaction(melted_df$Lake, melted_df$Taxa,melted_df$Depth)
model_gam <- gamm(size ~ data_type + s(Temperature, by = Taxa), random = list(Lake = ~1), correlation = corAR1(form = ~obs | Lake/Depth), data = melted_df, weights = count)


#model_gam <- gamm(size ~ data_type + s(Temperature, by = Taxa), random = list(Lake = ~1), correlation = corAR1(form = ~Depth | series), data = melted_df, weights = count)

summary(model_gam$gam)  # Summary of the GAM (smooth terms)
summary(model_gam$lme)  # Summary of the linear mixed-effects (random effects)
#=================================================================================================

# mblm of the size vs temperature by species and beamplot of mbl slopes byt taxa and lake
# Ensure it's a data.table

setDT(melted_df)
melted_df <- na.omit(melted_df)


# Step 2: Compute MBLM slopes by Taxa, lake, and measurement_type
results <- melted_df[, {
  slope <- NA_real_
  pval <- NA_real_
  
  model <- tryCatch(mblm(size ~ Temperature), error = function(e) NULL)
  
  if (!is.null(model)) {
    smry <- summary(model)
    coef_table <- smry$coefficients
    if (!is.null(coef_table) && "Temperature" %in% rownames(coef_table)) {
      slope <- coef_table["Temperature", "Estimate"]
      if ("Pr(>|V|)" %in% colnames(coef_table)) {
        pval <- coef_table["Temperature", "Pr(>|V|)"]
      }
    }
  }
  
  list(slope = slope, p_value = pval)
}, by = .(Taxa, Lake)]

#step 2a compute slopes for all taxa irrespective of lake

results1 <- melted_df[, {
  slope <- NA_real_
  pval <- NA_real_
  
  model <- tryCatch(mblm(size ~ Temperature), error = function(e) NULL)
  
  if (!is.null(model)) {
    smry <- summary(model)
    coef_table <- smry$coefficients
    if (!is.null(coef_table) && "Temperature" %in% rownames(coef_table)) {
      slope <- coef_table["Temperature", "Estimate"]
      if ("Pr(>|V|)" %in% colnames(coef_table)) {
        pval <- coef_table["Temperature", "Pr(>|V|)"]
      }
    }
  }
  
  list(slope = slope, p_value = pval)
}, by = .(Taxa)]


# Step 3: Assign slope categories
results[, slope_color := fifelse(is.na(p_value) | p_value > 0.05, "not significant",
                                 fifelse(slope > 0, "increase", "decrease"))]

# Step 4: Remove NAs 
results <- na.omit(results)



# Step 5: Plot beamplot of mbl slopes byt taxa and lake

ggplot(results, aes(x = Lake, y = slope, color = slope_color)) +
  geom_point(size = 5, alpha = 0.3) +
  facet_wrap(~ Taxa, scales = "fixed")+
  scale_color_manual(
    values = c("increase" = "red", "decrease" = "blue", "not significant" = "black"),
    name = "Trend"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Slope of Size vs. Temperature by Taxa and Lake",
    x = "Lake",
    y = "Slope"
  )


# Step 3a: Assign slope categories
results1[, slope_color := fifelse(is.na(p_value) | p_value > 0.05, "not significant",
                                  fifelse(slope > 0, "increase", "decrease"))]

# Step 4a: Remove NAs 
results1 <- na.omit(results1)

p1=ggplot(results1, aes(x = reorder(Taxa, slope), y = slope, color = slope_color)) +
  geom_point(size = 5,alpha=0.3) +
  coord_flip() +
  scale_color_manual(
    values = c("increase" = "red", 
               "decrease" = "blue", 
               "not significant" = "black"),
    name = "Trend"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Slope of Size vs Temperature by Taxa",
    x = "Taxa",
    y = "Slope"
  )
print(p1)
#############################
#Plot smoothed trends of size from temperature in different lakes for different taxa

scale_color_viridis_d(option = "turbo")
# Plot with brewer palette and legend
p <- ggplot(
  melted_df,
  aes(x = Temperature, y = size, color = Taxa)
) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(~ Lake, scales = "fixed") +
  labs(
    title = "Size vs Temperature by Lake and Measurement Type",
    x = "Temperature (Temperature)",
    y = "Size",
    color = "Taxa"
  ) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

#not faceted by lake
p1 <- ggplot(
  melted_df,
  aes(x = Temperature, y = size, color = Taxa)
) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  
  labs(
    title = "Size vs Temperature by Lake and Measurement Type",
    x = "Temperature (Temperature)",
    y = "Size",
    color = "Taxa"
  ) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )



# 3. Print plot
print(p)


summary_stats <- results %>%
  group_by(slope_color) %>%
  summarise(
    n = n(),
    across(c(slope, p_value),
           list(mean = mean, sd = sd),
           .names = "{col}_{fn}")
  )

print(summary_stats)

####################################################################################################################
#2.Counts of classes per set of data
#read original automatic counts
morphology_ldrs_df   <- read.csv("morphology_ldrs_df.csv", stringsAsFactors = FALSE)
morphology_hij_1003_df <- read.csv("morphology_hij_1003_df.csv", stringsAsFactors = FALSE)
morphology_sk1003_df <- read.csv("morphology_sk1003_df.csv", stringsAsFactors = FALSE)
morphology_ldry_df   <- read.csv("morphology_ldry_df.csv", stringsAsFactors = FALSE)
morphology_ana_df    <- read.csv("morphology_ana_df.csv", stringsAsFactors = FALSE)
morphology_peleaga_df <- read.csv("morphology_peleaga_df.csv", stringsAsFactors = FALSE)



# Put datasets into a named list
datasets <- list(
  ldrs = morphology_ldrs_df,
  hij_1003 = morphology_hij_1003_df,
  sk1003 = morphology_sk1003_df,
  ldry = morphology_ldry_df,
  ana = morphology_ana_df,
  peleaga = morphology_peleaga_df
)

# Count occurrences of each label per dataset
label_counts <- imap_dfr(datasets, ~ .x %>%
                           count(label) %>%
                           mutate(dataset = .y))

# Reorder columns for readability
label_counts <- label_counts %>%
  select(dataset, label, n)

# View merged table
print(label_counts)



label_counts <- label_counts %>%
  mutate(dataset = recode(dataset,
                          "ldrs" = "Laguna de Rio Seco","hij_1003"="Hijkermeer","ldry"="Laguna de la Roya","ana"="lake Ana","peleaga"="lake Peleaga","sk1003"="Tatra set"))


write_csv(label_counts, "label_frequency_per_dataset.csv")

# Plot histogram / bar plot
ggplot(label_counts, aes(x = label, y = n)) +
  geom_col() +
  facet_wrap(~ dataset, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Label",
    y = "Count",
    title = "Occurrences of Label Levels per Dataset"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compute overall label counts across all datasets
overall_counts <- label_counts %>%
  group_by(label) %>%
  summarise(total_n = sum(n), .groups = "drop")

# Print overall counts
print(overall_counts)
#write_csv(overall_counts, "overall_counts.csv")

#training set classes distribution
labels <- c(
  "Acari spp","Background","Blurry specimens","Chaoborus mandible",
  "Chaoborus th","Chironomus spp","Corynoneura spp","Diatom",
  "Dicrotendipes spp","Fragmented hc","Heterotrissocladius marcidus",
  "Ligula","Limnophyes type","Mandible","Mentum",
  "Micropsectra insignilobus","Micropsectra radialis",
  "Microtendipes pedellus","Multiple","Orthocladiinae Diamesinae",
  "Paratanytarsus austriacus","Particles",
  "Pentaneurini spp","Pollen","Polypedilum spp","Psectrocladius spp",
  "Pseudochironomus spp",
  "Stictochironomus rosenschoeldi","Tanypodinae",
  "Tanytarsus lugens","Tanytarsus spp")

counts <- c(
  119,341,438,213,16,156,965,31,8,959,538,3,7,150,27,394,967,153,157,
  524,178,218,87,321,5,923,20,5,184,675,280
)

df <- data.frame(
  label = labels,
  count = counts
)

# Plot histogram-like bar chart
ggplot(df, aes(x = label, y = count)) +
  geom_col() +
  theme_bw() +
  labs(
    x = "Label",
    y = "Count",
    title = "Label Frequency"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )


#================================================================
# 3. make strat plots in RiojaPlot for automatic and manual counts
#===================================================================================
#===================================================================================
#Counts comparsion in the lakes of Slovakian surface sediment set
raw_counts_sk=read.csv("sk_counts.csv",sep=",")

transposed_sk_man <- as.data.frame(t(raw_counts_sk))
# Make first row the column names
colnames(transposed_sk_man) <- as.character(unlist(transposed_sk_man[1, ]))

# Remove that row from the data
transposed_sk_man <- transposed_sk_man[-1, ]
# Set the first row as column names

transposed_sk_man$Depth <- rownames(transposed_sk_man)
transposed_sk_man[is.na(transposed_sk_man)] <- 0

#automatic measurments hij
sk_aut <- read.csv("sk_automatic_reduced.csv")

sk_aut=sk_aut[,c(3:4)]


colnames(sk_aut)<-c("Taxa","Depth")

############################################################
# abundance table automatic
abund_a <- sk_aut %>%
  count(Depth, Taxa)

# wide format automatic
abund_wide_a <- abund_a %>%
  pivot_wider(
    names_from = Taxa,
    values_from = n,
    values_fill = 0
  )


# split by method
auto <- abund_wide_a

manual <- transposed_sk_man

# extract matrices
depth_auto <- auto$Depth
mat_auto <- as.matrix(auto[, !(names(auto) %in% c("Depth"))])
mat_auto <- mat_auto[, !colnames(mat_auto) %in% c(
  "Background",
  "Blurry specimens",
  "unsure",
  "Fragmented hc",
  "Multiple",
  "Pollen",
  "Particles",
  "Acari spp"
)]



depth_manual <- manual$Depth
mat_manual <- as.matrix(manual[, !(names(manual) %in% c("Depth"))])
mat_manual <- mat_manual[, !colnames(mat_manual) %in% c(
  "Count",
  "Depth"
)]
mat_manual <- matrix(
  as.numeric(trimws(mat_manual)),
  nrow = nrow(mat_manual),
  dimnames = dimnames(mat_manual)
)
mat_manual[is.na(mat_manual)] <- 0

# convert to relative abundance (%)
mat_auto <- sweep(mat_auto, 1, rowSums(mat_auto), "/") * 100
mat_manual <- sweep(mat_manual, 1, rowSums(mat_manual), "/") * 100

# keep 5 most abundant taxa
top_manual <- names(sort(colSums(mat_manual), decreasing = TRUE))[1:5]


top_auto <- intersect(c("Micropsectra radialis", "Tanytarsus lugens", "Heterotrissocladius marcidus", "Orthocladiinae Diamesinae", "Tanypodinae"),
                      colnames(mat_auto))

mat_auto <- mat_auto[, top_auto]
mat_manual <- mat_manual[, top_manual]

# sort lake names
rn <- rownames(mat_manual)
rn_clean <- tolower(gsub("\\.", " ", rn))

depth_clean <- tolower(depth_auto)
#keep only lakes present in Automatic dataset
mat_man_subset <- mat_manual[rn_clean %in% depth_clean, ]


rn_clean <- tolower(gsub("\\.", " ", rownames(mat_man_subset)))

# Get order index
ord <- match(depth_clean, rn_clean)

# Reorder (and drop non-matches)
mat_man <- mat_man_subset[ord[!is.na(ord)], ]

#instead of depth we will use the numerical vector, where numbers are corresponding with lakes in the vector "depth_auto"
# drop more non matching rows
depth=1:42
depth=as.numeric(depth)
depth=order(depth)
# plot
mat_man <- mat_man[
  !(rownames(mat_man) %in% c("Male.Zabie")),
]
mat_auto <- mat_auto[-c(14:15), ]

#automatic
p.col1 <- rep("gold2", times=5)

strat.plot(
  mat_auto,
  yvar = depth,
  y.rev = FALSE,
  plot.poly=TRUE,
  col.poly=p.col1,
  scale.percent = TRUE,
  ylabel = "lake number",
  title = "Tatra lakes, Automatic"
)

#manual
p.col <- rep("forestgreen", times=5)

strat.plot(
  mat_man,
  yvar = depth,
  y.rev = FALSE,
  plot.poly=TRUE,
  scale.percent = TRUE,
  col.poly=p.col,
  ylabel = "lake number",
  title = "Tatara lakes, Manual"
)

########################
#Pearson´s bray curtis distance btwn manual and automatic counts
auto_interp <- apply(mat_man, 2, function(col) {
  approx(x = depth, y = col, xout = depth)$y
})

bray_sk <- mapply(function(x, y) {
  
  tmp <- rbind(x, y)
  
  1 - as.numeric(vegdist(tmp, method = "bray"))
  
},
x = as.data.frame(mat_auto),
y = as.data.frame(auto_interp)
)

names(bray_sk) <- colnames(mat_man)

#=====================================================

#=====================================================
#Proportion of abundant taxa in downcore sets - automatic vs manual, using strat.plot
#HIJKERMEER

counts_hij<- read.csv("Hijkermeer_count.csv", check.names = FALSE)

#automatic measurments hij

hij_aut<- read.csv("hij_aut.csv", check.names = FALSE)


colnames(hij_aut)<-c("Taxa","Depth")

############################################################
# abundance table automatic
abund_a <- hij_aut %>%
  count(Depth, Taxa)

# wide format automatic
abund_wide_a <- abund_a %>%
  pivot_wider(
    names_from = Taxa,
    values_from = n,
    values_fill = 0
  )


# split by method
auto <- abund_wide_a

manual <- counts_hij

# extract matrices
depth_auto <- auto$Depth
mat_auto <- as.matrix(auto[, !(names(auto) %in% c("Depth"))])
mat_auto <- mat_auto[, !colnames(mat_auto) %in% c(
  "Background",
  "Blurry specimens",
  "unsure",
  "Fragmented hc",
  "Multiple",
  "Pollen",
  "Particles",
  "Acari spp"
)]

depth_manual <- manual$depth
mat_manual <- as.matrix(manual[, !(names(manual) %in% c("Depth"))])
mat_manual <- mat_manual[, !colnames(mat_manual) %in% c(
  "Count",
  "depth"
)]


#name matricies for retrival in WAPLS
mat_manual_hij=mat_manual
mat_auto_hij=mat_auto

# convert to relative abundance (%)
mat_auto <- sweep(mat_auto, 1, rowSums(mat_auto), "/") * 100
mat_manual <- sweep(mat_manual, 1, rowSums(mat_manual), "/") * 100

# keep 5 most abundant taxa
top_manual <- names(sort(colSums(mat_manual), decreasing = TRUE))[1:5]


top_auto <- intersect(c("Microtendipes", "Chironomus spp", "Tanytarsus spp", "Paratanytarsus austriacus", "Tanytarsus lugens"),
                      colnames(mat_auto))

mat_auto <- mat_auto[, top_auto]
mat_manual <- mat_manual[, top_manual]

# sort depths
ord <- order(depth_auto)
mat_auto <- mat_auto[ord, ]
depth_auto <- depth_auto[ord]
depth_auto_hij=depth_auto

ord <- order(depth_manual)
mat_manual <- mat_manual[ord, ]
depth_manual <- depth_manual[ord]


# plot

#automatic
p.col1 <- rep("gold2", times=5)

strat.plot(
  mat_auto,
  yvar = depth_auto,
  y.rev = TRUE,
  plot.poly=TRUE,
  col.poly=p.col1,
  scale.percent = TRUE,
  ylabel = "Depth (cm)",
  title = "Hijkermeer lake, Automatic"
)

#manual
p.col <- rep("forestgreen", times=5)

strat.plot(
  mat_manual,
  yvar = depth_manual,
  y.rev = TRUE,
  plot.poly=TRUE,
  scale.percent = TRUE,
  col.poly=p.col,
  ylabel = "Depth (cm)",
  title = "Hijkermeer lak, Manual"
)

#bray_curtis dist
auto_interp <- apply(mat_manual, 2, function(col) {
  approx(x = depth, y = col, xout = depth)$y
})


bray_hij <- mapply(function(x, y) {
  
  tmp <- rbind(x, y)
  
  1 - as.numeric(vegdist(tmp, method = "bray"))
  
},
x = as.data.frame(mat_auto),
y = as.data.frame(auto_interp)
)
names(bray_hij) <- colnames(mat_auto)
#=========================================================================================

#Laguna de la Roya

roy_raw <- read.csv(
  "ROY_counts.csv",
  sep = ",",
  fileEncoding = "Windows-1252",
  stringsAsFactors = FALSE
)
roy_raw[is.na(roy_raw)] <- 0
roy_raw=roy_raw[1:44,]
roy_raw=roy_raw[,c(2,3:63,66)]

roy_raw[ , 1:63] <- lapply(
  roy_raw[ , 1:63],
  function(x) {
    x[x == ""] <- NA
    as.numeric(chartr(",", ".", x))
  }
)
roy_raw[is.na(roy_raw)] <- 0

#add automatic ldry

ldry_aut_fin1<- read.csv("ldry_aut_1.csv", check.names = FALSE)

##############################################################
# abundance table automatic
abund_a <- ldry_aut_fin1 %>%
  count(Absolute.depth, taxa)

# wide format automatic
abund_wide_a <- abund_a %>%
  pivot_wider(
    names_from = taxa,
    values_from = n,
    values_fill = 0
  )


# split by method
auto <- abund_wide_a

manual <- roy_raw

# extract matrices
depth_auto <- auto$Absolute.depth
#remove depth and non chironomid classes
mat_auto <- as.matrix(auto[, !(names(auto) %in% c("Absolute.depth"))])
cols_remove <- c(
  "Background",
  "Blurry specimens",
  "unsure",
  "Fragmented hc",
  "Multiple",
  "Pollen",
  "Particles",
  "Acari spp"
)

mat_auto <- mat_auto[, !(colnames(mat_auto) %in% cols_remove), drop = FALSE]

depth_manual <- manual$Absolute.depth..2010.scale..cm.
mat_manual <- manual |>
  dplyr::select(-`Absolute.depth..2010.scale..cm.`) |>
  dplyr::mutate(across(everything(), as.numeric)) |>
  as.matrix()
depth_manual_ldry <- manual$Absolute.depth..2010.scale..cm.
depth_manual_ldry <- depth_manual_ldry[ord]
#specific matricies naming for WAPLS
mat_manual_ldry=mat_manual 
mat_auto_ldry=mat_auto

# convert to relative abundance (%)
mat_auto <- sweep(mat_auto, 1, rowSums(mat_auto), "/") * 100
mat_manual <- sweep(mat_manual, 1, rowSums(mat_manual), "/") * 100



# keep 5 most abundant taxa
#top_auto <- names(sort(colSums(mat_auto), decreasing = TRUE))[1:5]
top_manual <- names(sort(colSums(mat_manual), decreasing = TRUE))[1:5]
#since top 5 includes daphnia, I am going to set top 5 Chironomidae (and chaoborus) manualy, based on the automatic list - daphnia

top_auto <- intersect(c("Micropsectra insignilobus", "Chaoborus mandible", "Pentaneurini spp","Tanypodinae","Micropsectra radialis"),
                      colnames(mat_auto))

mat_auto <- mat_auto[, top_auto]
mat_manual <- mat_manual[, top_manual]

# sort depths
ord <- order(depth_auto)
mat_auto <- mat_auto[ord, ]
depth_auto <- depth_auto[ord]
depth_auto_ldry=depth_auto
ord <- order(depth_manual)
mat_manual <- mat_manual[ord, ]

depth_man_ldry=depth_manual
# plot
#automatic
p.col1 <- rep("gold2", times=5)

strat.plot(
  mat_auto,
  yvar = depth_auto,
  y.rev = TRUE,
  plot.poly=TRUE,
  col.poly=p.col1,
  scale.percent = TRUE,
  ylabel = "Depth (cm)",
  title = "Laguna de la Roya, Automatic"
)

#manual
p.col <- rep("forestgreen", times=5)

strat.plot(
  mat_manual,
  yvar = depth_manual,
  y.rev = TRUE,
  plot.poly=TRUE,
  scale.percent = TRUE,
  col.poly=p.col,
  ylabel = "Depth (cm)",
  title = "Laguna de la Roya, Manual"
)

#bray curtis dist
auto_interp <- apply(mat_manual, 2, function(col) {
  approx(x = depth_manual, y = col, xout = depth_auto)$y
})

bray_ldry <- mapply(function(x, y) {
  
  tmp <- rbind(x, y)
  
  1 - as.numeric(vegdist(tmp, method = "bray"))
  
},
x = as.data.frame(mat_auto),
y = as.data.frame(auto_interp)
)
names(bray_ldry) <- colnames(mat_auto)

############################################################################

#LDRS

ldrs_raw <- read.csv(
  "LdRS for Viktor July 25.csv",
  sep = ",")
ldrs_raw=ldrs_raw[11:58,]



ldrs_raw[] <- lapply(ldrs_raw, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})
#matrix
ldrs_raw1=ldrs_raw[,c(3,5:23)]
#abiotic data
ldrs_raw2=ldrs_raw[,c(1:4)]
colnames(ldrs_raw2)<-c("core.LdRS.Core.01","Depth..m.","REAL.DEPTH","Age"  )


#add automatic ldrs
#############################################################################


merged_df_ldrs<- read.csv("ldrs_aut_1.csv", check.names = FALSE)

##############################################################
# abundance table automatic
abund_a <- merged_df_ldrs %>%
  count(label,REAL.DEPTH)

# wide format automatic
abund_wide_a <- abund_a %>%
  pivot_wider(
    names_from = label,
    values_from = n,
    values_fill = 0
  )


# split by method
auto <- abund_wide_a
manual <- ldrs_raw1

# extract matrices
depth_auto <- auto$REAL.DEPTH
mat_auto <- as.matrix(auto[, !(names(auto) %in% c("REAL.DEPTH"))])
#remove non chironomid classes
mat_auto <- mat_auto[, !colnames(mat_auto) %in% c(
  "Background",
  "Blurry specimens",
  "unsure",
  "Fragmented hc",
  "Multiple",
  "Pollen",
  "Particles",
  "Acari spp"
)]

depth_manual <- manual$REAL.DEPTH
mat_manual <- as.matrix(manual[, !(names(manual) %in% c("REAL.DEPTH"))])

#give specific names for WAPLS analysis
mat_manual_ldrs=mat_manual
mat_auto_ldrs=mat_auto


# convert to relative abundance (%)
mat_auto <- sweep(mat_auto, 1, rowSums(mat_auto), "/") * 100
mat_manual <- sweep(mat_manual, 1, rowSums(mat_manual), "/") * 100


# keep 5 most abundant taxa
#top_auto <- names(sort(colSums(mat_auto), decreasing = TRUE))[1:5]
top_manual <- names(sort(colSums(mat_manual), decreasing = TRUE))[1:5]
top_auto <- intersect(c("Psectrocladius spp", "Corynoneura spp","Micropsectra radialis","Chironomus spp"),
                      colnames(mat_auto))
mat_auto <- mat_auto[, top_auto]
mat_manual <- mat_manual[, top_manual]

# sort depths
ord <- order(depth_auto)
mat_auto <- mat_auto[ord, ]
depth_auto <- depth_auto[ord]
depth_auto_ldrs=depth_auto

ord <- order(depth_manual)
mat_manual <- mat_manual[ord, ]
depth_manual <- depth_manual[ord]

# plot
#automatic
p.col1 <- rep("gold2", times=5)

strat.plot(
  mat_auto,
  yvar = depth_auto,
  y.rev = TRUE,
  plot.poly=TRUE,
  col.poly=p.col1,
  scale.percent = TRUE,
  ylabel = "Depth (cm)",
  title = "Automatic, LDRS"
)

#manual
p.col <- rep("forestgreen", times=5)

strat.plot(
  mat_manual,
  yvar = depth_manual,
  y.rev = TRUE,
  plot.poly=TRUE,
  scale.percent = TRUE,
  col.poly=p.col,
  ylabel = "Depth (cm)",
  title = "Manual, LDRS"
)

# Bray_curis by depth per morphotype (5 top taxa) - not that 4 automatic classes 
#are matching 5 morphotypes here, since most of the "psectrocladius spp" class is P. cf sordidellus
#I am removing one column from mat_man - allopsectrocladius!!!!!

mat_manual=mat_manual[,c(1:3,5)]

auto_interp <- apply(mat_manual, 2, function(col) {
  approx(x = depth_manual, y = col, xout = depth_auto)$y
})

bray_ldrs <- mapply(function(x, y) {
  
  tmp <- rbind(x, y)
  
  1 - as.numeric(vegdist(tmp, method = "bray"))
  
},
x = as.data.frame(mat_auto),
y = as.data.frame(auto_interp)
)
names(bray_ldrs) <- colnames(mat_auto)
#====================================================================
#Lake Ana count

counts_ana     <- read.csv("lake_ana_counts.csv", check.names = FALSE)
counts_ana=as.data.frame(counts_ana)
# Transpose the data frame
transposed_ana <- as.data.frame(t(counts_ana))
# Make first row the column names
colnames(transposed_ana) <- as.character(unlist(transposed_ana[1, ]))

# Remove that row from the data
transposed_ana <- transposed_ana[-1, ]
# Set the first row as column names

transposed_ana$Age <- rownames(transposed_ana)
#write.csv(transposed_ana, "transposed_ana.csv", row.names = TRUE)

age_depth=transposed_ana[,c(1,40)]
transposed_ana=transposed_ana[,1:39]
# Reset row names
rownames(transposed_ana) <- NULL

# View the transposed data frame


#automatic measurments ana

ana_aut<- read.csv("ana_aut_1.csv", check.names = FALSE)


#==============================================================
# abundance table automatic
abund_a <- ana_aut %>%
  count(Depth, Taxa)


# wide format automatic
abund_wide_a <- abund_a %>%
  pivot_wider(
    names_from = Taxa,
    values_from = n,
    values_fill = 0
  )


# split by method
auto <- abund_wide_a
manual <- transposed_ana

# extract matrices
depth_auto <- auto$Depth
mat_auto <- as.matrix(auto[, !(names(auto) %in% c("Depth"))])
#remove non-chironomid classes
mat_auto <- mat_auto[, !colnames(mat_auto) %in% c(
  "Background",
  "Blurry specimens",
  "unsure",
  "Fragmented hc",
  "Multiple",
  "Pollen",
  "Particles",
  "Acari spp"
)]

depth_manual <- manual$Depth
mat_manual <- as.matrix(manual[, !(names(manual) %in% c("Depth"))])
mat_manual <- apply(mat_manual, 2, as.numeric)

#specific matricies naming for WAPLS
mat_manual_ana=mat_manual
mat_auto_ana=mat_auto

# convert to relative abundance (%)
mat_auto <- sweep(mat_auto, 1, rowSums(mat_auto), "/") * 100
mat_manual <- sweep(mat_manual, 1, rowSums(mat_manual), "/") * 100


# keep 5 most abundant taxa
#top_auto <- names(sort(colSums(mat_auto), decreasing = TRUE))[1:5]
top_manual <- names(sort(colSums(mat_manual), decreasing = TRUE))[1:5]
top_auto <- intersect(c("Micropsectra insignilobus", "Heterotrissocladius marcidus","Corynoneura spp","Pentaneurini spp","Tanypodinae"),
                      colnames(mat_auto))
mat_auto <- mat_auto[, top_auto]
mat_manual <- mat_manual[, top_manual]

# sort depths
ord <- order(depth_auto)
mat_auto <- mat_auto[ord, ]
depth_auto <- depth_auto[ord]
depth_auto <- as.numeric(depth_auto)
dpth_auto_ana=depth_auto
ord <- order(depth_manual)
mat_manual <- mat_manual[ord, ]
depth_manual <- depth_manual[ord]
depth_manual<-as.numeric(depth_manual)
# plot
#automatic
p.col1 <- rep("gold2", times=5)

strat.plot(
  mat_auto,
  yvar = depth_auto,
  y.rev = TRUE,
  plot.poly=TRUE,
  col.poly=p.col1,
  scale.percent = TRUE,
  ylabel = "Depth (cm)",
  title = "Automatic, lake Ana"
)

#manual
p.col <- rep("forestgreen", times=5)

strat.plot(
  mat_manual,
  yvar = depth_manual,
  y.rev = TRUE,
  plot.poly=TRUE,
  scale.percent = TRUE,
  col.poly=p.col,
  ylabel = "Depth (cm)",
  title = "Manual, lake Ana"
)

# bray curtis by depth per morphotype (5 top taxa)
auto_interp <- apply(mat_manual, 2, function(col) {
  approx(x = depth_manual, y = col, xout = depth_auto)$y
})

bray_ana <- mapply(function(x, y) {
  
  tmp <- rbind(x, y)
  
  1 - as.numeric(vegdist(tmp, method = "bray"))
  
},
x = as.data.frame(mat_auto),
y = as.data.frame(auto_interp)
)
names(bray_ana) <- colnames(mat_auto)

#######################################################################################################

#====================================================================
#Lake peleaga count

counts_peleaga     <- read.csv("lake_peleaga_counts.csv", check.names = FALSE)
counts_peleaga=as.data.frame(counts_peleaga)

# Transpose the data frame
transposed_peleaga <- as.data.frame(t(counts_peleaga))

# Make first row the column names
colnames(transposed_peleaga) <- as.character(unlist(transposed_peleaga[1, ]))
#write.csv(transposed_peleaga, "transposed_peleaga.csv", row.names = TRUE)
# Remove that row from the data
transposed_peleaga <- transposed_peleaga[-1, ]

# Set the first row as column names
transposed_peleaga$Age <- rownames(transposed_peleaga)

age_depth=transposed_peleaga[,c(1,61)]
transposed_peleaga=transposed_peleaga[,1:60]

# Reset row names
rownames(transposed_peleaga) <- NULL



#automatic measurments peleaga
peleaga_aut<- read.csv("peleaga_aut_1.csv", check.names = FALSE)

#==============================================================
# abundance table automatic
abund_p <- peleaga_aut %>%
  count(Depth, Taxa)

# wide format automatic
abund_wide_p <- abund_p %>%
  pivot_wider(
    names_from = Taxa,
    values_from = n,
    values_fill = 0
  )

# split by method
auto <- abund_wide_p
manual <- transposed_peleaga

# extract matrices
depth_auto <- auto$Depth
mat_auto <- as.matrix(auto[, !(names(auto) %in% c("Depth"))])
#remove non chironomid classes
mat_auto <- mat_auto[, !colnames(mat_auto) %in% c(
  "Background",
  "Blurry specimens",
  "unsure",
  "Fragmented hc",
  "Multiple",
  "Pollen",
  "Particles",
  "Acari spp"
)]

depth_manual <- manual$Depth
mat_manual <- as.matrix(manual[, !(names(manual) %in% c("Depth"))])
mat_manual <- apply(mat_manual, 2, as.numeric)

#specific names for matricies WAPLS
mat_manual_peleaga=mat_manual
mat_auto_peleaga=mat_auto

# convert to relative abundance (%)
mat_auto <- sweep(mat_auto, 1, rowSums(mat_auto), "/") * 100
mat_manual <- sweep(mat_manual, 1, rowSums(mat_manual), "/") * 100

# keep 5 most abundant taxa
#top_auto <- names(sort(colSums(mat_auto), decreasing = TRUE))[1:5]
top_manual <- names(sort(colSums(mat_manual), decreasing = TRUE))[1:5]
top_auto <- intersect(c("Tanytarsus lugens", "Tanypodinae","Micropsectra insignilobus","Tanytarsus spp","Heterotrissocladius marcidus"),
                      colnames(mat_auto))


mat_auto <- mat_auto[, top_auto]
mat_manual <- mat_manual[, top_manual]

# sort depths
# convert first
depth_auto <- as.numeric(depth_auto)
depth_manual <- as.numeric(depth_manual)

# then order
ord <- order(depth_auto)
mat_auto <- mat_auto[ord, ]
depth_auto <- depth_auto[ord]
depth_auto_peleaga=depth_auto

ord <- order(depth_manual)
mat_manual <- mat_manual[ord, ]
depth_manual <- depth_manual[ord]
# plot
#automatic
p.col1 <- rep("gold2", times=5)

strat.plot(
  mat_auto,
  yvar = depth_auto,
  y.rev = TRUE,
  plot.poly=TRUE,
  col.poly=p.col1,
  scale.percent = TRUE,
  ylabel = "Depth (cm)",
  title = "Automatic, lake Peleaga"
)

#manual
p.col <- rep("forestgreen", times=5)

strat.plot(
  mat_manual,
  yvar = depth_manual,
  y.rev = TRUE,
  plot.poly=TRUE,
  scale.percent = TRUE,
  col.poly=p.col,
  ylabel = "Depth (cm)",
  title = "Manual, lake Peleaga"
)
# Bray curtis by depth per morphotype (5 top taxa)
mat_manual=mat_manual[1:16,]
depth_manual <- depth_manual [c(1:16)] 
auto_interp <- apply(mat_manual, 2, function(col) {
  approx(x = depth_manual, y = col, xout = depth_auto)$y
})

bray_pel <- mapply(function(x, y) {
  
  tmp <- rbind(x, y)
  
  1 - as.numeric(vegdist(tmp, method = "bray"))
  
},
x = as.data.frame(mat_auto),
y = as.data.frame(mat_manual)
)
names(bray_pel) <- colnames(mat_auto)


#=====================================================
#Clean up
#dev.off()
#rm(list = ls())







