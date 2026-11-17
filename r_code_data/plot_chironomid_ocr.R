library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Confusion matrix values
x <- read_csv('chironomid_7models_ocr_PROVISIONAL.csv', show_col_types = FALSE)

# Index refers to class position in the confusion matrix, not a species name.
# Add an authoritative class-name mapping after verifying the axis labels.
stopifnot(nrow(x) == 224)

# Exclude rows requiring manual review for an initial QC-only visualization.
# For a publication figure, verify EVERY OCR-derived value against the PDF.
x_qc <- x

long <- x_qc %>%
  select(model, cap, class_index, Precision, Recall) %>%
  pivot_longer(
    cols = c(Precision, Recall),
    names_to = "metric",
    values_to = "percent"
  )

p <- ggplot(long, aes(x=cap, y=percent, group=class_index)) +
  geom_line(alpha=.25) + geom_point(size=.7, alpha=.6) +
  facet_wrap(~metric, ncol=1) +
  scale_x_log10(breaks=c(10,25,50,100,300,500,1000)) +
  scale_y_continuous(limits=c(0,100)) +
  labs(x='Nominal per-class training-image cap',y='Class performance (%)') +
  theme_bw()
ggsave('ocr_qc_performance1.pdf',p,width=9,height=7)

#plot top 10 classes by abundance
library(dplyr)
library(tidyr)
library(ggplot2)

# Selected classes and their zero-based indices
selected_taxa <- c(
  "Blurry specimens"             = 3,
  "Corynoneura"                  = 7,
  "Fragmented hc"                = 10,
  "Heterotrissocladius marcidus" = 11,
  "Micropsectra insignilobus"    = 15,
  "Micropsectra radialis"        = 16,
  "Microtendipes pedellus"       = 17,
  "Orthocladiinae Diamesinae"    = 19,
  "Psectrocladius spp"           = 26,
  "Tanytarsus lugens"            = 30
)

# Filter and reshape
long <- x_qc %>%
  filter(class_index %in% selected_taxa) %>%
  mutate(
    taxon = names(selected_taxa)[
      match(class_index, selected_taxa)
    ]
  ) %>%
  select(model, cap, taxon, Precision, Recall) %>%
  pivot_longer(
    cols = c(Precision, Recall),
    names_to = "metric",
    values_to = "percent"
  )

# Plot
p <- ggplot(
  long,
  aes(x = cap, y = percent, colour = taxon, group = taxon)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~metric, ncol = 1) +
  scale_x_log10(
    breaks = c(10, 25, 50, 100, 300, 500, 1000)
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_viridis_d(option = "turbo", name = "Class") +
  labs(
    x = "Nominal per-class training-image cap",
    y = "Classification performance (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  "selected_10_classes_performance_3.pdf",
  p,
  width = 11,
  height = 7
)




# Spearman correlations
# Mean precision and recall for each training-image cap
mean_performance <- long %>%
  group_by(cap, metric) %>%
  summarise(
    mean_percent = mean(percent, na.rm = TRUE),
    .groups = "drop"
  )

# Spearman correlations
spearman_results <- mean_performance %>%
  group_by(metric) %>%
  summarise(
    rho = unname(cor.test(
      cap, mean_percent,
      method = "spearman",
      exact = FALSE
    )$estimate),
    p_value = cor.test(
      cap, mean_percent,
      method = "spearman",
      exact = FALSE
    )$p.value,
    .groups = "drop"
  )

print(spearman_results)


