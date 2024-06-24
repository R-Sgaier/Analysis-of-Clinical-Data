# Load data (IHC results plus clinical parameters)

my_IHC_data<-as.data.frame(IHC_findings.csv)


# Load libraries
library(dplyr)
library(pheatmap)



# Function to convert selected columns from character to double
convert_columns_to_double <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ as.numeric(.)))
}

# Remove categorical variables
my_numdata<- my_IHC_data[, -c(1,2,42,43)]
# Specify the columns to convert
columns_to_convert <- colnames(my_numdata)

# Apply the conversion function to the specified columns
ICH_converted <- convert_columns_to_double(my_IHC_data, columns_to_convert)

# Select variables to scale
IHC_pars<- ICH_converted[, 1:10]


# Scale the data and set sample IDs as row names
scaled_data <- IHC_pars %>%
  select(-Sample_ID, -Phenotype) %>%
  scale()
rownames(scaled_data) <- IHC_pars$Sample_ID

# Create df for heatmap annotations (Phenotype, Sample_IDs)
heatmap_annotations <- data.frame(Phenotype = ICH_converted$Phenotype)
rownames(heatmap_annotations)<-rownames(hp_data)

# Render heatmaps
hmp_1 <- heatmap(hp_data, Colv= NULL, scale = "none")
hmp_2 <- pheatmap (scaled_data, cluster_rows = TRUE, cluster_cols = FALSE, annotation_row = heatmap_annotations)


