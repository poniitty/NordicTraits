# NordicTraits

## Description
NordicTraits is a comprehensive, imputed, and openly available species-level functional trait dataset for all native and most important introduced vascular plants across Denmark, Finland, Iceland, Norway, and Sweden. This dataset includes a wide set of 44 key functional traits for 3,099 species, with no missing values.

The dataset was created by compiling and harmonizing trait data from major global databases and regional sources. Missing values were imputed using a Random Forest-based framework, incorporating phylogenetic information to improve accuracy.

## File Structure
The dataset consists of six files:

1. **NordicTraits_wide_V1.csv**: Imputed species-level trait values in a wide format.
2. **NordicTraits_long_V1.csv**: Imputed species-level trait values in a long format, including information on which values were imputed.
3. **NordicTraits_metadataTraits_V1.xlsx**: Metadata explaining the selected 44 traits.
4. **NordicTraits_metadata_species_V1.xlsx**: Species metadata, including taxonomic information and vernacular names.
5. **NordicTraits_evaluation_statistics_V1.xlsx**: Validation statistics for the imputed data.
6. **NordicTraits_metadata_traits_extended_V1.xlsx**: Metadata for all 205 traits included in the imputation model.

The dataset can be downloaded from [this](https://github.com/poniitty/NordicTraits/blob/main/final_dataset/NordicTraits_V1.zip) folder.

All computer code used to generate the dataset can be accessed at the [scripts](https://github.com/poniitty/NordicTraits/tree/main/scripts) folder. The original trait data are not provided here and need to be downloaded from the original sources first to run the scripts. All data sources are provided in the data linked article.

## How to Cite
Please cite the dataset using the following citation: 

Niittynen, P., Heikkinen, R. K., Hällfors, M. H., Määttänen, A.-M., Norros, V., & Kemppinen, J. (2026). NordicTraits: Imputed species-level functional trait dataset for vascular plants of Denmark, Finland, Iceland, Norway and Sweden. PREPRINT. *bioRxiv*. https://doi.org/10.64898/2026.03.03.709463

## Additional Information
### Dataset Coverage
The dataset covers a wide range of plant functional traits, including plant height, seed mass, leaf nitrogen content, and many others. These traits are crucial for understanding plant strategies, ecosystem processes, and predicting biodiversity responses to environmental change.

A detailed desciption of the dataset can be found in [this preprint](https://doi.org/10.64898/2026.03.03.709463)

### Data Quality and Limitations
While the dataset is comprehensive and gap-free, users should be aware of the limitations of imputed data. The reliability of imputed values depends on the amount and pattern of missing values in the original data. Users are advised to exercise caution when using imputed values for less-studied taxa or specific traits.

### Future Updates
The NordicTraits dataset is planned to be updated at irregular intervals. The most recent version can always be accessed in this GitHub repository.

## Contact Information
For questions or feedback regarding the dataset, please open on issue or contact the corresponding author.
