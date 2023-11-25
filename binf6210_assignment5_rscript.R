library("tidyverse")

protein_groups_file <- 'proteinGroupscopy.txt'

msdata <- read.table(protein_groups_file, header = T, sep = '\t')

#select columns of interest from msdata 
msdata2 <- msdata %>%
  select(matches("LFQ"), Only.identified.by.site, Reverse, Potential.contaminant, Taxonomy.IDs, Taxonomy.names, Peptides, Razor...unique.peptides, Unique.peptides, Sequence.coverage...., Unique...razor.sequence.coverage...., Unique.sequence.coverage...., Mol..weight..kDa., Q.value, Score, Intensity, MS.MS.count, Protein.IDs, Majority.protein.IDs, id)


#remove potential contaminants, peptides identified by site, and reverse peptides
msdata3 <- msdata2 |>
  filter(Potential.contaminant != '+') |>
  filter(Only.identified.by.site != '+') |>
  filter(Reverse != '+')

#number of unique proteins in each replicate 

lfq_names <- c()


for (i in colnames(msdata3)) {
  if (grepl("LFQ", i)) {
    lfq_names <- append(lfq_names, i)
  }
}

#creating an empty vector to store the number of proteins in each LFQ column
lfq_num_p = c()
#for loop that loops through each LFQ column and adds the sum of proteins to the lfq_num_p vector.
for (i in lfq_names) {
  lfq_num_p <- append(lfq_num_p, sum(msdata3[[i]] != 0))
}

#create a data frame by combining the lfq_names and lfq_num_p vectors
number_of_proteins <- data.frame(Name = lfq_names,
                                 Total = lfq_num_p )

view(number_of_proteins)

#log2 transform LFQ values 
msdata4 <- msdata3 |>
  mutate(across(matches("LFQ"), log2))



