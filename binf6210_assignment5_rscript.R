library("tidyverse")
#assume inserting proteingroups.txt file from MaxQuant and there are two conditions to test for (e.g. WT and mutant)

protein_groups_file <- 'proteinGroupscopy.txt'

msdata <- read.table(protein_groups_file, header = T, sep = '\t')

#select columns of interest from msdata 
msdata2 <- msdata %>%
  select(matches("LFQ"), Only.identified.by.site, Reverse, Potential.contaminant, Taxonomy.IDs, Taxonomy.names, Peptides, Razor...unique.peptides, Unique.peptides, Sequence.coverage...., Unique...razor.sequence.coverage...., Unique.sequence.coverage...., Mol..weight..kDa., Q.value, Score, Intensity, MS.MS.count, Protein.IDs, Majority.protein.IDs, id)


# function for user to enter the number of groups in their data
get_num_of_groups <- function(p = "Enter the number of groups: ") {
  cat(p)
  input <- readLines(n = 1)
  return(input)
}

num_of_groups <- as.integer(get_num_of_groups())

#function for user to enter the number of samples in each group
get_num_of_samples <- function(p = "Enter the number of samples in each group: ") {
  cat(p)
  input <- readLines(n = 1)
  return(input)
}

num_of_samples <- as.integer(get_num_of_samples())

#function for user to enter the name of eacch group

get_sample_group <- function(p = "Enter a label for each group: ") {
  cat(p)
  input <- readLines(n = 1)
  input <- rep(input, times = 4)
  return(input)
}

#creating a for loop to add the name of each sample to an empty vector called "num_of_sample". The vector is pre-allocated to accomindate for the number of names based on the number of samples in each group
name_of_each_group <- list()

for (input in seq(1:(num_of_groups))) {
  name_of_each_group <- c(name_of_each_group, get_sample_group())
}





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


#replacing all 0's with NA in the LFQ columns
msdata4 <- msdata3 |>
  mutate(across(matches("LFQ"), (~ifelse(. == 0, NA, .))))

#log2 transform LFQ values 
msdata5 <- msdata4 |>
  mutate(across(matches("LFQ"), log2))

#Changing the column names so we can group each of the samples for downstream analysis

for (i in 1:(num_of_groups * num_of_samples))
  colnames(msdata5)[i] <- as.character(name_of_each_group[i])

#filter rows for valid values. Filters out peptides that are missing



#group samples together. Function where user inserts the names of the groups and regular expression caputes each of the colums that will be grouped. Maybe put this step in once the matrix is final

#user inserts number of groups and number of replicates in each group and then the name for each group and I create a function that changes the name of each column based on user input





      



