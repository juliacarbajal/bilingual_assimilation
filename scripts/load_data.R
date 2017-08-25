# LOAD TEST DATA ####
filenames = list.files(data.folder, pattern = "^test.*.csv", full.names = TRUE)
ldf = lapply(filenames, read.table, header = T, sep = ",")
data = do.call(rbind,ldf)

# LOAD TRAINING DATA ####
training.filenames = list.files(data.folder, pattern = "^training.*.csv", full.names = TRUE)
ldf = lapply(training.filenames, read.table, header = T, sep = ",")
training.data = do.call(rbind,ldf)


# LOAD AGE & GENDER INFO ####
datafile = paste(data.folder,'age_sex.txt', sep='/')
if (file.exists(datafile)) {
  age_sex = read.table(datafile, header=T, sep=',')
} else {
  age_sex = data.frame(subject_id = unique(data$subject_id), age = NA, sex = NA)
  print('WARNING: AGE AND SEX INFO NOT AVAILABLE. REPLACING WITH NA.')
}
