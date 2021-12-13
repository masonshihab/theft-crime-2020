theftdata = read.csv("../data/clean/dataclean.csv") %>% as.data.frame() %>% select(-county, -state,)

set.seed(471)

theft_samples = sample(1:nrow(theftdata), round(0.8*nrow(theftdata))) 
theft_train = theftdata %>% filter(row_number() %in% train_samples)
theft_test = theftdata %>% filter(!(row_number() %in% train_samples))

write.csv(theft_train, file = '../data/clean/theft_train.csv', row.names = FALSE)
write.csv(theft_test, file = '../data/clean/theft_test.csv', row.names = FALSE)