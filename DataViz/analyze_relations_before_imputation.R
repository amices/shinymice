# set-up environment
library(mice) # Data imputation
library(dplyr) # Data manipulation
library(magrittr)
library(broom)
library(ggplot2)
set.seed(123)

# check pattern
md.pattern(boys)

# save pattern
mpat <- md.pattern(boys, plot = FALSE)

# investigate missingness in gen visually
Rgen <- is.na(boys$gen)
levels(Rgen) <- c("Missing", "Observed")
data <- boys %>% as_tibble %>% cbind(Rgen)
lattice::histogram( ~ gen, data = boys)
lattice::histogram( ~ age | Rgen, data = boys)
# now with ggplot:
data %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  facet_wrap( ~ Rgen) # optional: use argument "ncols = 1"
# now with the right colors:
colorspace::qualitative_hcl(1, 240, 100, 40) #observed
## #006CC2
colorspace::qualitative_hcl(1, 0, 100, 40) #missing
## #B61A51
# or even better: plot on top of each other
data %>%
  ggplot(aes(x = age, fill = is.na(gen))) +
  geom_histogram(aes(y = 100*..density..),
                 alpha = 0.5, position = 'identity') +
  xlab("Age (years)") + ylab("Percentage observed") + 
  scale_fill_manual(name = "Gen", breaks = c("FALSE", "TRUE"), 
                    labels = c("Observed","Missing"), values = c("#006CC2", "#B61A51"))  

## Maar dit is verwarrend misschien omdat de Age values niet missing zijn, maar de gen values...


# investigate numerically
out_Rgen <- lm(age ~ Rgen, data = data, na.action = "na.omit")
out_Rgen <- data %>% lm(age ~ is.na(gen), data = .)
out_gen <- data %>% lm(age ~ gen, data = .)
#tidy(out_Rgen)
#tidy(out_gen)
## the age of gen missing and observed differs
## age should be included in the imp model!

## also do this for missing*missing? and obs*obs?
data <- data %>% mutate(
  Rage = is.na(data$age),
  Rbmi = is.na(data$bmi),
  Rhgt = is.na(data$hgt),
  Rwgt = is.na(data$wgt)
)
Rall <- sapply(boys, is.na)
# dan met grepl: grepl("bmi", names(boys))
# lm(boys$bmi~., data = as.data.frame(Rall))
# dus als daar NAs in zitten dan moeten we die var altijd meenemen!
# dus geobserveerd hangt dan niet samen met ...
# lm(boys$bmi~., data=as_tibble(sapply(boys, is.na)))

# test for all miss indicators and age
out_all <-
  lm(age ~ Rgen + Rbmi + Rhgt + Rwgt,
     data = data,
     na.action = "na.omit")
tidy(out_all)
# intercept only
out_int <- lm(age ~ 1, data = data)
out_int_gen <- data %>% filter(!is.na(gen)) %>% lm(age~1, data=.)
#tidy(out_int)
# compare
AIC(out_all, out_int)

# or use anova
anova(out_int, out_Rgen)
anova(out_int_gen, out_gen)

