library(data.table)
library(tidyverse)
library(Hmisc)
drop_path <- "/Users/eladg/Dropbox/skewness/"
office_path <- "C:/Users/eco116/Documents/sup matirials for vsdecomp/"
dat <- fread(paste0(office_path, "full_data.csv"))
dat <- fread(paste0(drop_path, "full_data.csv"))
dat %>% glimpse()

#define sample
dat <- dat %>% 
       filter(!is.na(hr_wage) & hr_wage > 0 & hr_alloc == 0) %>%
       mutate(log_hr_wage = log(hr_wage))

#drop wages at top and bottom 5% (in each year) by setting their weight to 0 
dat <- dat %>%  
       group_by(year) %>%
       mutate(wgt_hrs = case_when(log_hr_wage <= wtd.quantile(log_hr_wage, wgt_hrs, 0.05) ~ 0,
                                  log_hr_wage >= wtd.quantile(log_hr_wage, wgt_hrs, 0.95) ~ 0,
                                  TRUE ~ wgt_hrs)) %>%
       ungroup()


#Figure 6: skewness of log hourly wages
wtd_skew <- function(x, w){
  tmp <- (x - wtd.mean(x, w)) / sqrt(wtd.var(x, w))
  wtd.mean(tmp^3, w)
}

pdf(paste0(drop_path, "figure6.pdf"), width = 6, height = 4)
dat %>% 
  group_by(year) %>%
  summarise(skew = wtd_skew(log_hr_wage, wgt_hrs)) %>%
  ggplot(aes(x = year, y = skew)) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  geom_vline(xintercept = c(1991.5, 2002.5)) +
  ggtitle("Figure 6: skewness of log hourly wages")
dev.off()
#it's slightly different from the paper

#Figure 8: Skewness Decomposition by 3-Digit Occupation
dat <- dat %>% filter(year >= 1992 & year <= 2002 & wgt_hrs > 0) 
y <- dat %>% pull(log_hr_wage)
year <- dat %>% pull(year)
w <- dat %>% pull(wgt_hrs)
X <- dat %>% select(occ) %>% as.data.frame()
decomp_object <- vs_decomp(y = y, X = X, year = year, wgt = w)  
plot(decomp_object, abs.terms = F)  
  
#Figure 25: Skewness Decomposition by Occupation and Industry 
dat <- dat %>% mutate(ind = as.factor(ind),
                      occ = as.factor(occ))

y_comp <- linear_projection(y = "log_hr_wage", X.list = list("race", "ind"), 
                            data = dat, wgt = w) 

decomp_object <- vs_decomp(X = y_comp, wgt = w, year = year)
colnames(decomp_object$components)
#summary.vs_decomp(decomp_object, 
#        sum.comp = c("3cov(epsilon^2,ind)",
#                     "3cov(epsilon^2,occ)"))                
  
plot(decomp_object, 
        plot.comp = c("3cov(epsilon^2,race)",
                     "3cov(epsilon^2,ind)"))    
  
  



       