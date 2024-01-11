#### libraries ----------------------------------------------------------- ####
library(pacman)
p_load(bartik.weight, tidyverse, haven, janitor, kableExtra, Rcpp, fixest, modelsummary)

sourceCpp('code/bw.cpp')

bw = function(master, y, x, controls = NULL, weight = NULL, local, Z, global, G) {
  
  # Parsing the master file
  y = master[[y]]
  x = master[[x]]
  n = length(x)
  
  if (is.null(weight)) {
    weight = diag(n)
  } else {
    weight = diag(master[[weight]], n, n)
  }
  
  if (is.null(controls)) {
    WW = matrix(1, n, 1)
  } else {
    W = as.matrix(master[controls])
    WW = cbind(W, matrix(1, n, 1))
  }
  
  # Parsing the local file
  Z = as.matrix(local[Z])
  
  # Parsing the global file
  G = global[[G]]
  
  # Compute the Rotemberg weights (alpha) and the just-identified coefficients (beta)
  alpha_beta = ComputeAlphaBeta(y, x, WW, weight, Z, G)
  
  # Return a tibble
  tibble::as_tibble(cbind(global, alpha = alpha_beta[[1]], beta = alpha_beta[[2]], gam = alpha_beta[[3]], pi = alpha_beta[[4]]))
}

### read data ------------------------------------------------------------ ####
hgl_raw = haven::read_dta("data/finaldata.dta")

# drop Hawaii and Alaska
hgl_master = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15))

### some goodies for later ----------------------------------------------- ####
countries = c("uk", "ire", "it", "ger", "pol", "rus", "eur", "mex", "pr", "can", 
              "cam", "sam", "car", "cub", "chi", "ind", "asi", "wor")

elect_1980 = hgl_raw %>% select(contains("elect1980")) %>% names()

regions = c("plains", "mideast", "glakes", "southeast", "southwest", 
              "mountain", "farwest") 

regions80 = hgl_raw %>% select(contains(regions) & ends_with("80")) %>% names()

oth_vars  = c("dage", "dldod", "larea", "lpop1940", "lpcpersinc1940")




oth_vars_nocol  = c("dage", "dldod", "larea", "lpop1940",
                    "lpcpersinc1940")

dummies_year = hgl_raw %>%
  select(starts_with("ydum")) %>%
  select(-c("ydum6", "ydum9", "ydum1", "ydum2")) %>%
  names()

dummies_state = hgl_raw %>%
  select(starts_with("sdum")) %>%
  select(-c("sdum2", "sdum9", "sdum12", "sdum22", "sdum25", "sdum29")) %>%
  names()

dummies_elect = hgl_raw %>%
  select(starts_with("elect1980")) %>%
  select(-elect1980_40) %>%
  names()

sharen = hgl_raw %>%
  select(starts_with("sharen1940")) %>%
  names()

controls = c(oth_vars, dummies_year, regions)
controls80 = c(oth_vars, dummies_year, regions80)
controls_nocol = c(oth_vars_nocol, dummies_year, regions)
controls_nocol80 = c(oth_vars_nocol, dummies_year, regions80)
controls2 = c(oth_vars, dummies_year)
controls_post = c("dnpostshare", oth_vars_nocol, dummies_year, regions)
controls_post80 = c("dnpostshare", oth_vars_nocol, dummies_year, regions80)

weight = "pop_w1"

y = "dlmpatentspc" #chr of var name: change in log patents per capita
x = "dfcollshare" #chr of var name: change in foreign college share
x2 = "dfpostshare" #chr of var name: change in foreign college share
x3 = "dfengscishare" #chr of var name: change in foreign college share

### prepare the share piece ---------------------------------------------- ####
# Card: immigrants from country (group) k in location l in 1980
# HGL: state i’s share in 1940 of the national total of immigrants 
# who originate from region k, all education levels

# transform hgl data to wide format

hgl_local = hgl_raw %>%
  filter(year %in% 1950:2000, !state %in% c(2, 15)) %>% # HGL say data 1950-2000
  select(year, state, starts_with("sharen1940"), popl) %>%
  pivot_longer(cols = -c("year", "state", "popl"), names_to = "iso", values_to = "sh1940_") %>%
  mutate(iso = str_remove(iso, "sharen1940"),
         iso = str_glue("t{year}_sh_ind_{iso}")) %>%
  pivot_wider(names_from = iso, values_from = sh1940_, values_fill = 0) %>%
  mutate(across(.cols = -c("year", "state", "popl"), ~ .x / popl)) %>%
  select(-popl)

z = setdiff(names(hgl_local), c("year", "state")) # base period shares

### prepare the shift piece ---------------------------------------------- ####
# Prepare variables in the global tibble (ADH_global in vignette)
# Card: the number of people arriving in the US from 1990 to 2000 from country 
# (group) k and skill group j.
# HGL: national change in the number of skilled immigrants from that region

hgl_global = hgl_raw %>% 
  filter(year %in% 1950:2000, !state %in% c(2, 15)) %>% 
  select(year,
         contains("dcollnoah") & contains(countries)) %>% 
  pivot_longer(cols = !c("year"), 
               names_to = "iso",
               values_to = "imm_") %>%
  distinct() %>%
  mutate(iso = str_remove(iso, "dcollnoah"))

hgl_global2 = hgl_raw %>% 
  filter(year %in% 1950:2000, !state %in% c(2, 15)) %>% 
  select(year,
         contains("dpostnoah") & contains(countries)) %>% 
  pivot_longer(cols = !c("year"), 
               names_to = "iso",
               values_to = "imm_") %>%
  distinct() %>%
  mutate(iso = str_remove(iso, "dpostnoah"))

g = "imm_" # contemp. immigration

### bartik weights regression -------------------------------------------- ####

# dfcollshare
bw = bw(master = hgl_master, 
        y = y, 
        x = x, 
        controls = controls,
        weight = weight,
        local = hgl_local, 
        Z = z, 
        global = hgl_global, 
        G = g)

# dfpostshare
bw2 = bw(master = hgl_master, 
        y = y, 
        x = x2, 
        controls = controls,
        weight = weight,
        local = hgl_local, 
        Z = z, 
        global = hgl_global2, 
        G = g)

# coefficient check
bw  %>% 
  mutate(coeff = alpha*beta) %>%
  summarise(coeff = sum(coeff)) 

bw2  %>% 
  mutate(coeff = alpha*beta) %>%
  summarise(coeff = sum(coeff))

# sum of pos weights
bw %>% 
  filter(alpha > 0) %>%
  mutate(pos_weight_sum = sum(alpha*beta))

# sum of neg weights
bw %>% 
  filter(alpha < 0) %>%
  mutate(pos_weight_sum = sum(alpha*beta))

# sum of pos weights
bw2 %>% 
  filter(alpha > 0) %>%
  mutate(pos_weight_sum = sum(alpha*beta))

# sum of neg weights
bw2 %>% 
  filter(alpha < 0) %>%
  mutate(pos_weight_sum = sum(alpha*beta))



# PANEL A (share of negative weights COLLEGE)
neg_weights1 = bw %>%
  mutate(pos_weight = ifelse(alpha > 0, "Positive", "Negative"),
         share = if_else(pos_weight == "Negative", sum(alpha < 0) / n(), sum(alpha > 0) / n())) %>%
  group_by(pos_weight) %>%
  summarise(sum = round(sum(alpha), 3),
            mean = round(mean(alpha), 3),
            share = round(mean(share), 3)
            ) 

neg_weights2 = bw2 %>%
  mutate(pos_weight = ifelse(alpha > 0, "Positive", "Negative"),
         share = if_else(pos_weight == "Negative", sum(alpha < 0) / n(), sum(alpha > 0) / n())) %>%
  group_by(pos_weight) %>%
  summarise(sum = round(sum(alpha), 3),
            mean = round(mean(alpha), 3),
            share = round(mean(share), 3)
  ) 

neg_weights = rbind(neg_weights1, neg_weights2) 

neg_weights %>%
  kbl(col.names = c(" ", "Sum", "Mean", "Share"), booktabs = T, 
      format = "latex") %>%
  # kable_classic() %>%
  pack_rows("Panel A. College graduates", 1, 2) %>%
  pack_rows("Panel B. Post-college", 3, 4) %>%
  save_kable(file = "tables/neg_weights.tex", self_contained = FALSE)

# PANEL B
agg_bw = bw %>% 
  group_by(iso) %>%
  summarize(alpha = mean(alpha),
            beta = mean(beta),
            g = mean(imm_)) %>%
  select(-iso)
  
corr_bw = cor(agg_bw) 
term = c("$\\hat{\\alpha_k}$", "$\\hat{\\beta_k}$", "$g_k$")

agg_bw2 = bw2 %>% 
  group_by(iso) %>%
  summarize(alpha = mean(alpha),
            beta = mean(beta),
            g = mean(imm_)) %>%
  select(-iso)

corr_bw2 = cor(agg_bw2) 

corr_table = as_tibble(cbind(term, rbind(corr_bw, corr_bw2))) %>%
  mutate(across(.cols = !term, ~ as.numeric(.x)))

corr_table %>%
  kbl(col.names = c(" ", "$\\hat{\\alpha_k}$", "$\\hat{\\beta_k}$", "$g_k$"),
    booktabs = T, format = "latex", escape = FALSE, digits = 3) %>%
  pack_rows("Panel A. College graduates", 1, 3) %>%
  pack_rows("Panel B. Post-college", 4, 6) %>%
  save_kable(file = "tables/corr_bw.tex", self_contained = FALSE)

# PANEL C

# PANEL D (top 5 weighted countries)
top5_col = bw %>% 
  arrange(desc(alpha)) %>% 
  # group_by(year) %>%
  slice_max(n = 5, alpha) %>% 
  arrange(desc(alpha)) %>% 
  rename(g = imm_) 

top5_post = bw2 %>% 
  # group_by(year) %>%
  arrange(desc(alpha)) %>% 
  slice_max(n = 5, alpha) %>% 
  arrange(desc(alpha)) %>% 
  rename(g = imm_) 

top_5 = rbind(top5_col, top5_post)

top_5 %>%
  mutate(iso = case_when(
    iso == "asi" ~ "Other Asia",
    iso == "chi" ~ "China",
    iso == "sam" ~ "South America",
    iso == "car" ~ "Other Caribbean",
    iso == "rus" ~ "Russia",
    iso == "pol" ~ "Poland",
    TRUE ~ iso
  ), 
         iso_year = str_glue("{iso}, {year}")) %>%
  select(iso_year, alpha, g, beta) %>%
  kbl(col.names = c(" ", "$\\hat{\\alpha_k}$", "$g_k$", "$\\hat{\\beta_k}$"), booktabs = T, 
    format = "latex", escape = FALSE, digits = 3) %>%
  # kable_classic() %>%
  pack_rows("Panel A. College graduates", 1, 5) %>%
  pack_rows("Panel B. Post-college", 6, 10) %>%
  save_kable(file = "tables/top_5.tex", self_contained = FALSE)

# plot for top 5 Rotemberg weight countries per year
bw %>%
  group_by(year) %>%
  slice_max(n = 5, alpha) %>% 
  arrange(desc(alpha)) %>% 
  mutate(iso = case_when(
    iso == "asi" ~ "Other Asia",
    iso == "chi" ~ "China",
    iso == "sam" ~ "South America",
    iso == "car" ~ "Other Caribbean",
    iso == "rus" ~ "Russia",
    iso == "pol" ~ "Poland",
    iso == "wor" ~ "Rest of World",
    iso == "mex" ~ "Mexico",
    iso == "ind" ~ "India",
    TRUE ~ iso
  )) %>%
  ggplot(aes(x = year, y = alpha, color = iso)) +
  geom_point(data = bw %>% group_by(year) %>% slice_max(n = 5, alpha) %>% arrange(desc(alpha)) %>% filter(year <= 1970),
             show.legend = FALSE) +
  geom_text(aes(label = ifelse(year > 1970, iso, " ")),
            position=position_jitter(width = 0,height = 0),
            show.legend =  FALSE) +
  theme_minimal() +
  ylim(0.0, 0.20) + 
  scale_x_continuous(limits = c(1950, 2005)) + 
  labs(y = expression(paste("Weight (", hat(alpha), ")")),
       x = "Year")

ggsave(file = "figures/top5_col.eps",
       device = "eps",
       height = 4,
       width = 5.5)

# plot for top 5 Rotemberg weight countries per year
bw2 %>%
  group_by(year) %>%
  slice_max(n = 5, alpha) %>% 
  arrange(desc(alpha)) %>% 
  mutate(iso = case_when(
    iso == "asi" ~ "Other Asia",
    iso == "chi" ~ "China",
    iso == "sam" ~ "South America",
    iso == "car" ~ "Other Caribbean",
    iso == "rus" ~ "Russia",
    iso == "pol" ~ "Poland",
    iso == "wor" ~ "Rest of World",
    iso == "mex" ~ "Mexico",
    iso == "ind" ~ "India",
    TRUE ~ iso
  )) %>%
  ggplot(aes(x = year, y = alpha, color = iso)) +
  geom_point(data = bw2 %>% group_by(year) %>% slice_max(n = 5, alpha) %>% arrange(desc(alpha)) %>% filter(year <= 1970),
             show.legend = FALSE) +
  geom_text(aes(label = ifelse(year > 1970, iso, " ")),
            position=position_jitter(width = 0,height = 0),
            show.legend =  FALSE) +
  theme_minimal() +
  ylim(0.0, 0.20) + 
  scale_x_continuous(limits = c(1950, 2005)) + 
  labs(y = expression(paste("Weight (", hat(alpha), ")")),
       x = "Year")

ggsave(file = "figures/top5_post.eps",
       device = "eps",
       height = 4,
       width = 5.5)

bw %>%
  mutate(iso = case_when(
    iso == "asi" ~ "Other Asia",
    iso == "chi" ~ "China",
    iso == "sam" ~ "South America",
    iso == "car" ~ "Other Caribbean",
    iso == "rus" ~ "Russia",
    iso == "pol" ~ "Poland",
    iso == "wor" ~ "Rest of World",
    iso == "mex" ~ "Mexico",
    iso == "ind" ~ "India",
    TRUE ~ iso
  )) %>%
  ggplot(aes(x = beta, y = alpha, color = iso)) +
  geom_text(aes(label = ifelse(alpha > 0.05, str_glue("{iso}_{year}"), " ")), 
            show.legend =  FALSE,
            position=position_jitter(width = 0,height = 0.01)) + 
  geom_point(data = bw %>% filter(alpha <= 0.05),
             show.legend = FALSE) +
  theme_minimal() +
  xlim(-1500,1000) +
  ylim(-0.05, 0.20) + 
  labs(y = expression(paste("Weight (", hat(alpha), ")")),
      x = expression(paste("Effect (", hat(beta), ")")))

ggsave(file = "figures/coefs_col.eps",
       device = "eps",
       height = 4,
       width = 5.5)

bw2 %>%
  mutate(iso = case_when(
    iso == "asi" ~ "Other Asia",
    iso == "chi" ~ "China",
    iso == "sam" ~ "South America",
    iso == "car" ~ "Other Caribbean",
    iso == "rus" ~ "Russia",
    iso == "pol" ~ "Poland",
    iso == "wor" ~ "Rest of World",
    iso == "mex" ~ "Mexico",
    iso == "ind" ~ "India",
    TRUE ~ iso
  )) %>%
  ggplot(aes(x = beta, y = alpha, color = iso)) +
  geom_text(aes(label = ifelse(alpha > 0.05, str_glue("{iso}_{year}"), " ")), 
            show.legend =  FALSE,
            position=position_jitter(width = 0,height = 0.01)) + 
  geom_point(data = bw2 %>% filter(alpha <= 0.05),
             show.legend = FALSE) +
  theme_minimal() +
  xlim(-1500,1000) +
  ylim(-0.05, 0.20) + 
  labs(y = expression(paste("Weight (", hat(alpha), ")")),
       x = expression(paste("Effect (", hat(beta), ")")))

ggsave(file = "figures/coefs_post.eps",
       device = "eps",
       height = 4,
       width = 5.5)


# Location chars --------------------------------------------------

loc_1 = feols(sharen1940asi ~ lpop1940 + lpcpersinc1940,
                data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                weights = ~pop_w1,
                cluster = ~state)

loc_2 = feols(sharen1940chi ~ lpop1940 + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_3 = feols(sharen1940sam ~ lpop1940 + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_4 = feols(sharen1940car ~ lpop1940 + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_5 = feols(sharen1940rus ~ lpop1940 + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_6 = feols(sharen1940wor ~ lpop1940  + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_7= feols(dinstr1940pp ~ lpop1940 + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_8 = feols(dinstrpost1940pp ~ lpop1940  + lpcpersinc1940,
              data = hgl_master %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
              weights = ~pop_w1,
              cluster = ~state)

loc_list = list(loc_1, loc_2, loc_3, loc_4, loc_5, loc_6)
loc_list2 = list(loc_1, loc_2, loc_3, loc_4, loc_5, loc_6, loc_7, loc_8)

loc_table = modelsummary(loc_list, output = "data.frame",
             coef_rename = c("lpop1940" = "Population 1940 (log)",
                             # "popdensity1940" = "Population density 1940",
                             "lpcpersinc1940" = "State personal income per capita 1940 (log)"),
             coef_omit = "Intercept",
             stars = T,
             gof_map = c("nobs", "r.squared")) 

loc_table2 = modelsummary(loc_list2, output = "data.frame",
                         coef_rename = c("lpop1940" = "Population 1940 (log)",
                                         # "popdensity1940" = "Population density 1940",
                                         "lpcpersinc1940" = "State personal income "),
                         coef_omit = "Intercept",
                         stars = T,
                         gof_map = c("nobs", "r.squared")) 

loc_table2 %>%
  select(-c(part, statistic)) %>%
  mutate(term = ifelse(row_number() %% 2, term, " "),
         term = ifelse(row_number() == 6, "$R^2$", term),
         term = case_when(
           row_number() == 4 ~ "\\vspace{1em} per capita 1940 (log)",
           TRUE ~ term)) %>%
  kbl(col.names = c(" ", " ", " ", " ", " ", " ", " ", " ", " "), booktabs = T, 
      align = c("l","c","c", "c", "c", "c", "c", "c", "c"),
      format = "latex", escape = F, linesep = "\\addlinespace") %>%  
  add_header_above(c(" " = 1, "\\\\shortstack{Other\\\\\\\\\\Asia}" = 1, "China" = 1, "\\\\shortstack{South\\\\\\\\\\America}" = 1, 
                     "\\\\shortstack{Other\\\\\\\\\\Caribbean}" = 1, "Russia" = 1, "\\\\shortstack{Rest of\\\\\\\\\\World}" = 1,
                     "\\\\shortstack{Bartik\\\\\\\\\\College}" = 1, "\\\\shortstack{Bartik\\\\\\\\\\Post-college}" = 1), 
                   line = F, escape = F) %>%
  save_kable(file = "tables/loc_chars.tex", self_contained = FALSE) 

# Pre-trends --------------------------------------------------
pretrend_vars = c("sharen1940asi", "sharen1940chi", "sharen1940sam", 
                  "sharen1940rus", "sharen1940wor", "sharen1940car", 
                  "dinstr1940pp", "dinstrpost1940pp")

models = list() # empty list for storing
pre_models = list() # empty list for storing

i = 1 # set index

for (v in pretrend_vars) {
  for (y in seq(1950, 2000, 10)) {
    var = c(v)
    models[[i]] = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] + .[var],
               data = hgl_raw %>% filter(year %in% y, !state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)
    
    i = i + 1
  }
}

i = 1 # reset index
for (m in models){
  pre_models[[i]] = get_estimates(m) %>% 
    select(term, estimate, std.error) %>% 
    filter(term %in% pretrend_vars) %>%
    mutate(set = i)
  i = i + 1
}

pre_df = do.call(rbind, pre_models)

test = pre_df %>%
  group_by(term) %>%
  mutate(year = case_when(
    row_number() == 1 ~ "1950",
    row_number() == 2 ~ "1960",
    row_number() == 3 ~ "1970",
    row_number() == 4 ~ "1980",
    row_number() == 5 ~ "1990",
    row_number() == 6 ~ "2000",
  ),
  estimate = as.numeric(estimate),
  std.error = as.numeric(std.error),
  upper = estimate + 1.96*std.error,
  lower = estimate - 1.96*std.error, 
  label = case_when(
    term == "sharen1940asi" ~ "Other Asia",
    term == "sharen1940chi" ~ "China",
    term == "sharen1940sam" ~ "South America",
    term == "sharen1940car" ~ "Other Caribbean",
    term == "sharen1940rus" ~ "Russia",
    term == "sharen1940pol" ~ "Poland",
    term == "sharen1940wor" ~ "Rest of World",
    term == "sharen1940mex" ~ "Mexico",
    term == "sharen1940ind" ~ "India",
    term == "dinstr1940pp" ~ "Bartik College",
    term == "dinstrpost1940pp" ~ "Bartik Post-college",
    TRUE ~ term
  )) %>% 
  ungroup()

ggplot(data = test %>% filter(!(term %in% c("dinstr1940pp", "dinstrpost1940pp"))),
       mapping = aes(x = year,
                     y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymax = upper,
               ymin = lower),
               width = 0.25) +
  facet_wrap(~label, nrow = 3) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = " ",
       y = " ")

ggsave(file = "figures/pretrends_countries.eps",
       device = "eps",
       height = 8,
       width = 5.5)

ggplot(data = test %>% filter(term %in% c("dinstr1940pp", "dinstrpost1940pp")),
       mapping = aes(x = year,
                     y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymax = upper,
                    ymin = lower),
                width = 0.25) +
  facet_wrap(~label) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = " ",
       y = " ")

ggsave(file = "figures/pretrends_instruments.eps",
       device = "eps",
       height = 2.8,
       width = 5.5)

ggplot(data = test %>% filter(term %in% c("dinstr1940pp", "dinstrpost1940pp")),
       mapping = aes(x = year,
                     y = estimate)) +
  geom_point(color = "white") + 
  geom_point(data = test %>% filter(term %in% c("dinstr1940pp", "dinstrpost1940pp") & year != "1960")) + 
  geom_errorbar(data = test %>% filter(term %in% c("dinstr1940pp", "dinstrpost1940pp") & year != "1960"),
                aes(ymax = upper,
                    ymin = lower),
                width = 0.25) +
  facet_wrap(~label) + 
  theme_minimal() + 
  ylim(-150, 150) + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = " ",
       y = " ")

ggsave(file = "figures/pretrends_instruments_no60.eps",
       device = "eps",
       height = 2.8,
       width = 5.5)


# Alternative estimators --------------------------------------------------
library("ManyIV")

# need to get the pieces of the instruments
hgl_wide = hgl_raw %>%
  filter(year %in% c(1950:2000),
         !state %in% c(2, 15)) %>% # HGL say data 1950-2000
  select(year, state, starts_with("dcoll"), starts_with("sharen1940"), dinstr1940pp, popl) %>%
  select(contains("noah"), starts_with("sharen1940"), state, year, dinstr1940pp, popl) %>%
  # select(!contains("noah") & contains(countries), starts_with("sharen1940"), state, year, popl, dinstr1940pp) %>%
  pivot_longer(cols = -c(year, state, popl, dinstr1940pp),
               names_to = "iso",
               values_to = "values") %>%
  mutate(country = case_when(
    str_detect(iso, "dcollnoah") == TRUE ~ str_remove(iso, "dcollnoah"),
    # str_detect(iso, "dcoll") == TRUE ~ str_remove(iso, "dcoll"),
    str_detect(iso, "sharen1940") == TRUE ~ str_remove(iso, "sharen1940"),
    TRUE ~ "NA")) %>%
  group_by(state, year, country) %>%
  mutate(instrument = prod(values)) %>%
  ungroup() %>%
  group_by(year, state) %>%
  mutate(dinstrument = sum(instrument) / popl) 


hgl_join = left_join(hgl_master, hgl_local, by = c("year","state"))

hgl_manyiv = hgl_master %>%
  mutate(uk_inst = sharen1940uk*dcollnoahuk/popl,
         ire_inst = sharen1940ire*dcollnoahire/popl,
         it_inst = sharen1940it*dcollnoahit/popl,
         ger_inst = sharen1940ger*dcollnoahger/popl,
         pol_inst = sharen1940pol*dcollnoahpol/popl,
         rus_inst = sharen1940rus*dcollnoahrus/popl,
         eur_inst = sharen1940eur*dcollnoaheur/popl,
         mex_inst = sharen1940mex*dcollnoahmex/popl,
         pr_inst = sharen1940pr*dcollnoahpr/popl,
         can_inst = sharen1940can*dcollnoahcan/popl,
         cam_inst = sharen1940cam*dcollnoahcam/popl,
         sam_inst = sharen1940sam*dcollnoahsam/popl,
         car_inst = sharen1940car*dcollnoahcar/popl,
         cub_inst = sharen1940cub*dcollnoahcub/popl,
         chi_inst = sharen1940chi*dcollnoahchi/popl,
         ind_inst = sharen1940ind*dcollnoahind/popl,
         asi_inst = sharen1940asi*dcollnoahasi/popl,
         wor_inst = sharen1940wor*dcollnoahwor/popl,
         )

# Panel A
# Column 2 ORIGINAL
feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfcollshare ~ dinstr1940pp, 
                data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                weights = ~pop_w1,
                cluster = ~state)

# create formula for the regression
vars = names(hgl_local %>% select(-c(year, state)))
f0 = paste(vars, sep =" + ") # instruments
f1 = paste(controls_nocol, dummies_elect, sep = " + ") # controls
f2 = paste("dlmpatentspc ~ dfcollshare ", f1," | ", f0 ,sep = "")

many_out = IVreg(dlmpatentspc ~ dfcollshare + dage + dldod + larea + lpop1940 + 
                   lpcpersinc1940 + ydum3 + ydum4 + ydum5 + ydum7 + ydum8 + plains 
                 + mideast + glakes + southeast + southwest + mountain + farwest | 
                   uk_inst + ire_inst + it_inst + ger_inst + pol_inst
                 + rus_inst + eur_inst + mex_inst + pr_inst + can_inst + cam_inst + sam_inst + 
                   car_inst + cub_inst + chi_inst + ind_inst + asi_inst + wor_inst,
      data = hgl_manyiv %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)),
      inference = c("standard", "re", "il", "lil"))

# overid test
IVoverid(many_out)

many_out_df = many_out[[3]] %>% 
  select(beta) %>%
  add_row(beta = 20.3) %>%
  drop_na()

labels_alt_est = c("OLS", "TSLS", "LIML", "MBTSLS", "Bartik TSLS")

many_out_df = cbind(labels_alt_est, many_out_df)

many_out_df %>%
  kbl(col.names = c(" ", "(1)"), booktabs = T, 
    align = c("l","c"),
    format = "latex", escape = F, linesep = "\\addlinespace") %>%  
  save_kable(file = "tables/alt_est.tex", self_contained = FALSE) 

# coefficient on Bartik instrument
bw  %>% 
  mutate(coeff = alpha*beta) %>%
  summarise(coeff = sum(coeff)) 


