# Download ONET and BLS OES data
# Data is about 50MB - depending on your connection, this might take a while.

source(file.path(rprojroot::find_root(rprojroot::has_file("pathconfig.R")),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"), echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# exclusions to not consider

exclusions.ext <- c("eps","pdf","doc","docx","ps","csv","dta","tex")

dep.files = c("environment.yml","requirements.txt",
              "project.toml","manifest.toml",
              "renv.lock")

files_db <- dbConnect(RSQLite::SQLite(), kranz.sql)
articles_db <- dbConnect(RSQLite::SQLite(), kranz.sql2)

# ingest the articles db
dbGetQuery(articles_db,"SELECt id,journ,title,year,date,vol,issue,artnum,
           article_url,has_data,data_url,article_doi,data_doi FROM article  ;") %>%
  group_by(journ) -> articles

# ingest files

dbGetQuery(files_db,"SELECT * from files;") -> files.df
names(files.df)
# [1] "id"        "file"      "file_type" "kb"        "nested"   
skim(files.df)

# file extensions
files.python = c("py")
files.julia  = c("jl")
files.notebooks = c("ipynb","qmd") # Cannot detect Pluto notebooks
files.r = c("r","rmd")
files.stata = c("do","ado","sthlp","dta")
files.m = c("m") # m files are ambiguous...
files.matlab = c("mat") # plus files.m
files.mod    = c("mod") # shared between dynare and AMPL https://twitter.com/jacob_mays/status/1687989116934225920
files.ampl   = c("dat","run")
files.ox     = c("ox")
files.mathematica = c("wl","nb","wxf","wdx","mx") # plus files.m

files.df %>%
  filter(!file_type %in% exclusions.ext) %>%
  mutate(lft = tolower(file_type),
         present_python=lft %in% files.python,
         present_julia =lft %in% files.julia,
         present_notebooks = lft %in% files.notebooks,
         present_r     = lft %in% files.r,
         present_stata = lft %in% files.stata,
         tmp_m         = lft %in% files.m,
         present_matlab= lft %in% files.matlab,
         present_mathematica = lft %in% files.mathematica,
         # disambiguate m files
         present_matlab= ( present_matlab | ( tmp_m & ! present_mathematica)), # use m files if no other mathematica files are there
         present_mathematica = (present_mathematica | (tmp_m & ! present_matlab)),
         tmp_mod       = lft %in% files.mod,
         present_ampl  = lft %in% files.ampl,
         # same thing here
         present_dynare = ( tmp_mod & ! present_ampl),
         present_ampl   = ( tmp_mod & ! present_dynare),
         present_ox     = lft %in% files.ox
         ) -> files_main
  names(files_main)
head(files_main)
skim(files_main)

dbDisconnect(files_db)

# merge to article db to get journals
# id|year|date|journ|title|vol|issue|artnum|article_url|

files_main %>% 
  left_join(dbGetQuery(articles_db,"SELECt id,year,date,journ FROM article
;"),
            by="id") -> analysis_software

# Test
articles %>%  distinct(journ,.keep_all = TRUE)

# transform by journal specific pattern (in the absence of DOI)
articles %>%
  mutate(publisher = case_when(
    str_detect(article_url,fixed("aeaweb")) ~ "aea",
    str_detect(article_url,fixed("oup.com")) ~ "oup",
    str_detect(article_url,fixed("uchicago")) ~ "ucp",
    str_detect(article_url,fixed("econometric")) ~ "ecta",
    TRUE ~ "unknown"),
    article_doi = case_when(
      !is.na(article_doi) ~ article_doi,
      # https://www.aeaweb.org/articles?id=10.1257/aer.20150361  
      publisher == "aea" ~ str_remove(article_url,fixed("https://www.aeaweb.org/articles?id=")),
      # https://academic.oup.com/restud/article/81/1/1/1727641
      # publisher == "oup" ~ cannot be transformed
      # https://www.journals.uchicago.edu/doi/abs/10.1086/704494     
      publisher == "ucp" ~ str_remove(article_url,fixed("https://www.journals.uchicago.edu/doi/abs/"))
      # https://www.econometricsociety.org/publications/econometrica/2019/01/01/aggregate-betting-data-individual-risk-preferences
      # publisher == "ecta" ~ cannot be transformed
    )
  )

# We may need to match on vol/issue/artnum instead for other journals



# by journal


# by journal
analysis_software %>% 
  group_by(journ,id) %>%
  summarize(across(starts_with("present_"),max)) %>%
  ungroup() %>%
  group_by(journ) %>%
  summarize(articles=n(),
            across(starts_with("present_"),sum)) %>%
  pivot_longer(starts_with("present_")) %>%
  mutate(Percent = 100*value/articles)  -> analysis_software.byj.table
analysis_software.byj.table

# write out tables

write.csv(analysis_software.byj.table,file.path(generated,"analysis-software-byj.csv"))

print(
  xtable(analysis_software.byj.table,
         caption="Software use in main Econ journals",
         label="tab:software:byjournal"),
  caption.placement = "top",
  file=file.path(generated,"table_analysis_software_byj.tex"))

# now summarize across all journals

analysis_software.byj.table %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(articles = sum(articles),
            value = sum(value)) %>%
  mutate(articles = max(articles),
         Percent = 100*value/articles) -> analysis_software.table

# write out tables

write.csv(analysis_software.table,file.path(generated,"analysis-software.csv"))

print(
  xtable(analysis_software.table,
         caption="Software use in main Econ journals",
         label="tab:software"),
  caption.placement = "top",
  file=file.path(generated,"table_analysis_software.tex"))


# by year and software
analysis_software %>% 
  group_by(year,id) %>%
  summarize(across(starts_with("present_"),max)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(articles=n(),
            across(starts_with("present_"),sum)) %>%
  pivot_longer(starts_with("present_")) %>%
  mutate(software=str_remove(name,"present_")) %>%
  select(-name) %>%
  mutate(Percent = 100*value/articles)  -> analysis_software.byy.table
analysis_software.byy.table

# write out tables

write.csv(analysis_software.byy.table,file.path(generated,"analysis-software-byy.csv"))

print(
  xtable(analysis_software.byy.table,
         caption="Software use in main Econ journals",
         label="tab:software:byyear"),
  caption.placement = "top",
  file=file.path(generated,"table_analysis_software_byy.tex"))

library(ggplot2)
library(viridis)
library(ggrepel)

# get end points

data_ends <- analysis_software.byy.table %>%
  group_by(software) %>%
  filter(year == max(year)) %>%
  mutate(Percent = round(Percent,1)) %>%
  filter(Percent > 5) %>%
  pull(Percent)

data_ends.words <- analysis_software.byy.table %>%
  group_by(software) %>%
  mutate(label = if_else(Percent > 3.5,software,""),
         # add an Other label
         label = if_else(software=="dynare","Other",label),
         label = if_else(year == max(year),label,NA_character_))

g<- ggplot(data_ends.words, 
       aes(x=year, y=Percent, color=software)) +
  geom_line() +
  geom_point() +
  #scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends.words)) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color="grey90", 
                                          linewidth = 1,
                                          linetype="dotted"
                                          ),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_color_viridis_d(option = "viridis",direction = -1,guide="none") +
  labs(title="Frequency of Software by Year",
       x="Year",
       y=element_blank())

# write out stuff 
write.csv(analysis_software.byy.table,file.path(generated,"analysis-software-byy.csv"))

print(
  xtable(analysis_software.byy.table %>% select(year,software,Percent) %>% 
           filter(year>2010) %>%
           pivot_wider(names_from=year,values_from = Percent),
         caption="Software use in main Econ journals",
         label="tab:software:byyear"),
  caption.placement = "top",
  file=file.path(generated,"table_analysis_software_byy.tex"))

ggsave(filename=file.path(generated,"analysis-software-byy.png"),g)

dbDisconnect(articles_db)

