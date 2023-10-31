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



files.df %>%
  filter(!file_type %in% exclusions.ext) %>%
  mutate(present_main=str_detect(tolower(file),"main"),
         present_master=str_detect(tolower(file),"master"),
         present_dockerfile=str_detect(tolower(file),"dockerfile"),
         present_apptainer=(str_detect(tolower(file),"apptainer") |
                            str_detect(tolower(file),"singularity") |
                            tolower(file_type) %in% c("sif"))) %>%
  # dependency files
  mutate(present_dependency=str_detect(tolower(file),paste(dep.files, collapse = "|")))-> files_main
  names(files_main)
head(files_main)

head(files_main %>% filter(present_main))

dbDisconnect(files_db)

# merge to article db to get journals
# id|year|date|journ|title|vol|issue|artnum|article_url|

files_main %>% 
  left_join(dbGetQuery(articles_db,"SELECt id,year,date,journ FROM article
;"),
            by="id") -> analysis_main

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
analysis_main %>% 
  group_by(journ,id) %>%
  summarize( present_main=max(present_main),
             present_master=max(present_master)) %>%
  ungroup() %>%
  group_by(journ) %>%
  summarize(n=n(),
            main_n=sum(present_main),
            master_n=sum(present_master),
            any_n = sum(present_master | present_main)) -> tmp
tmp %>%
  # add a column sum
  bind_rows(
    tmp %>% ungroup() %>% summarize(n=sum(n),
                                    main_n = sum(main_n),
                                    master_n=sum(master_n),
                                    any_n   =sum(any_n))
  ) %>%
  mutate(any_pct = 100 * any_n/n,
         journ = replace_na(journ,"Total")) -> analysis_main.byj.table
analysis_main.byj.table

# write out tables

write.csv(analysis_main.byj.table,file.path(generated,"analysis-main-byj.csv"))

print(
  xtable(analysis_main.byj.table,
         caption="Presence of main or master file in main Econ journals",
         label="tab:master:byjournal"),
  caption.placement = "top",
  file=file.path(generated,"table_analysis_main_byj.tex"))



### Now find "Dockerfiles"


# summarize
articles <- nrow(files_main %>% distinct(id))
files_main %>% 
  #group_by(journ,id) %>%
  group_by(id) %>%
  mutate(present_any = (present_master | present_main)) %>%
  summarize(across(starts_with("present_"),max),) %>%
  ungroup() %>%
  summarize(present_n=n(),
            across(starts_with("present_"),sum)) %>%
  pivot_longer(starts_with("present_")) %>%
  mutate(Percent = 100*value/articles)-> analysis_main.table

write.csv(analysis_main.table,file.path(generated,"analysis-main.csv"))

print(
  xtable(analysis_main.table,
         caption="Presence of main or master file in main Econ journals",
         label="tab:master"),
  caption.placement = "top",
  file=file.path(generated,"table_analysis_main.tex"))


dbDisconnect(articles_db)

