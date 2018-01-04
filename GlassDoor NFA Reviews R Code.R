# Objective: pre-processing GlassDoor NFA Reviews text data.

# 1: setwd() 2: load .Rdata. 3: Before closing, save .Rdata at bottom of script. 
setwd("~/Documents/PREDICT 453 Text Analytics/Glassdoor Case Study")
list.files()
load(file = "R project data.Rdata")

library(readr)
library(tidyr) # dataset manipulation (e.g. row/col transforms)
library(dplyr) # dataset cleaning & magrittr data pipelines.
library(lubridate) # date cleaning.
#library(stringr) # regex and string manipulation.
library(tidytext) # tidy sentiment analysis and tf-idf.
#library(tm) # to clean the DSIs.
#library(qdap) # to clean the DSIs. 
library(quanteda) # quant text data package. https://github.com/kbenoit/quanteda/issues/519
library(phrasemachine) # noun-phrase generator using POS tagging of openNLP.
library(stringi) # string manipulation.
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(tibble) # rownames_to_column
library(topicmodels)

rvw_df <- read_csv(file = "glassdoor-nfa-reviews (2-10-17).csv") %>%
    mutate(rvw_date = round_date(ymd_hm(rvw_date),"days"),
           # if null, call it unknown, else clean up NY and Chicago office names.
           rvwr_loc = ifelse(is.na(rvwr_location), "Unknown", 
                             ifelse(rvwr_location == "New York, NY", "New York",
                                    ifelse(rvwr_location %in% c("Chicago, IL", "Lombard, IL"),
                                           "Chicago", "Error")))) %>%
    separate(rvwr_job_title, into = c("rvwr_status", "rvwr_job_title"), 
             sep = " - ", extra = "merge") %>%
    select(-rvwr_location, -X1)

# (IGNORE) Clean the Corpus for the Pros Reviews, Cons Reviews with tm and qdap ##############################

pros_source <- VectorSource(rvw_df$pros) # interprets each element as a DSI. 
pros_corpus <- VCorpus(pros_source) # creates volatile Corpus object. 
pros_corpus[[1]][[1]]; pros_corpus[[1]][2] # preview DSI, metadata.
pros_corpus[[67]][[1]]; pros_corpus[[67]][2]

cons_source <- VectorSource(rvw_df$cons)
cons_corpus <- VCorpus(cons_source)

source("clean_corpus().R") # in my project folder. didn't remove numbers or tolower() to improve phrasematch algorithm.
clean_corpus
pros_corpus_clean <- clean_corpus(pros_corpus)
pros_corpus[[29]][[1]];pros_corpus_clean[[29]][[1]]
pros_corpus[[67]][[1]];pros_corpus_clean[[67]][[1]]

cons_corpus_clean <- clean_corpus(cons_corpus)
cons_corpus[[29]][[1]];cons_corpus_clean[[29]][[1]]
cons_corpus[[67]][[1]];cons_corpus_clean[[67]][[1]]

# write function to bring the cleaned text back to rvw_df.

pros_clean <- vector("character", nrow(rvw_df))
for (text in 1:nrow(rvw_df)) {
  pros_clean[text] <- pros_corpus_clean[[text]][[1]]
}

cons_clean <- vector("character", nrow(rvw_df))
for (text in 1:nrow(rvw_df)) {
  cons_clean[text] <- cons_corpus_clean[[text]][[1]]
}

rvw_df_clean <- bind_cols(rvw_df, data.frame(pros_clean, stringsAsFactors = FALSE), 
                          data.frame(cons_clean, stringsAsFactors = FALSE))
# remove tm corpus source and original corpus. 
remove(pros_clean, cons_clean, pros_corpus, cons_corpus, pros_source, cons_source)


# create Corpus objects for DSIs and document variable metadata with Quanteda ########################
pros_corpus <- corpus(subset(rvw_df, select = -c(cons, advice_mgmt)),
                      text_field = "pros",
                      metacorpus = list(source = "From a data.frame called rvw_df_clean",
                                        notes = paste("Pros Reviews Data acquired from a focused", 
                                                "web-crawl stored in local file called")))

cons_corpus <- corpus(subset(rvw_df, select = -c(pros, advice_mgmt)),
                      text_field = "cons",
                      metacorpus = list(source = "From a data.frame called rvw_df_clean",
                                        notes = paste("Pros Reviews Data acquired from a focused", 
                                                      "web-crawl stored in local file called",
                                                      "glassdoor-nfa-reviews (2-10-17).csv.")))


# Explore Corpus Data/Text  #########################################################################

class(pros_corpus)
# inspect the document-level variables
head(docvars(pros_corpus))
# inspect the corpus-level metadata
metacorpus(pros_corpus)

summary(pros_corpus, n = 10, showmeta = TRUE) # data.frame summary.
# plot word tokens vs. sentences written.
summary(pros_corpus) %>% 
    ggplot(aes(x= Tokens, y=Sentences, col = rvwr_loc)) + 
    geom_jitter() +
    ggtitle("Sentences vs. Word Frequencies by Location")
# plot Reviewer Location and Proportion of Current/Former Employees
summary(cons_corpus) %>% 
    ggplot(aes(x= rvwr_loc, fill = rvwr_status)) + geom_bar(position = "dodge") +
    labs(x = "Office Location", y = "Review Count", fill = "Status") +
    ggtitle("Reviewer Location and Employment Profile")
    



texts(pros_corpus[1]) # extract first DSI's text. 

kwic(pros_corpus, keywords = "401k", new = TRUE, window = 7) %>% View() #query a keyword, in context.
kwic(pros_corpus, keywords = "work life", new = TRUE) 
kwic(cons_corpus, keywords = "management", new = TRUE, window =5) %>% View()
textplot_xray(kwic(pros_corpus, keywords = "work life", new = TRUE))
textplot_xray(kwic(cons_corpus, keywords = "management", new = TRUE))


# Create THESAURUS to develop equivalence classes ##################################################

kwic(pros_corpus, keywords = "**", new = TRUE, window = 7) # %>% View() # query a keyword, in context.

# create PROs ECs after initial pre-processing like stopword removal, stemming.
nfa_thesaurus <- dictionary(
    list(work_life_balance = c("worklife", "balanc", "life", "work_lif", "work.lif", 
                             "life_balance","excellent_work.lif", "perfect_work.lif", 
                             "work.life.bal", "work.life_bal", "work-life_bal", "life_bal",
                             "life_fit"),
       # ideas, values, norms at the company that are generally accepted by employees.
       work_culture = c("work_cultur", "enthusiastic_workforc", "company_cultur", 
                        "office_cultur", "cultur", "atmospher", "family_feel", 
                        "great_cultur"),
       # Environment: anything that surrounds or environs us physically. 
       office_environment = c("work_environ", "nice_environ", "office_environ", "environ",
                              "welcoming_environ", "friendly_environ", "corporate_environ"),
       the_work = c("rewarding_work", "engaging_work", "forward_work", "workload", "work_load",
                    "typical_work", "light_work"),
       workweek = c("workweek","workweek","hr_workweek"),
       pay = c("pay", "great_pay", "good_pay", "competitive_pay", "higher_pay", "initial_pay",
               "compens", "good_compens", "competitive_salari", "excellent_compens", 
               "compet", "compensation_packag", "competitive_compens", "salari",
               "decent_salari", "X15K_increas", "X65k_rang", "market_wag", "wage"),
       coworkers = c("cowork", "co.work", "workforc", "most_peopl", "many_peopl", "people",
                     "everyon", "staff", "colleague", "peopl"),
       nice_coworkers = c("nice_co.work", "nice_peopl", "great_peopl", "wonderful_peopl",
                          "kind_peopl", "accessible_everyon", "earth_everyon"),
       college_graduate = c("college", "recent_college", "college_grad", "old_college",
                   "degre", "grad"),
       # updated
       young_people = c("young_people", "youthful_atmospher", "young_profession"),
       friend = c("friend", "buddi", "college_buddi"),
       employees = c("employe", "other_employe", "individual_employe", "junior_employe",
                     "new_employe", "employ"),
       job = c("job", "good_job", "time_job",  "great_job", "job_secur"),
       business_ops = c("busi", "business_oper", 'successful_busi', "different_oper"),
       industry_experience = c("industri", "futures_industri", "industry_experi",
                               "industry_background", "swaps_industri", "experi"),
       compliance_dept = c("complianc", "compliance_dept"),
       # updated
       audit_process = c("audit", "audit_process", "audit_program", "audit_program",
                         "fieldwork", "auditing_repetit", "better_audit", "audit_loc",
                         "basic_audit", "auditor.pool_arrang", "people_audit",
                         "examin", "seniors_examin", "examination_team"),
       field_supervisors = c("supervisor", "supervisor_hour", "supervisory_rol",
                             "field_supervisor", "experience_staff"),
       senior_management = c("manag", "management_team", "senior_manag", "director", 
                             "senior_posit", "senior_colleagu", "corporate_execut",
                             "levels_execut", "senior_level"),
       the_company = c("compani",  "company_lack", "small_compani"),
       great_company = c("great_plac", "great_work", "good_work", "great_compani",
                         "best_compani", "good_compani"),
       leadership = c("leadership", "great_leadership"),
       knowledge = c("knowledge", "knowledgeable_peopl"),
       learning_opportunity = c("learn", "opportun", "grow", "countless_opportun",
                                "multiple_opportun", "great_opportun", "exposur",
                                "excellent_exposur", "unique_perspect"),
       flex_day = c("flex", "flex_day", "flex_time", "flex_hour", "extra_day",
                     "X10_day", "X9day", "one_day", "X2_week", "flex_tim"),
       flexible_schedule = c("flexible", "flexible_schedul", "flexible_work", "work_schedul"),
       tuition_reimbursement = c("tuition", "tuition_reimburs", "educ"),
       training = c("train", "excellent_train", "in.depth_train", "training_opportun",
                    "formal_train", "extensive_train", "solid_train", "training_program"),
       professional_development = c("professional_develop", "develop", "professional_support",
                                    "professional_car", "profession"),
       career = c("career", "career_growth", "career_progress"),
       promotions = c("quick_advanc", "promot", "promotion_track","upward_mobl"),
       union_station = c("union_st", "union", "station", "station_"),
       location = c("locat", "good_loc"),
       # this is more of a concept
       travel_perks = c("travel", "travel_opportun", "great_travel", "travel_perk",
                        "extensive_travel", "hotel_point", "per_diem", "diem", "own_car",
                        "hotel"),
       good_benefits = c("benefit", "great_benefit", "benefits_packag", "good_benefit",
                            "excellent_benefit", "wonderful_benefit", "best_benefit",
                            "health_benefit", "more_benefit", "benefit_off",
                            "nice_benefit", "other_benefit", "decent_benefit",
                            "exceptional_benefit", "outstanding_benefit"),
       plan_401k = c("X401k_match", "X401k_contribut", "X401k_benefit", "X401k_plan",
                     "terrific_match"),
       health_insurance = c("health", "best_health","health_insur","health_benefit",
                            "insur"),
       personal_life= c("personal_affair", "other_commit", "commit", "outside_commit",
                        "personal_life"),
       workhours = c("hour", "X37.5_hour", "hour_week", "X35_hour", "X35_hrs", "weekly_hour", 
                     "X50_hour","supervisor_hour", "hours_increas", "X45.50_minut", "hrs", 
                     "time", "time_job"),
       vacation = c("vacat", "vacation_day"),
       # this is more of a concept to be mapped
       public_transportation = c("cta", "metra", "transport"),
       # this is more of a concept to be mapped
       company_events = c("company_out", "happy_hour", "ice_cream", "beer", "outing",
                          "cream_soci", "rooftop","rooftop_soci", "chicago_boat", "boat",
                          "quarterly_ev", "annual_depart", "depart_ev", "social",
                          "social_ev"),
       united_kingdom = c("london", "uk"),
       chicago = c("chicago", "downtown_chicago"),
       new_york = c("ny", "ny_office"),
       suburbs = c("burb", "suburban_guy"),
       regulation = c("regul", "government_regul")))



kwic(cons_corpus, keywords = "*downside*", new = TRUE, window = 7)
# build upon nfa_thesaurus
nfa_cons_thesaurus <- dictionary(
    list(work_life_balance = c("worklife", "balanc", "life", "work_lif", "work.lif", 
                               "life_balance","excellent_work.lif", "perfect_work.lif", 
                               "work.life.bal", "work.life_bal", "work-life_bal", "life_bal",
                               "life_fit"),
         # ideas, values, norms at the company that are generally accepted by employees.
         work_culture = c("work_cultur", "enthusiastic_workforc", "company_cultur", 
                          "office_cultur", "cultur", "atmospher", "family_feel", 
                          "great_cultur"),
         # Environment: anything that surrounds or environs us physically. 
         office_environment = c("work_environ", "nice_environ", "office_environ", "environ",
                                "welcoming_environ", "friendly_environ", "corporate_environ",
                                "office_spac", "offic"),
         #updated
         the_work = c("rewarding_work", "engaging_work", "forward_work", "typical_work", 
                      "light_work", "hard_work", "average_work", "inconsistent_work", "workflow", 
                      "unfinished_work", "staff_overwork", "work_opportun", "challenging_work",
                      "quality_work"),
         # new
         work_load = c("workload", "work_load"),
         workweek = c("workweek","workweek","hr_workweek"),
         pay = c("pay", "great_pay", "good_pay", "competitive_pay", "higher_pay", "initial_pay",
                 "compens", "good_compens", "excellent_compens", "compet", "compensation_packag", 
                 "competitive_compens", "X15K_increas", "X65k_rang", "market_wag", "wage",
                 "pay_low", "net_pay", "seem_compens"),
         # new
         salary = c("competitive_salari", "salari", "decent_salari", "low_salari", 
                    "salaries_low"),
         coworkers = c("cowork", "co.work", "workforc", "staff", "colleague", 
                       "worker"),
         # new
         people = c("most_peopl", "many_peopl", "peopl", "everyon"),
         nice_coworkers = c("nice_co.work", "nice_peopl", "great_peopl", "wonderful_peopl",
                            "kind_peopl", "accessible_everyon", "earth_everyon"),
         college = c("colleg", "recent_colleg", "college_grad", "old_colleg",
                              "degre", "grad"),
         # updated
         young_people = c("young_people", "youthful_atmospher", "young_profession",
                          "worker_ag", "young"),
         friend = c("friend", "buddi", "college_buddi"),
         employees = c("employe", "other_employe", "individual_employe", "junior_employe",
                       "new_employe", "employ"),
         # updated
         job = c("job", "good_job", "time_job",  "great_job", "job_secur", "other_job"),
         business_ops = c("busi", "business_oper", 'successful_busi', "different_oper"),
         industry_experience = c("industri", "futures_industri", "industry_experi",
                                 "industry_background", "swaps_industri"),
         compliance_dept = c("complianc", "compliance_dept"),
         # updated
         audit_process = c("audit", "audit_process", "audit_program", "audit_program",
                           "fieldwork", "auditing_repetit", "better_audit", "audit_loc",
                           "basic_audit", "auditor.pool_arrang", "people_audit",
                           "examin", "seniors_examin"),
         # updated
         field_supervisors = c("supervisor", "supervisor_hour", "supervisory_rol",
                               "field_supervisor","different_supervisor", "supervisor_level"),
         # new
         management = c("touch_manag", "poor_manag", "clueless_manag",
                             "lackluster_manag", "manag", "management_team", 
                        "several_manag", "other_manag","management_structur", 
                        "management_support"),
         # updated
         senior_management = c("senior_manag", "director", "senior_posit", "senior_colleagu", 
                               "corporate_execut","levels_execut", "senior_level",
                               "upper_manag", "certain_director", "decision.mak"),
         the_company = c("compani",  "company_lack", "small_compani", "organ"),
         great_company = c("great_plac", "great_work", "good_work", "great_compani",
                           "best_compani", "good_compani"),
         leadership = c("leadership", "great_leadership"),
         # updated & renamed
         expertise_and_skills = c("knowledge", "knowledgeable_peopl","expertis", "expert", 
                                 "knowledg", "financial_knowledg", "skill", "subject_matt"),
         learning_opportunity = c("learn", "opportun", "grow", "countless_opportun",
                                  "multiple_opportun", "great_opportun", "exposur",
                                  "excellent_exposur", "unique_perspect"),
         flex_day = c("flex", "flex_day", "flex_time", "flex_hour", "extra_day",
                      "X10_day", "X9day", "one_day", "X2_week", "flex_tim"),
         flexible_schedule = c("flexible", "flexible_schedul", "flexible_work", "work_schedul"),
         tuition_reimbursement = c("tuition", "tuition_reimburs", "educ"),
         # updated
         training = c("train", "excellent_train", "in.depth_train", "training_opportun",
                      "formal_train", "extensive_train", "solid_train", "training_program",
                      "poor_train", "classroom_train", "guidanc"),
         professional_development = c("professional_develop", "develop", "professional_support",
                                      "professional_car", "profession"),
         career = c("career", "career_growth", "career_progress"),
         # updated
         promotions = c("quick_advanc", "promot", "promotion_track","upward_mobl",
                        "promotion_process", "impact_promot", "promotions_random",
                        "upward_mov", "growth"),
         union_station = c("union_st", "union", "station", "station_"),
         location = c("locat", "good_loc"),
         # this is more of a concept
         travel_perks = c("travel_opportun", "great_travel", "travel_perk",
                          "hotel_point", "per_diem", "diem", "own_car","hotel"),
         # new
         travel = c("travel", "extensive_travel", "non_travel", "travel_depart",
                    "travel_polici"),
         good_benefits = c("benefit", "great_benefit", "benefits_packag", "good_benefit",
                           "excellent_benefit", "wonderful_benefit", "best_benefit",
                           "health_benefit", "more_benefit", "benefit_off",
                           "nice_benefit", "other_benefit", "decent_benefit",
                           "exceptional_benefit", "outstanding_benefit"),
         plan_401k = c("X401k_match", "X401k_contribut", "X401k_benefit", "X401k_plan",
                       "terrific_match"),
         health_insurance = c("health", "best_health","health_insur","health_benefit",
                              "insur"),
         personal_life= c("personal_affair", "other_commit", "commit", "outside_commit",
                          "personal_life"),
         workhours = c("hour", "X37.5_hour", "hour_week", "X35_hour", "X35_hrs", "weekly_hour", 
                       "X50_hour","supervisor_hour", "hours_increas", "X45.50_minut", "hrs", 
                       "time", "time_job"),
         vacation = c("vacat", "vacation_day"),
         # this is more of a concept to be mapped
         public_transportation = c("cta", "metra", "transport"),
         # this is more of a concept to be mapped
         company_events = c("company_out", "happy_hour", "ice_cream", "beer", "outing",
                            "cream_soci", "rooftop","rooftop_soci", "chicago_boat", "boat",
                            "quarterly_ev", "annual_depart", "depart_ev", "social",
                            "social_ev"),
         united_kingdom = c("london", "uk"),
         chicago = c("chicago", "downtown_chicago"),
         new_york = c("ny", "ny_office"),
         suburbs = c("burb", "suburban_guy"),
         regulation = c("regul", "government_regul"),
         # new
         turnover = c("high_attrit", "attrit", "attrition_r", "turnov", "recent_turnov",
                      "supervisor_turnov", "worker_turnov"),
         # new
         bonus = c("bonus", "real_bonus", "bonus_structur"),
         # new
         technology = c("it_depart", "technolog", "tech", "technical_background",
                        "technical_debt", "technological_expertis", "information_system",
                        "code", "code_chang", "developed_softwar", "softwar"),
         # new
         experience = c("experi", "experienced_staff", "outside_experi", "experienced_work",
                               "strong_background", "background", "experience_staff"),
         #new
         pfg_incident = c("pfg", "pfg_incid", "whole_peregrin", "peregrin",
                                 "peregrine_financi"),
         new_staff = c("new_staff", "new_employe", "new_hir"),
         fresh_ideas = c("new_th", "idea", "good_idea", "fresh_idea", "progress"),
         status_quo = c("status_quo", "facad", "peaceful_facad"),
         cliques = c("same_colleg", "college_circl", "cliquish_cultur", "friend"),
         feedback = c("feedback", "feedback_beg", "evalu", "coach"),
         office_politics = c("polit","office_polit", "politics_impact", "political_environ"),
         favoritism = c("management_fav", "favorite_people", "fave", "favorit"),
         merit = c("meritocraci", "merit.based_system", "merit"),
         years = c("year", "X10_year", "few_year"),
         nothing = c("noth"),
         corporate_card = c("card", "corporate_card"),
         exam_teams = c("group_work", "group", "team", "team_memb", "examination_team")
         ))


# Create Stopwords dictionary vector ##############################################################

my_stopwords <- c(stopwords("english"), "NFA", "nfa", "national", "futures", "association", 
                  "work", "day")


# (IGNORE) PROs: Unigram Extraction #######################################################################

# tokenized document words as unigrams, with more cleaning.
pros_unigrams <- tokenize(pros_corpus, what = "word",
                          removePunct = TRUE, 
                          removeNumbers = TRUE,
                          removeSymbols = TRUE) %>%
  removeFeatures(my_stopwords) 
class(pros_unigrams)
pros_unigrams


# PROs: phrasematch() Extraction ########################################################################

# pros phrases. Per POS tagging results, everything is mostly singular or mass noun. 
pros_phrases <- phrasemachine(texts(pros_corpus),
                              maximum_ngram_length = 2,
                              minimum_ngram_length = 1,
                              return_phrase_vectors = FALSE,
                              return_tag_sequences = FALSE)
pros_phrases
class(pros_phrases)
save(pros_phrases, file="pros_phrases.Rdata") # save phrasematch output. 
#load(file = "pros_phrases.Rdata")

# assign pros_phrases to the empty tokens list. 
pros_corpus$tokens <- pros_phrases


# PROs: Create Document Feature Matrix (NO Equivalence Classes yet) ###################################

# clean the terms, stopwords, stemming, create Document Feature Matrix.
pros_dfm <- dfm(pros_corpus$tokens, 
                 tolower = TRUE,
                 removePunct = TRUE, 
                 removeNumbers = TRUE,
                 removeSymbols = TRUE,
                 remove = my_stopwords,
                 stem = TRUE) %>%
    dfm_sort(decreasing = TRUE, margin = "both")

# analyze the dfm
head(pros_dfm, n = 20, nfeature = 10)
pros_dfm[,1:50] %>% View() # view entire feature matrix
topfeatures(pros_dfm, n = 50)
ndoc(pros_dfm)
nfeature(pros_dfm)
sparsity(pros_dfm)
textstat_lexdiv(x = pros_dfm, measure = c("TTR", "CTTR")) # TTR = Type / Token Ratio
featnames(pros_dfm) # names of all features. 
dim(pros_dfm)

# analyze the fraction of DSI's in which a term appears.
pros_dfm %>% 
    data.frame() %>%
    summarise_all(function(x) sum(x!=0))

# analyze the total number of terms in each document. May want to drop DSI's with few terms.
pros_dfm%>% 
    data.frame() %>%
    rowSums()

# colocations of terms
Pros_collocations <- collocations(pros_corpus$tokens)
Pros_collocations2 <- removeFeatures(collocations(pros_corpus, size = 2), stopwords("english"))

# used to analyze/develop Equivalent Classes.
data.frame(pros_dfm) %>%
    rownames_to_column("document") %>% #tibble package. 
    write_csv("sorted PROs DFM.csv")

# PROs DFM (no Equivalence Classes yet): Term Frequency * Inverse Document Frequency (tf-idf)
pros_tfidf <- tfidf(pros_dfm, normalize = TRUE, scheme = "inverse")
class(pros_tfidf)

topfeatures(pros_tfidf, n = 50)
head(pros_tfidf, n = 20, nfeature = 10)
pros_tfidf[,1:50] %>% View() # view entire feature matrix
nfeature(pros_tfidf)
sparsity(pros_tfidf)
featnames(pros_tfidf)

# cosine similarity
pros_simil <- textstat_simil(pros_tfidf, margin = "documents", method = "cosine")
dotchart(as.list(pros_simil)$"text23", xlab = "Cosine similarity")

# alternative method that removes features occurring only in 1 DSI then computes tf-idf.
pros_tfidf_trim <-dfm_trim(pros_dfm, min_docfreq = 2) %>%
    tfidf(normalize = TRUE, scheme = "inverse")
topfeatures(pros_tfidf_trim, n = 50)
head(pros_tfidf_trim, n = 20, nfeature = 10)
pros_tfidf_trim[,1:50] %>% View() # view entire feature matrix
nfeature(pros_tfidf_trim)
sparsity(pros_tfidf_trim)
featnames(pros_tfidf_trim)

# cosine similarity
pros_tfidf_simil <- textstat_simil(pros_tfidf_trim, margin = "documents", method = "cosine")
dotchart(as.list(pros_tfidf_simil)$"text67", xlab = "Cosine similarity")


# Explore the Pros DFM with WordClouds ############################################################

textplot_wordcloud(pros_dfm)
if (require(RColorBrewer))
  textplot_wordcloud(pros_dfm, max.words = 100, 
                     colors = brewer.pal(6, "Dark2"), 
                     scale = c(5, 0.5))

summary(pros_corpus) %>% 
  select(Text, Types, Tokens, Sentences) %>% 
  arrange(desc(Sentences), desc(Tokens)) 

# top 4 texts in most diverse term types.
png(filename = "Pros 5 Most Lexically Diverse DSIs.png", width = 525, height =480)
if (require(RColorBrewer))
textplot_wordcloud(pros_dfm[c("text16", "text29","text17", "text64","text37"),], 
                   max.words = 75,
                   colors = brewer.pal(5, "Dark2"), 
                   comparison = TRUE)
dev.off()

# plot the NY vs. CHI most popular bigrams.
pros_ny_vs_chi_bigrams <- corpus_subset(pros_corpus, rvwr_loc != "Unknown") %>%
  dfm(groups = "rvwr_loc",
      tolower = TRUE,
      removePunct = TRUE, 
      removeNumbers = TRUE,
      removeSymbols = TRUE,
      remove = my_stopwords,
      stem = TRUE,
      ngrams = 2)

pros_ny_vs_chi_bigrams # low sparsity.
pros_ny_vs_chi_bigrams[,1:50] %>% View()

png(filename = "NFA Chi vs. NY popular Pros bigrams.png", width = 550, height =480)
textplot_wordcloud(pros_ny_vs_chi_bigrams, 
                   max.words = 50, 
                   colors = brewer.pal(6, "Dark2"), 
                   comparison = TRUE)
dev.off()


# PROs: Create DFM for topic modeling #########################################################

pros_dfm_ec <- dfm(pros_corpus$tokens, 
                tolower = TRUE,
                removePunct = TRUE, 
                removeNumbers = TRUE,
                removeSymbols = TRUE,
                remove = my_stopwords,
                stem = TRUE) %>% 
            dfm_lookup(., dictionary = nfa_thesaurus, exclusive = FALSE) %>%# ECs will map my thesaurus after applying other cleaning funcs. ECs will be ALL_CAPs
            dfm_trim(min_docfreq = 3) # remove infrequent terms. used instead of tf-idf. Not sure how to subset?

# analyze the total number of terms in each document. May want to drop DSI's with few terms.
pros_dfm_ec%>% 
    data.frame() %>%
    rowSums() %>% 
    sort()

# dropped DSI without any terms left.
pros_dfm_ec <- pros_dfm_ec[-c(23,5,26,48),] 
dim(pros_dfm_ec)

pros_dfm_ec %>%
    data.frame() %>%
    write_csv("pros_dfm_ec.csv")
  
# analyze the dfm
head(pros_dfm_ec, n = 20, nfeature = 10)
pros_dfm_ec %>% View() # view entire feature matrix
topfeatures(pros_dfm_ec, n = 50)
nfeature(pros_dfm_ec)
sparsity(pros_dfm_ec)
dim(pros_dfm_ec)
textstat_lexdiv(x = pros_dfm_ec, measure = c("TTR", "CTTR")) # TTR = Type / Token Ratio
featnames(pros_dfm_ec) # names of all features. 


# Pros - Topic Modeling. Should use raw frequencies only. #############################################
?`LDAcontrol-class`
SEED = 1988 

"LDA has two hyperparameters, tuning them changes assumptions about the prior probability
assumptions of the topics. a low alpha value places more weight on having each document 
composed of only a few dominant topics (whereas a high value will return many more 
relatively dominant topics). Similarly, a low beta value places more weight on having each
topic composed of only a few dominant words." # http://bit.ly/2lb0P7L

# To assess an estimated alpha versus a fixed alpha. 
pros_TM <-list(VEM = LDA(pros_dfm_ec, k = 8, control = list(seed = SEED)),
              VEM_fixed = LDA(pros_dfm_ec, k = 8,
                              control = list(estimate.alpha = FALSE, seed = SEED)),
              Gibbs = LDA(pros_dfm_ec, k = 8, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
              CTM = CTM(pros_dfm_ec, k = 8,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))
# estimated alpha is low vs. non-estimate of 50/k topics = 10. which makes sense. My DSIs are pretty short. 
sapply(pros_TM[1:3], slot, "alpha") 

pros_LDA2_vem <- convert(x = pros_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 2, method = "VEM", control = list(seed = SEED))
get_terms(pros_LDA2_vem,7)
topics(pros_LDA2_vem, 3)

pros_LDA2_gibbs <- convert(x = pros_dfm_ec, to = "topicmodels") %>%
    LDA(k = 2, method = "Gibbs", control = list(seed = SEED))
get_terms(pros_LDA2_gibbs,7)
topics(pros_LDA2_gibbs, 3)

pros_LDA3 <- convert(x = pros_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 3,  method = "VEM", control = list(seed = SEED))
get_terms(pros_LDA3,5)
topics(pros_LDA3, 3)

pros_LDA4 <- convert(x = pros_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 4, method = "VEM", control = list(seed = SEED))
get_terms(pros_LDA4,5)
topics(pros_LDA4, 4)

pros_LDA5 <- convert(x = pros_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 5, method = "Gibbs", control = list(seed = SEED))
get_terms(pros_LDA5,5)
topics(pros_LDA5, 5)

pros_LDA6 <- convert(x = pros_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 6, method = "Gibbs", control = list(seed = SEED))
get_terms(pros_LDA6,5)
topics(pros_LDA6, 5)

# This one is interesting.
pros_LDA8 <- convert(x = pros_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 8, method = "VEM", control = list(seed = SEED))
get_terms(pros_LDA8,3)
attributes(pros_LDA8)


# Pros - Analysis of 8 Topic LDA model (tidytext book) ################################################

#tidy df. one-topic-per-term-per-row format.
pros_LDA8_betas <- tidy(pros_LDA8)

# betas are per-topic per-word probabilities. for each row, this is the computed probability of 
#   the term being generated from that topic. 

LDA8_top_betas <- pros_LDA8_betas %>%
    mutate(topic = factor(topic, 
                          labels = c("Topic 1: Office Environment", 
                                     "Topic 2: Company Events & Culture", 
                                     "Topic 3: Learning & Development", 
                                     "Topic 4: Good Benefits", 
                                     "Topic 5: Pay",
                                     "Topic 6: Hours & Flex Time", 
                                     "Topic 7: Auditor Travel Perks", 
                                     "Topic 8: Work-Life Balance"))) %>%
    group_by(topic) %>%
    top_n(n = 5, wt = beta) %>% 
    ungroup() %>%
    arrange(topic, desc(beta))

# What are the most probabilistic terms generated by Topic?
LDA8_top_betas %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(x = term, y = beta, fill = topic)) +
        geom_bar(stat = "identity", show.legend = "FALSE") +
        facet_wrap(~ topic, scales = "free", nrow = 4, ncol = 2) +
        ggtitle("NFA Pros Reviews", subtitle = "Prevalent Terms by Topics") +
        theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
        theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
        theme(axis.title.y = element_text(hjust = 0.4, vjust = 0)) +
        ylab("Probability of Term Association with Topic") +
        xlab(NULL) +
        coord_flip() +
    ggsave(filename = "Pros LDA8 Prevalent Terms by Topic.png")
        
# OFFICE_ENVIRONMENT & WORK_CULTURE are amongst driving terms in Topics 1 and 2. 
#   These topics seem a bit fuzzy. Which terms are most different on? May help think about 
#   their topic labels.

pros_LDA8_betas %>%
    filter(topic %in% c(1,2)) %>%
    mutate(topic = factor(topic, 
                          labels = c("Topic_1", "Topic_2"))) %>%
    group_by(topic) %>%
    summarise(min = min(beta),
              percentile_25 = quantile(beta, .25),
              mean = mean(beta),
              percentile_75 = quantile(beta, .75))
    #ggplot(aes(x = topic, y = beta)) + geom_boxplot()

pros_topic1v2 <- pros_LDA8_betas %>%
    filter(topic %in% c(1,2)) %>%
    mutate(topic = factor(topic, labels = c("topic1", "topic2"))) %>%
    spread(key = topic, value = beta) %>%
    filter(topic1 > 0.01639344 | topic2 > 0.01639344) %>% 
    mutate(log_ratio = log10(topic2 / topic1),
           difference_ind = ifelse(log_ratio > 0, "More Common in Topic 2", 
                                   "More Common in Topic 1")) 
pros_topic1v2 %>%
    ggplot(aes(reorder(term, log_ratio), log_ratio, fill = difference_ind)) +
        geom_bar(stat = "identity") +
        ggtitle("Topic 1 vs. Topic 2", subtitle = "Term Differentiation between Topics") +
        theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
        theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
        xlab("Term") +
        ylab("Log10 Ratio of Topic 2 / Topic 1") +
        coord_flip() +
    ggsave(filename = "Pros Topic 1 vs. 2 Term Differences.png")
  
# Gamma PROs topic distributions combined with corpus metadata. 
pros_corpus_subset <- corpus_subset(pros_corpus, 
                                    !(rownames(pros_corpus$documents) %in% 
                                          c("text23", "text5", "text26", "text48")))


pros_LDA8_gamma <- attributes(pros_LDA8)$gamma %>%
    data.frame() %>%
    mutate(document = pros_dfm_ec %>% rownames()) %>%
    rename(topic_1 = X1, topic_2 = X2, topic_3 = X3, topic_4 = X4,
           topic_5 = X5, topic_6 = X6, topic_7 = X7, topic_8 = X8) %>%
    cbind(summary(pros_corpus_subset)) %>%
    arrange(desc(Tokens))

pros_corpus[16] # Largest PROs review
wordcloud(pros_corpus[16] , min.freq = 1, max.words = 50, random.order = FALSE)

pros_corpus[17] # 2nd largest PROs review
pros_corpus[64] # 3rd largest PROs review
pros_corpus[37] # 5th largest PROs review

textplot_wordcloud(pros_dfm[c("text16", "text17","text37", "text64"),], 
                   max.words = 120,
                   colors = brewer.pal(5, "Dark2"), 
                   comparison = TRUE)


# PROs - Clustering  #####################################################################################

# hierarchical clustering - get distance matrix on normalized dfm
cluster_pros <- as.matrix(pros_tfidf, "relFreq") %>% 
  dist() %>%
  hclust()  
cluster_pros$labels
# plot as a dendrogram
plot(cluster_pros, xlab = "", sub = "", main = "Euclidean Distance on Normalized Pros Term Frequency")


# CONs: phrasematch() Extraction  + Document Term Matrix + Explore ###############################
class(summary(cons_corpus)) # data.frame
summary(cons_corpus) %>% 
  select(Text, Types, Tokens, Sentences) %>% 
  arrange(Sentences, Tokens)

# Cons phrases. Had errors generating as sparse DSIs were throwing errors.. 
# cons_phrases <- phrasemachine(texts(cons_corpus),
#                              maximum_ngram_length = 2,
#                              minimum_ngram_length = 1,
#                              return_phrase_vectors = FALSE,
#                              return_tag_sequences = FALSE)

# initiate vector to store extracted Cons phrases.
cons_phrases <- vector("character", length(texts(cons_corpus)))

# My favorite piece of code in script!
#   due to issues with phrasemachine on very small DSIs (e.g. text62), conditional phrase extaction.
#   if wordcount < 8, extract bi-skipgrams that aren't stopwords into a single string.
#   else if # of words >8, apply the phrasemachine() engine.
for(i in 1:length(texts(cons_corpus))) {
  # stringi to count words in each Cons DSI.
  if (stri_count(texts(cons_corpus[i]),regex="\\S+") < 8) { 
    cons_phrases[i]  <- tokenize(cons_corpus[i], what = "word",
                                 removePunct = TRUE, 
                                 removeSymbols = FALSE,
                                 removeSeparators = TRUE,
                                 ngrams = 1) %>% # get unigrams
                        removeFeatures(my_stopwords) %>%
                        tokens_ngrams(n = 2, skip = 1) %>% # derive bi-skipgrams.
                        .[[1]] %>% # convert from list to char vector
                        paste(collapse = " ") # convert to single string.
  } else {
    cons_phrases[i]  <- phrasemachine(texts(cons_corpus[i]),
                                      maximum_ngram_length = 2,
                                      minimum_ngram_length = 1,
                                      return_phrase_vectors = FALSE,
                                      return_tag_sequences = FALSE)
  }
}
cons_phrases
class(cons_phrases)
save(cons_phrases, file="cons_phrases.Rdata") # save phrasematch output. 
#load(file = "cons_phrases.Rdata")

# assign cons_phrases to the originally empty tokens list. 
cons_corpus$tokens <- cons_phrases

# clean the terms, stopwords, stemming, create Document Feature Matrix.
cons_dfm <- dfm(cons_corpus$tokens, 
                tolower = TRUE,
                removePunct = TRUE, 
                removeNumbers = TRUE,
                removeSymbols = TRUE,
                remove = my_stopwords,
                stem = TRUE) %>%
  dfm_sort(decreasing = TRUE, margin = "both")

cons_dfm[,1:50] %>% View()
topfeatures(cons_dfm, n = 50)
summary(cons_dfm)
ndoc(cons_dfm)
nfeature(cons_dfm)
sparsity(cons_dfm)
textstat_lexdiv(x = cons_dfm, measure = c("TTR", "CTTR")) # TTR = Type / Token Ratio
featnames(cons_dfm) # names of all features. 

# Tools for starting to explore ECs for Cons Corpus tokens.

# the number of DSI's in which a term appears.
cons_dfm %>% 
    data.frame() %>%
    summarise_all(function(x) sum(x!=0))

# analyze the total number of terms in each document. May want to drop DSI's with few terms.
cons_dfm %>% 
    data.frame() %>%
    rowSums()

# colocations of terms
cons_collocations1 <- collocations(cons_corpus$tokens)
cons_collocations2 <- removeFeatures(collocations(cons_corpus, size = 2), stopwords("english"))

# used to analyze/develop Equivalent Classes.
dfm_sort(cons_dfm, decreasing = TRUE, margin = "both") %>% 
    data.frame() %>%
    rownames_to_column("document") %>% #tibble package. 
    write_csv("sorted CONs DFM.csv")


# Explore the CONs DFM with WordClouds ############################################################

textplot_wordcloud(cons_dfm)
if (require(RColorBrewer))
    textplot_wordcloud(cons_dfm, max.words = 100, 
                       colors = brewer.pal(6, "Dark2"), 
                       scale = c(5, 0.5))

summary(cons_corpus) %>% 
    select(Text, Types, Tokens, Sentences) %>% 
    arrange(desc(Sentences), desc(Tokens)) 

# top 4 texts in most diverse term types.
if (require(RColorBrewer))
    textplot_wordcloud(cons_dfm[c("text23", "text37","text47", "text55","text29"),], 
                       max.words = 75,
                       colors = brewer.pal(5, "Dark2"), 
                       comparison = TRUE)

# plot the NY vs. CHI most popular bigrams.
cons_ny_vs_chi_bigrams <- corpus_subset(cons_corpus, rvwr_loc != "Unknown") %>%
    dfm(groups = "rvwr_loc",
        tolower = TRUE,
        removePunct = TRUE, 
        removeNumbers = TRUE,
        removeSymbols = TRUE,
        remove = my_stopwords,
        ngrams = 2)

textplot_wordcloud(cons_ny_vs_chi_bigrams, 
                   max.words = 100,
                   colors = brewer.pal(6, "Dark2"), 
                   comparison = TRUE)


# CONs: Create DFM for topic modeling #########################################################

cons_dfm_ec <- dfm(cons_corpus$tokens, 
                   tolower = TRUE,
                   removePunct = TRUE, 
                   removeNumbers = TRUE,
                   removeSymbols = TRUE,
                   remove = c(my_stopwords, 'people'),
                   stem = TRUE) %>% 
    dfm_lookup(., dictionary = nfa_cons_thesaurus, exclusive = FALSE) %>%# ECs will map my thesaurus after applying other cleaning funcs. ECs will be ALL_CAPs
    dfm_remove(., c("COWORKERS", "THE_COMPANY")) %>%
    dfm_trim(min_docfreq = 3) # remove infrequent terms. used instead of tf-idf. Not sure how to subset?

# analyze the total number of terms in each document. May want to drop DSI's with few terms.
cons_dfm_ec%>% 
    data.frame() %>%
    rowSums() %>% 
    sort() # text 20, 32, 45, 56

# only run this in conjuction with re-running cons_dfm_ec above.
cons_dfm_ec <- cons_dfm_ec[-c(20,32, 45, 56,1,5,9,10,24,35,48,53,62,65),] # dropped DSI without any terms left.
dim(cons_dfm_ec)

cons_dfm_ec %>%
    data.frame() %>%
    rownames_to_column("document") %>% #tibble package. 
    write_csv("cons_dfm_ec.csv")

# analyze the dfm
head(cons_dfm_ec, n = 20, nfeature = 10)
cons_dfm_ec %>% View() # view entire feature matrix
topfeatures(cons_dfm_ec, n = 50)
nfeature(cons_dfm_ec)
sparsity(cons_dfm_ec)
dim(cons_dfm_ec)
textstat_lexdiv(x = cons_dfm_ec, measure = c("TTR", "CTTR")) # TTR = Type / Token Ratio
featnames(cons_dfm_ec) # names of all features. 


# CONs - Topic Modeling (First Pass). Should use raw frequencies only. #############################################
?`LDAcontrol-class`
SEED = 1988 

"LDA has two hyperparameters, tuning them changes assumptions about the prior probability
assumptions of the topics. a low alpha value places more weight on having each document 
composed of only a few dominant topics (whereas a high value will return many more 
relatively dominant topics). Similarly, a low beta value places more weight on having each
topic composed of only a few dominant words." # http://bit.ly/2lb0P7L

cons_LDA2 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 2, method = "VEM", control = list(seed = SEED))
get_terms(cons_LDA2,5)

# interesting one. 
cons_LDA3 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 3, method = "VEM", control = list(seed = SEED))
get_terms(cons_LDA3,5)
topics(cons_LDA3, 3)

# interesting one. 
cons_LDA4 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 4, method = "VEM", control = list(seed = SEED))
get_terms(cons_LDA4,5)

cons_LDA5 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 5, method = "Gibbs", control = list(seed = SEED))
get_terms(cons_LDA5,5)

# interesting one
cons_LDA6 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 6, method = "Gibbs", control = list(seed = SEED))
get_terms(cons_LDA6,3)

cons_LDA7 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 7, method = "Gibbs", control = list(seed = SEED))
get_terms(cons_LDA7,5)

# interesting one.
cons_LDA8 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 8, method = "Gibbs", control = list(seed = SEED))
get_terms(cons_LDA8,4)

# interesting one. 
cons_LDA9<- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 9, method = "VEM", control = list(seed = SEED))
get_terms(cons_LDA9,2)

cons_LDA10 <- convert(x = cons_dfm_ec, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 10, method = "VEM", control = list(seed = SEED))
get_terms(cons_LDA10,2)


# CONs - Analysis of Topic models (tidytext book) ################################################

#tidy df. one-topic-per-term-per-row format.
cons_LDA4_betas <- tidy(cons_LDA4)

# betas are per-topic per-word probabilities. for each row, this is the computed probability of 
#   the term being generated from that topic. 

cons_LDA4_top_betas <- cons_LDA4_betas %>%
    mutate(topic = factor(topic, 
                          labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4"))) %>%
    group_by(topic) %>%
    top_n(n = 10, wt = beta) %>% 
    ungroup() %>%
    arrange(topic, desc(beta))

# What are the most probabilistic terms generated by Topic?
cons_LDA4_top_betas %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(x = term, y = beta, fill = topic)) +
    geom_bar(stat = "identity", show.legend = "FALSE") +
    facet_wrap(~ topic, scales = "free", nrow = 2, ncol = 2) +
    ggtitle("NFA Cons Reviews", subtitle = "Prevalent Terms by 4 Topics") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    theme(axis.title.y = element_text(hjust = 0.4, vjust = 0)) +
    ylab("Probability of Term Association with Topic") +
    xlab(NULL) +
    coord_flip() +
    ggsave(filename = "Cons LDA4 Prevalent Terms by Topic.png")


# 9-Topic Cons Model
#tidy df. one-topic-per-term-per-row format.
cons_LDA7_betas <- tidy(cons_LDA7)

# betas are per-topic per-word probabilities. for each row, this is the computed probability of 
#   the term being generated from that topic. 

cons_LDA7_top_betas <- cons_LDA7_betas %>%
    mutate(topic = factor(topic, 
                          labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic5",
                                     "Topic 6", "Topic 7"))) %>%
    group_by(topic) %>%
    top_n(n = 5, wt = beta) %>% 
    ungroup() %>%
    arrange(topic, desc(beta))

# What are the most probabilistic terms generated by Topic?
cons_LDA7_top_betas %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(x = term, y = beta, fill = topic)) +
    geom_bar(stat = "identity", show.legend = "FALSE") +
    facet_wrap(~ topic, scales = "free", nrow = 4, ncol = 2) +
    ggtitle("NFA Cons Reviews", subtitle = "Prevalent Terms by 7 Topics") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    theme(axis.title.y = element_text(hjust = 0.4, vjust = 0)) +
    ylab("Probability of Term Association with Topic") +
    xlab(NULL) +
    coord_flip() +
    ggsave(filename = "Cons LDA7 Prevalent Terms by Topic.png")


# CONs: Subset Topic Models: Current vs. Former Employees ######################################

cons_token_summary <- summary(cons_corpus)  %>% 
    arrange(Tokens) %>%
    group_by(rvwr_status) %>%
    summarise(rvw_count = n(),
              min = min(Tokens),
              percentile25 = quantile(Tokens, probs = 0.25),
              median = median(Tokens),
              mean = mean(Tokens),
              percentile75 = quantile(Tokens, probs = 0.75),
              max = max(Tokens),
              stdev = sd(Tokens))
cons_token_summary

summary(cons_corpus) %>% 
    select(Text, Types, Tokens, Sentences, rvwr_status) %>% 
    ggplot(aes(y = Tokens, x = rvwr_status)) + geom_boxplot()


# 1. Subset corpora for current vs. former based on cons_corpus

cons_frmr_corpus <- corpus_subset(cons_corpus, rvwr_status == "Former Employee")
cons_curr_corpus <- corpus_subset(cons_corpus, rvwr_status == "Current Employee")
summary(cons_frmr_corpus)
summary(cons_curr_corpus)

cons_frmr_corpus$tokens <- NULL
cons_curr_corpus$tokens <- NULL

# 2. CONs Former Employees: Extract Term tokens, Create DFM, fit CONs frmr employee Topic Models.

# Extract Terms for Former Employees - CONs corpus.
cons_frmr_phrases <- vector("character", length(texts(cons_frmr_corpus)))
#   if wordcount < 16 (25th percentile), extract unigrams that aren't stopwords into a single string.
#   else if # of words >= 16, apply the phrasemachine() engine.
for(i in 1:length(texts(cons_frmr_corpus))) {
    # stringi to count words in each Cons DSI.
    if (stri_count(texts(cons_frmr_corpus[i]),regex="\\S+") < 16) { 
        cons_frmr_phrases[i]  <- tokens(cons_frmr_corpus[i], what = "word",
                                    removePunct = TRUE, 
                                    removeSymbols = FALSE,
                                    removeSeparators = TRUE,
                                    ngrams = 1) %>% # get unigrams
            removeFeatures(my_stopwords) %>%
            .[[1]] %>% # convert from list to char vector
            paste(collapse = " ") # convert to single string.
    } else {
        cons_frmr_phrases[i]  <- phrasemachine(texts(cons_frmr_corpus[i]),
                                           maximum_ngram_length = 2,
                                           minimum_ngram_length = 1,
                                           return_phrase_vectors = FALSE,
                                           return_tag_sequences = FALSE)
    }
}
cons_frmr_phrases
save(cons_frmr_phrases, file="cons_frmr_employee_phrases.Rdata") # save phrasematch output. 
cons_frmr_corpus$tokens <- cons_frmr_phrases

# Cons former employees DFM. 
cons_dfm_frmr <- dfm(cons_frmr_corpus$tokens, 
                     tolower = TRUE,
                     removePunct = TRUE, 
                     removeNumbers = TRUE,
                     removeSymbols = TRUE,
                     remove = c(my_stopwords, 'people'),
                     stem = TRUE) %>% 
    dfm_lookup(., dictionary = nfa_cons_thesaurus, exclusive = FALSE) %>%# ECs will map my thesaurus after applying other cleaning funcs. ECs will be ALL_CAPs
    #dfm_remove(., c("COWORKERS", "THE_COMPANY")) %>%
    dfm_trim(min_docfreq = 2) # remove infrequent terms. used instead of tf-idf. Not sure how to subset?

sparsity(cons_dfm_frmr)
dim(cons_dfm_frmr)
topfeatures(cons_dfm_frmr, n =25)
cons_dfm_frmr%>% 
    data.frame() %>%
    rowSums() %>% 
    sort() 

# Cons Former Employee Topic Models. 
?`LDAcontrol-class`
SEED = 1988 
cons_frmr_LDA3 <- convert(x = cons_dfm_frmr, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 3, method = "Gibbs", control = list(seed = SEED))
get_terms(cons_frmr_LDA3,5)

# best one. 
cons_frmr_LDA4 <- convert(x = cons_dfm_frmr, to = "topicmodels") %>% # convert DFM to format for other text pkg.
    LDA(k = 4, method = "Gibbs", control = list(seed = SEED))
get_terms(cons_frmr_LDA4,5)



# What are the most probabilistic terms generated by Topic for cons_frmr_LDA4?
#tidy df. one-topic-per-term-per-row format. From TidyText book.
attributes(cons_frmr_LDA4)
cons_frmr_LDA4_betas <- tidy(cons_frmr_LDA4)
# betas: for each row, the probability of the term being generated from that topic. 
cons_frmr_LDA4_top_betas <- cons_frmr_LDA4_betas %>%
    mutate(topic = factor(topic, 
                          labels = c("Topic 1: Managing People", 
                                     "Topic 2: Promotions & Favoritism",
                                     "Topic 3: Senior Management", 
                                     "Topic 4: Audit Process & Skills"))) %>%
    group_by(topic) %>%
    top_n(n = 5, wt = beta) %>% 
    ungroup() %>%
    arrange(topic, desc(beta))

cons_frmr_LDA4_top_betas %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(x = term, y = beta, fill = topic)) +
    geom_bar(stat = "identity", show.legend = "FALSE") +
    facet_wrap(~ topic, scales = "free", nrow = 2, ncol = 2) +
    ggtitle("NFA Cons Former Employee Reviews", subtitle = "Prevalent Terms by 4 Topics") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    theme(axis.title.y = element_text(hjust = 0.4, vjust = 0)) +
    ylab("Probability of Term Association with Topic") +
    xlab(NULL) +
    coord_flip() +
    ggsave(filename = "CONs Former Employees Terms by Topic.png")


# Analyze Document Topic Probabilities for Cons Reviews by Former Employees.
tidy(cons_frmr_LDA4, matrix = "gamma") %>%
    group_by(document) %>%
    filter(gamma == max(gamma)) %>%
    ungroup() %>%
    ggplot(aes(y = gamma, x = factor(topic))) + geom_boxplot()

# Gamma topic distributions combined with corpus metadata. 
cons_frmr_LDA4_gamma <- attributes(cons_frmr_LDA4)$gamma %>% 
    data.frame() %>%
    rename(topic_1 = X1, topic_2 = X2, topic_3 = X3, topic_4 = X4) %>%
    cbind(summary(cons_frmr_corpus))
cons_frmr_LDA4_gamma %>% View()
cons_frmr_corpus[9] # review text 37. Topic 2.
cons_frmr_corpus[8] # review text 36. Topic 3. 
cons_frmr_corpus[24] # review text 64. Topic 4.
cons_frmr_corpus[16] # review text 49. Topic 1. 

attributes(cons_frmr_corpus)

# 3. CONs Former Employees: Extract Term tokens, Create DFM, fit CONs current
#   employee Topic Models.

# Extract Terms for Current Employees - CONs corpus.
cons_curr_phrases <- vector("character", length(texts(cons_curr_corpus)))
#   if wordcount < 18 (Median), extract unigrams that aren't stopwords into a single string.
#   else if # of words >= 18, apply the phrasemachine() engine.
for(i in 1:length(texts(cons_curr_corpus))) {
    # stringi to count words in each Cons DSI.
    if (stri_count(texts(cons_curr_corpus[i]),regex="\\S+") < 18) { 
        cons_curr_phrases[i]  <- tokens(cons_curr_corpus[i], what = "word",
                                        removePunct = TRUE, 
                                        removeSymbols = FALSE,
                                        removeSeparators = TRUE,
                                        ngrams = 1) %>% # get unigrams
            removeFeatures(my_stopwords) %>%
            .[[1]] %>% # convert from list to char vector
            paste(collapse = " ") # convert to single string.
    } else {
        cons_curr_phrases[i]  <- phrasemachine(texts(cons_curr_corpus[i]),
                                               maximum_ngram_length = 2,
                                               minimum_ngram_length = 1,
                                               return_phrase_vectors = FALSE,
                                               return_tag_sequences = FALSE)
    }
}
cons_curr_phrases
save(cons_curr_phrases, file="cons_curr_employee_phrases.Rdata") # save phrasematch output. 

cons_curr_corpus$tokens <- cons_curr_phrases

# Cons current employees DFM. 
cons_dfm_current <- dfm(cons_curr_corpus$tokens, 
                     tolower = TRUE,
                     removePunct = TRUE, 
                     removeNumbers = TRUE,
                     removeSymbols = TRUE,
                     remove = c(my_stopwords, 'people'),
                     stem = TRUE) %>% 
    dfm_lookup(., dictionary = nfa_cons_thesaurus, exclusive = FALSE)

sparsity(cons_dfm_current)
dim(cons_dfm_current)
topfeatures(cons_dfm_current, n =25)
cons_dfm_current%>% 
    data.frame() %>%
    rowSums() %>% 
    sort() 

# Cons Current Employee Topic Models. 
?`LDAcontrol-class`
SEED = 1988 
cons_current_LDA2 <- convert(x = cons_dfm_current, to = "topicmodels") %>%
    LDA(k = 2, method = "VEM", control = list(seed = SEED))
get_terms(cons_current_LDA2, 10)

cons_current_LDA3 <- convert(x = cons_dfm_current, to = "topicmodels") %>%
    LDA(k = 3, method = "VEM", control = list(seed = SEED)) 
get_terms(cons_current_LDA3, 5)

# Best one. 
cons_current_LDA4 <- convert(x = cons_dfm_current, to = "topicmodels") %>%
    LDA(k = 4, method = "VEM", control = list(seed = SEED)) 
get_terms(cons_current_LDA4, 5)

cons_current_LDA5 <- convert(x = cons_dfm_current, to = "topicmodels") %>%
    LDA(k = 5, method = "Gibbs", control = list(seed = SEED)) 
get_terms(cons_current_LDA5, 5)


# betas: for each row, the probability of the term being generated from that topic. 
attributes(cons_current_LDA4)
cons_current_LDA4_betas <- tidy(cons_current_LDA4)
cons_current_LDA4_top_betas <- cons_current_LDA4_betas %>%
    mutate(topic = factor(topic, 
                          labels = c("Topic 1: Management", 
                                     "Topic 2: Heavy Workload", 
                                     "Topic 3: Antiquated", 
                                     "Topic 4: Pay & Promotion"))) %>%
    group_by(topic) %>%
    top_n(n = 8, wt = beta) %>% 
    ungroup() %>%
    arrange(topic, desc(beta))

cons_current_LDA4_top_betas %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(x = term, y = beta, fill = topic)) + 
    scale_fill_brewer(palette = "Set2") +
    geom_bar(stat = "identity", show.legend = "FALSE") +
    facet_wrap(~ topic, scales = "free", nrow = 2, ncol = 2) +
    ggtitle("NFA Cons Current Employee Reviews", subtitle = "Prevalent Terms by 4 Topics") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    theme(axis.title.y = element_text(hjust = 0.4, vjust = 0)) +
    ylab("Probability of Term Association with Topic") +
    xlab(NULL) +
    coord_flip() +
    ggsave(filename = "CONs Current Employees Terms by Topic.png")


# Analyze Document Topic Probabilities for Cons Reviews by Current Employees.
tidy(cons_current_LDA4, matrix = "gamma") %>%
    group_by(document) %>%
    filter(gamma == max(gamma)) %>%
    ungroup() %>%
    ggplot(aes(y = gamma, x = factor(topic))) + geom_boxplot()

# Gamma topic distributions combined with corpus metadata. 
cons_current_LDA4_gamma <- attributes(cons_current_LDA4)$gamma %>% 
    data.frame() %>%
    rename(topic_1 = X1, topic_2 = X2, topic_3 = X3, topic_4 = X4) %>%
    cbind(summary(cons_curr_corpus))
cons_frmr_LDA4_gamma %>% View()
cons_curr_corpus[17] # review text 23 (Topic1). The Most words of all current employee cons reviews.
cons_curr_corpus[35] # review text 51 (Topic 4).
cons_curr_corpus[22] # review text 29
cons_curr_corpus[40] # review text 63
cons_curr_corpus[15] # review text 19. Topic 3. 
cons_curr_corpus[16] # review text 20. Topic 3. 
cons_curr_corpus[22] # review text 20. Topic 3. 
cons_corpus[33]
cons_corpus[44]
cons_corpus[16]
cons_corpus[23]

# Analysis of Main Topic Labels across the Reviews and other statistics ############# 

# Additional Analysis: what was the distribution of most
# popular topic label, average Glassdoor rating based on main topic?

cons_current_employee_topic_stats <- cons_current_LDA4_gamma %>%
    select(starts_with("topic_"), Text, overall_rating) %>%
    gather(starts_with("topic_"), key = 'topic', value = 'gamma') %>%
    group_by(Text) %>%
    filter(gamma == max(gamma))

cons_current_employee_topic_stats %>% 
    ungroup() %>%
    group_by(topic) %>%
    summarise(main_topic_freq = n(),
              median_glassdoor_rating = median(overall_rating),
              sdev = sd(overall_rating))

cons_current_employee_topic_stats %>%
    ggplot(aes(x = topic, fill = topic)) + geom_bar(alpha = 0.85, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") + 
    ylab("Review Count") +
    ggtitle("Current Employee Cons Reviews", subtitle = "Primary Topic Label Distribution") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    ggsave("Main Topic Distribution - Current Employee Cons Reviews.png")
    
# Cons Former Employees
cons_former_employee_topic_stats <- cons_frmr_LDA4_gamma %>%
    select(starts_with("topic_"), Text, overall_rating) %>%
    gather(starts_with("topic_"), key = 'topic', value = 'gamma') %>%
    group_by(Text) %>%
    filter(gamma == max(gamma)) 


cons_former_employee_topic_stats %>% 
    ungroup() %>%
    group_by(topic) %>%
    summarise(main_topic_freq = n(),
              median_glassdoor_rating = median(overall_rating),
              sdev = sd(overall_rating))

cons_former_employee_topic_stats %>% 
    ggplot(aes(x = topic, fill = topic)) + geom_bar(alpha = 0.85, show.legend = FALSE) +
    ylab("Review Count") +
    ggtitle("Former Employee Cons Reviews", subtitle = "Primary Topic Label Distribution") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    scale_y_continuous(limits = c(0,12), breaks = c(0, 4, 8, 12)) +
    ggsave("Main Topic Distribution - Former Employee Cons Reviews.png")

# Pros
pros_topic_stats <- pros_LDA8_gamma %>%
    select(starts_with("topic_"), Text, overall_rating) %>%
    gather(starts_with("topic_"), key = 'topic', value = 'gamma') %>%
    group_by(Text) %>%
    filter(gamma == max(gamma))

pros_topic_stats %>%
    ggplot(aes(y = gamma, x = topic)) + geom_boxplot()

pros_topic_stats %>%
    ggplot(aes(x = overall_rating, fill = topic)) + 
    geom_density(alpha = 0.35, show.legend = FALSE) +
    facet_wrap(~topic) +
    ggtitle("NFA Glassdoor Ratings Density",
            subtitle = "By Pros Reviews Primary Topic Label") +
    xlab("Glassdoor Overall Rating") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    ggsave("Glassdoor Ratings Density vs Pros Topic Label.png")
    

pros_topic_stats %>%
    group_by(topic) %>%
    summarise(main_topic_freq = n(),
              median_glassdoor_rating = median(overall_rating),
              mean_glassdoor_rating = mean(overall_rating),
              sdev = sd(overall_rating))

pros_topic_stats %>% 
    ggplot(aes(x = topic, fill = topic)) + geom_bar(alpha = 0.85, show.legend = FALSE) +
    ylab("Review Count") +
    ggtitle("Employee Pros Reviews", subtitle = "Primary Topic Label Distribution") +
    theme(plot.title = element_text(hjust = 0.4, vjust = 1, size = 16)) +
    theme(plot.subtitle = element_text(hjust = 0.4, vjust = 1, size = 12, face = "italic")) +
    ggsave("Main Topic Distribution - Pros Reviews.png")


# Save R Objects for quick loading next Session ########################################################
# http://www.statmethods.net/interface/workspace.html
save.image(file = "R project data.Rdata")
