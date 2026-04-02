### Data summaries for 'dises_midline2b_household_survey'
### AS 🐚🫧🪼🪸
### 23.01.2025 (Last update)

rm(list = ls())
pacman::p_load(dplyr, stringr, stringi, writexl,ggplot2, purrr, tidyr, broom) 
source("./clients/box_client.R")

# parameters -------------------------------------------------------------------

.households_midline2_box <- 2119452299384 # /Data_Management/Data/_CRDES_CleanData/Midline2/identified/dises_midline2b_household_survey_households.csv
.individuals_midline2_box <- 2119442543047 # /Data_Management/Data/_CRDES_CleanData/Midline2/identified/dises_midline2b_household_survey_individuals.csv 

.prefix_filename <- "midline2b_household_survey_"
.box_folder_plots <- 364761412547

# data -------------------------------------------------------------------------

households_midline2 <- box_read(.households_midline2_box)
individuals_midline2 <- box_read(.individuals_midline2_box)
#str(households_midline2, list.len = ncol(households_midline2))
#str(individuals_midline2, list.len = ncol(individuals_midline2))

# helpers ----------------------------------------------------------------------

pct <- function(x, na_codes = c(-9, -99)) {
  if (is.numeric(x)) x[x %in% na_codes] <- NA
  round(mean(as.numeric(x), na.rm = TRUE) * 100, 1)
}


plot_question <- function(df, var, meta, ymax = NULL){
  
  if(meta$type=="single"){
    
    df %>%
      filter(!is.na(.data[[var]])) %>%
      count(code=as.character(.data[[var]])) %>%
      mutate(
        pct = round(n/sum(n)*100,1),
        label_pct = paste0(pct,"%"),
        answer = meta$answers[code],
        correct = if(!is.null(meta$correct)) code==meta$correct else TRUE,
        answer=factor(answer,levels=unname(meta$answers))
      ) %>%
      ggplot(aes(answer,n,alpha=correct))+
      geom_col()+
      geom_text(aes(label=label_pct), vjust=-0.4, size=3.5)+
      scale_alpha_manual(values=c(.4,1),guide="none")+
      labs(title=meta$label,x=NULL,y="Freq.")+
      {if(!is.null(ymax)) coord_cartesian(ylim=c(0,ymax))}+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=35,hjust=1))
    
  } else {
    
    existing_vars <- intersect(names(meta$answers), names(df))
    
    denom <- df %>%
      select(all_of(existing_vars)) %>%
      filter(if_any(everything(), ~ !is.na(.))) %>%
      nrow()
    
    df %>%
      summarise(across(
        all_of(existing_vars),
        ~sum(.==1,na.rm=TRUE)
      )) %>%
      pivot_longer(
        everything(),
        names_to="option",
        values_to="n"
      ) %>%
      mutate(
        pct = round(n/denom*100,1),
        label_pct = paste0(pct,"%"),
        answer=meta$answers[option],
        correct=if(!is.null(meta$correct)) option%in%meta$correct else TRUE,
        answer=factor(answer,levels=unname(meta$answers))
      ) %>%
      ggplot(aes(answer,n,alpha=correct))+
      geom_col()+
      geom_text(aes(label=label_pct), vjust=-0.4, size=3.5)+
      scale_alpha_manual(values=c(.4,1),guide="none")+
      labs(title=meta$label,x=NULL,y="Freq. (select multiple)")+
      {if(!is.null(ymax)) coord_cartesian(ylim=c(0,ymax))}+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=35,hjust=1))
  }
}



plot_numeric_question <- function(df, var, label){
  
  df %>%
    filter(!is.na(.data[[var]])) %>%
    ggplot(aes(x = .data[[var]])) +
    geom_histogram(bins = 30) +
    labs(
      title = label,
      x = NULL,
      y = "Freq."
    ) +
    theme_minimal()
}


numeric_summary <- function(df, var){
  
  x <- df[[var]]
  x[x %in% c(-9, -99)] <- NA
  
  tibble(
    n_nonmiss = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    pct_zero = pct(x == 0),
    pct_positive = pct(x > 0)
  )
}


# households -------------------------------------------------------------------

table_metadata <- households_midline2 %>%
  summarise(
    total = sum(is_replacement_midline2 == 0, na.rm = TRUE),
    
    surveyed = sum(
      is_replacement_midline2 == 0 &
        is_attrited_midline2 == 0,
      na.rm = TRUE
    ),
    
    merged = sum(
      is_attrited_why == 2,
      na.rm = TRUE
    ),
    
    no_consent = sum(
      is_attrited_why == 1,
      na.rm = TRUE
    ),
    
    moved_away = sum(
      is_attrited_why == 3,
      na.rm = TRUE
    ),
    
    not_available = sum(
      is_attrited_why == 4,
      na.rm = TRUE
    ),
    
    replacements = sum(is_replacement_midline2 == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -total,
    names_to = "status",
    values_to = "n"
  ) %>%
  mutate(
    pct = if_else(
      status == "replacements",
      NA_real_,
      round(n / total * 100, 1)
    )
  )


table_household <- households_midline2 %>%
  filter(
    is_attrited_midline2 == 0,
    is_replacement_midline2 == 0
  ) %>%
  summarise(
    total = n(),
    
    resp_match_baseline = sum(resp_match_midline2 == 1, na.rm = TRUE),
    resp_match_midline  = sum(resp_match_midline2 == 2, na.rm = TRUE),
    resp_match_other    = sum(resp_match_midline2 == 0, na.rm = TRUE),
    
    hh_members_left_midline2  = mean(hh_members_left_midline2, na.rm = TRUE),
    hh_members_new_midline2   = mean(hh_members_new_midline2, na.rm = TRUE),
    hh_members_diff_midline2  = mean(hh_members_diff_midline2, na.rm = TRUE),
    hh_members_count_midline2 = mean(hh_members_count_midline2, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -total,
    names_to = "status",
    values_to = "stat"
  ) %>%
  mutate(
    n = if_else(
      str_detect(status, "^resp_match"),
      as.integer(stat),
      NA_integer_
    ),
    pct = if_else(
      str_detect(status, "^resp_match"),
      round(stat / total * 100, 2),
      NA_real_
    ),
    avg = if_else(
      str_detect(status, "^hh_members"),
      round(stat, 2),
      NA_real_
    )
  ) %>%
  select(total, status, n, pct, avg)


table_cla <- households_midline2 %>%
  filter(is_attrited_midline2 == 0) %>%
  summarise(
    total = sum(!is.na(hh_consent_cla)),
    n = sum(hh_consent_cla == 1, na.rm = TRUE)
  ) %>%
  mutate(
    status = "consent_cla",
    pct = round(n / total * 100, 1)
  ) %>%
  select(total, status, n, pct)

# knowledge module -------------------------------------------------------------

knowledge_meta_fr <- list(
  
  knowledge_01=list(
    index=1, type="single",
    label="4.1. Avez-vous déjà entendu parler de la bilharziose ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="1"),
  
  knowledge_03=list(
    index=3, type="single",
    label="4.3. Pensez-vous que la bilharziose est une maladie ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="1"),
  
  knowledge_04=list(
    index=4, type="single",
    label="4.4. Pensez-vous qu'il s'agit d'une maladie grave ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="1"),
  
  knowledge_05=list(
    index=5, type="single",
    label="4.5. Selon vous, quelle est la cause de la bilharziose ?",
    answers=c(
      "1"="Virus","2"="Ver","3"="Bactéries",
      "4"="L'eau","99"="Autre","5"="Je ne sais pas"),
    correct="2"),
  
  knowledge_06=list(
    index=6, type="multi",
    label="4.6. A votre avis, comment savoir si une personne est atteinte de bilharziose ?",
    answers=c(
      knowledge_06_1="Lorsqu'ils ont de la fièvre",
      knowledge_06_2="En cas de diarrhée",
      knowledge_06_3="En cas de douleurs à l'estomac",
      knowledge_06_4="En cas de sang dans les urines",
      knowledge_06_5="En cas de démangeaisons",
      knowledge_06_6="Je ne sais pas"),
    correct="knowledge_06_4"),
  
  knowledge_07=list(
    index=7, type="single",
    label="4.7. Pensez-vous qu'il existe un test à l'hôpital pour détecter la bilharziose chez un individu ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="1"),
  
  knowledge_09=list(
    index=9, type="multi",
    label="4.9. Selon vous, comment une personne peut-elle se protéger contre la bilharziose ?",
    answers=c(
      knowledge_09_1="Éviter d'uriner dans la rivière",
      knowledge_09_2="Éviter de déféquer dans la rivière",
      knowledge_09_3="Éviter de se rendre à la rivière",
      knowledge_09_4="Éviter de marcher pieds nus",
      knowledge_09_5="Dormir sous une moustiquaire",
      knowledge_09_6="Retirer les plantes dans les points d'eau",
      knowledge_09_99="Autre (à préciser)",
      knowledge_09_7="Je ne sais pas"),
    correct=c(
      "knowledge_09_1","knowledge_09_2",
      "knowledge_09_3","knowledge_09_6")),
  
  knowledge_10=list(
    index=10, type="multi",
    label="4.10. Selon vous, comment peut-on contracter la bilharziose ?",
    answers=c(
      knowledge_10_1="En marchant pieds nus",
      knowledge_10_2="En mangeant sans se laver les mains",
      knowledge_10_3="En allant à la rivière",
      knowledge_10_4="En buvant l'eau de la rivière",
      knowledge_10_5="En se faisant piquer par des moustiques",
      knowledge_10_6="Lors de rapports sexuels avec une personne infectée",
      knowledge_10_7="Je ne sais pas",
      knowledge_10_99="Autre (à préciser)"),
    correct="knowledge_10_3"),
  
  knowledge_11=list(
    index=11, type="single",
    label="4.11. Pensez-vous que la bilharziose est contagieuse ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="0"),
  
  knowledge_12=list(
    index=12, type="single",
    label="4.12. Connaissez-vous l'animal qui transmet la bilharziose ?",
    answers=c(
      "2"="Les moustiques",
      "3"="Grands escargots terrestres",
      "4"="Petits escargots de rivière",
      "5"="Mouches",
      "99"="Autre (à préciser)",
      "1"="Je ne sais pas"),
    correct="4"),
  
  knowledge_13=list(
    index=13, type="single",
    label="4.13 Pensez-vous que la bilharziose peut être guérie sans traitement ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="0"),
  
  knowledge_14=list(
    index=14, type="single",
    label="4.14 Pensez-vous qu'il existe un médicament pour traiter la bilharziose ?",
    answers=c("1"="Oui","0"="Non","2"="Je ne sais pas"),
    correct="1"),
  
  knowledge_24=list(
    index=24, type="single",
    label="4.24. Vrai ou faux : Ceratophyllum demersum possède des racines.",
    answers=c("1"="Vrai","2"="Faux","3"="Je ne sais pas"),
    correct="2"),
  
  knowledge_25=list(
    index=25, type="single",
    label="4.25. Ceratophyllum demersum pousse dans :",
    answers=c(
      "1"="Les eaux profondes des lacs",
      "2"="Les eaux peu profondes, chaudes et stagnantes",
      "3"="Les cours d’eau rapides",
      "4"="Je ne sais pas"),
    correct="2"),
  
  knowledge_26=list(
    index=26, type="single",
    label="4.26. Laquelle de ces plantes est Ceratophyllum demersum ?",
    answers=c("1"="A","2"="B","3"="C","4"="D","5"="Je ne sais pas"),
    correct="3"),
  
  knowledge_27=list(
    index=27, type="single",
    label="4.27. À quelle fréquence faut-il retourner le compost pendant sa production ?",
    answers=c(
      "1"="Tous les jours","2"="Chaque semaine",
      "3"="Une à deux fois par mois",
      "4"="Tous les quelques mois",
      "5"="Jamais","6"="Je ne sais pas"),
    correct="3"),
  
  knowledge_28=list(
    index=28, type="multi",
    label="4.28. Pourquoi faut-il ajouter du compost au sol ?",
    answers=c(
      knowledge_28_1="Il améliore la structure du sol",
      knowledge_28_2="Il aide à retenir l’eau",
      knowledge_28_3="Les nutriments du compost nourrissent les cultures",
      knowledge_28_4="Le compost réduit les déchets",
      knowledge_28_5="Le compost fait économiser de l’argent",
      knowledge_28_6="Le compost peut remplacer les engrais chimiques",
      knowledge_28_7="Je ne sais pas"),
    correct=c(
      "knowledge_28_1","knowledge_28_2",
      "knowledge_28_3","knowledge_28_6")),
  
  knowledge_29=list(
    index=29, type="single",
    label="4.29. Vrai ou faux : Il faut enfouir le compost dans le sol plutôt que de simplement le déposer à la surface.",
    answers=c("1"="Vrai","2"="Faux","3"="Je ne sais pas"),
    correct="2"),
  
  knowledge_30=list(
    index=30, type="single",
    label="4.30. Pourquoi faut-il éviter de nourrir le bétail avec des plantes encore mouillées provenant de la rivière, du lac ou d’un canal d’irrigation ?",
    answers=c(
      "1"="Elles sont moins nutritives",
      "2"="Pour éviter la fasciolose",
      "3"="Pour éviter des problèmes digestifs",
      "4"="Il n’y a aucun problème à donner des plantes encore mouillées aux animaux",
      "5"="Je ne sais pas"),
    correct="2"),
  
  knowledge_31=list(
    index=31, type="single",
    label="4.31. Combien de temps faut-il pour que des plantes aquatiques sèchent complètement lorsqu’elles sont étalées au soleil ?",
    answers=c(
      "1"="1 jour","2"="Moins d’une semaine",
      "3"="Environ deux semaines",
      "4"="Environ un mois",
      "5"="Plus d’un mois","6"="Je ne sais pas"),
    correct="3"),
  
  knowledge_32=list(
    index=32, type="single",
    label="4.32. Quelle est la cause de Fasciola hepatica ?",
    answers=c(
      "1"="Virus","2"="Ver","3"="Bactérie",
      "4"="Eau","99"="Autre","5"="Je ne sais pas"),
    correct="2")
)

knowledge_meta_fr <- knowledge_meta_fr[
  order(map_dbl(knowledge_meta_fr, "index"))
]

knowledge_meta_eng <- list(
  
  knowledge_01=list(
    index=1, type="single",
    label="4.1 Have you ever heard of schistosomiasis?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="1"),
  
  knowledge_03=list(
    index=3, type="single",
    label="4.3 Do you think schistosomiasis is a disease?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="1"),
  
  knowledge_04=list(
    index=4, type="single",
    label="4.4 Do you think it is a serious disease?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="1"),
  
  knowledge_05=list(
    index=5, type="single",
    label="4.5 What is the cause of schistosomiasis?",
    answers=c(
      "1"="Virus","2"="Worm","3"="Bacteria",
      "4"="Water","99"="Other","5"="Don’t know"),
    correct="2"),
  
  knowledge_06=list(
    index=6, type="multi",
    label="4.6 In your opinion, how do you know if someone has schistosomiasis?",
    answers=c(
      knowledge_06_1="When they have a fever",
      knowledge_06_2="When they have diarrhea",
      knowledge_06_3="When they have stomach pain",
      knowledge_06_4="When they have blood in their urine",
      knowledge_06_5="When they have itching",
      knowledge_06_6="I don’t know"),
    correct="knowledge_06_4"),
  
  knowledge_07=list(
    index=7, type="single",
    label="4.7 Do you know if there is a test at the hospital to detect schistosomiasis in an individual?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="1"),
  
  knowledge_09=list(
    index=9, type="multi",
    label="4.9 How can a person protect themselves from schistosomiasis?",
    answers=c(
      knowledge_09_1="Avoid urinating in the river",
      knowledge_09_2="Avoid defecating in the river",
      knowledge_09_3="Avoid going to the river",
      knowledge_09_4="Avoid walking barefoot",
      knowledge_09_5="Sleeping under a mosquito net",
      knowledge_09_6="Remove plants from water sources",
      knowledge_09_99="Other (specify)",
      knowledge_09_7="I don’t know"),
    correct=c(
      "knowledge_09_1","knowledge_09_2",
      "knowledge_09_3","knowledge_09_6")),
  
  knowledge_10=list(
    index=10, type="multi",
    label="4.10 How can one contract schistosomiasis?",
    answers=c(
      knowledge_10_1="By walking barefoot",
      knowledge_10_2="By eating without washing hands",
      knowledge_10_3="By going to the river",
      knowledge_10_4="By drinking river water",
      knowledge_10_5="When bitten by mosquitoes",
      knowledge_10_6="During sexual intercourse with a person infected with schistosomiasis",
      knowledge_10_7="I don’t know",
      knowledge_10_99="Other (specify)"),
    correct="knowledge_10_3"),
  
  knowledge_11=list(
    index=11, type="single",
    label="4.11 Do you think schistosomiasis is contagious?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="0"),
  
  knowledge_12=list(
    index=12, type="single",
    label="4.12 Do you know the animal that transmits schistosomiasis?",
    answers=c(
      "2"="Mosquitoes",
      "3"="Large terrestrial snails",
      "4"="Small river snails",
      "5"="Flies",
      "99"="Other (specify)",
      "1"="I don’t know"),
    correct="4"),
  
  knowledge_13=list(
    index=13, type="single",
    label="4.13 Do you think schistosomiasis can be cured without treatment?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="0"),
  
  knowledge_14=list(
    index=14, type="single",
    label="4.14 Do you think there is a medicine to treat schistosomiasis?",
    answers=c("1"="Yes","0"="No","2"="Don’t know"),
    correct="1"),
  
  knowledge_24=list(
    index=24, type="single",
    label="4.24 True or False: Ceratophyllum demersum has roots.",
    answers=c("1"="True","2"="False","3"="Don’t know"),
    correct="2"),
  
  knowledge_25=list(
    index=25, type="single",
    label="4.25 Ceratophyllum demersum grows in:",
    answers=c(
      "1"="Deep water of the lakes",
      "2"="Shallow warm stagnant water",
      "3"="Fast flowing river streams",
      "4"="Don’t know"),
    correct="2"),
  
  knowledge_26=list(
    index=26, type="single",
    label="4.26 Which plant is Ceratophyllum demersum?",
    answers=c("1"="A","2"="B","3"="C","4"="D","5"="Don’t know"),
    correct="3"),
  
  knowledge_27=list(
    index=27, type="single",
    label="4.27 How often should you turn compost during the production process?",
    answers=c(
      "1"="Every day","2"="Every week",
      "3"="Once or twice a month",
      "4"="Every few months",
      "5"="Never","6"="Don’t know"),
    correct="3"),
  
  knowledge_28=list(
    index=28, type="multi",
    label="4.28 Why should you add compost to soil?",
    answers=c(
      knowledge_28_1="It improves soil structure",
      knowledge_28_2="It helps retain water",
      knowledge_28_3="The nutrients in compost feed crops",
      knowledge_28_4="Compost reduces waste",
      knowledge_28_5="Compost saves money",
      knowledge_28_6="Compost can replace chemical fertilizer",
      knowledge_28_7="I don’t know"),
    correct=c(
      "knowledge_28_1","knowledge_28_2",
      "knowledge_28_3","knowledge_28_4",
      "knowledge_28_5","knowledge_28_6")),
  
  knowledge_29=list(
    index=29, type="single",
    label="4.29 True or False: You need to till compost into the soil rather than just putting it on top of the soil.",
    answers=c("1"="True","2"="False","3"="Don’t know"),
    correct="2"),
  
  knowledge_30=list(
    index=30, type="single",
    label="4.30 Why should you avoid feeding animals plants still wet from the river, lake, or irrigation canal?",
    answers=c(
      "1"="It is not as nutritious for the animals",
      "2"="To avoid fasciola",
      "3"="To avoid digestive issues",
      "4"="It is fine for animals to eat plants still wet",
      "5"="Don’t know"),
    correct="2"),
  
  knowledge_31=list(
    index=31, type="single",
    label="4.31 How long does it take dry aquatic plants to dry thoroughly if spread out in the sun?",
    answers=c(
      "1"="1 day","2"="Less than a week",
      "3"="About two weeks",
      "4"="Around a month",
      "5"="More than a month","6"="Don’t know"),
    correct="3"),
  
  knowledge_32=list(
    index=32, type="single",
    label="4.32 What causes Fasciola hepatica?",
    answers=c(
      "1"="Virus","2"="Worm","3"="Bacteria",
      "4"="Water","99"="Other","5"="Don’t know"),
    correct="2")
)

knowledge_meta_eng <- knowledge_meta_eng[
  order(map_dbl(knowledge_meta_eng, "index"))
]

plots <- imap(
  knowledge_meta_eng,
  ~plot_question(households_midline2, .y, .x, ymax = 2077)
)

iwalk(plots, function(plot_obj, question_name) {
  
  final_name <- paste0(.prefix_filename, question_name, ".png")
  tmp <- file.path(tempdir(), final_name)
  
  ggsave(
    filename = tmp,
    plot = plot_obj,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  box_ul(
    file = tmp,
    dir_id = .box_folder_plots
  )
})







# aquatic vegation -------------------------------------------------------------

water_meta_multi <- list(
  
  hh_12 = list(
    index = 12,
    type = "multi",
    label = "3.27 Reasons for spending time near/in water",
    answers = c(
      hh_12_1  = "Fetch water household",
      hh_12_2  = "Livestock watering",
      hh_12_3  = "Agriculture water",
      hh_12_4  = "Wash clothes",
      hh_12_5  = "Dishes",
      hh_12_6  = "Harvest aquatic vegetation",
      hh_12_7  = "Swim/bathe",
      hh_12_8  = "Play",
      hh_12_9  = "Cross",
      hh_12_10 = "Fish",
      hh_12_99 = "Other"
    )
  ),
  
  hh_15 = list(
    index = 15,
    type = "multi",
    label = "3.30 Use of aquatic vegetation",
    answers = c(
      hh_15_1  = "Sell",
      hh_15_2  = "Fertilizer/compost",
      hh_15_3  = "Animal feed",
      hh_15_4  = "Biodigester",
      hh_15_5  = "Nothing",
      hh_15_99 = "Other"
    )
  )
)


water_meta_numeric <- list(
  
  hh_10 = list(
    label = "3.25 Hours/week near water"
  ),
  
  hh_14 = list(
    label = "3.29 Aquatic vegetation kg/week"
  ),
  
  hh_14_a = list(
    label = "3.29a Vegetation collection events"
  ),
  
  hh_14_b = list(
    label = "3.29b Kg vegetation per event"
  )
)

water_numeric_tables <- imap_dfr(
  water_meta_numeric,
  ~numeric_summary(individuals_midline2, .y) %>%
    mutate(variable = .y),
  .id = NULL
)

veg_prevalence <- individuals_midline2 %>%
  summarise(
    total = sum(!is.na(hh_12_6)),
    harvesters = sum(hh_12_6 == 1, na.rm = TRUE),
    pct = pct(hh_12_6 == 1)
  )

numeric_plots <- imap(
  water_meta_numeric,
  ~plot_numeric_question(individuals_midline2, .y, .x$label)
)

multi_plots <- imap(
  water_meta_multi,
  ~plot_question(individuals_midline2, .y, .x)
)

iwalk(numeric_plots, function(plot_obj, var){
  
  final_name <- paste0(.prefix_filename, "water_", var, ".png")
  tmp <- file.path(tempdir(), final_name)
  
  ggsave(tmp, plot_obj, width = 8, height = 5, dpi = 300)
  
  box_ul(file = tmp, dir_id = .box_folder_plots)
})

iwalk(multi_plots, function(plot_obj, var){
  
  final_name <- paste0(.prefix_filename, "water_", var, ".png")
  tmp <- file.path(tempdir(), final_name)
  
  ggsave(tmp, plot_obj, width = 8, height = 5, dpi = 300)
  
  box_ul(file = tmp, dir_id = .box_folder_plots)
})

# education --------------------------------------------------------------------

# health -----------------------------------------------------------------------

health_meta_single <- list(
  
  health_5_2=list(
    index=1, type="single",
    label="5.2 Sick in last 12 months",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_5=list(
    index=5, type="single",
    label="5.5 Received schisto treatment",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_6=list(
    index=6, type="single",
    label="5.6 Ever diagnosed schisto",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_7=list(
    index=7, type="single",
    label="5.7 Schisto last 12 months",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_7_a=list(
    index=8, type="single",
    label="5.7a Red urine last 12 months",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_8=list(
    index=9, type="single",
    label="5.8 Blood in urine",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_9=list(
    index=10, type="single",
    label="5.9 Blood in stool",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_15=list(
    index=11, type="single",
    label="5.15 Eye sores",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  ),
  
  health_5_16=list(
    index=12, type="single",
    label="5.16 Blurred vision",
    answers=c("1"="Yes","0"="No","2"="Don't know")
  )
)

health_meta_multi <- list(
  
  health_5_3=list(
    index=3, type="multi",
    label="5.3 Type of illness",
    answers=c(
      health_5_3_1="Malaria",
      health_5_3_2="Schistosomiasis",
      health_5_3_3="Diarrhea",
      health_5_3_4="Injuries",
      health_5_3_5="Dental",
      health_5_3_6="Skin",
      health_5_3_7="Eye",
      health_5_3_8="Throat",
      health_5_3_9="Stomach",
      health_5_3_10="Fatigue",
      health_5_3_11="STI",
      health_5_3_12="Trachoma",
      health_5_3_13="River blindness",
      health_5_3_14="Filariasis",
      health_5_3_15="Rift Valley fever",
      health_5_3_99="Other"
    )
  )
)

health_single_plots <- imap(
  health_meta_single,
  ~plot_question(individuals_midline2, .y, .x)
)

health_multi_plots <- imap(
  health_meta_multi,
  ~plot_question(
    individuals_midline2 %>% filter(health_5_2==1),
    .y, .x
  )
)

health_days_summary <- numeric_summary(
  individuals_midline2 %>% filter(health_5_2==1),
  "health_5_4"
)

iwalk(health_single_plots, function(plot_obj, question_name) {
  
  final_name <- paste0(.prefix_filename, "health_", question_name, ".png")
  tmp <- file.path(tempdir(), final_name)
  
  ggsave(
    filename = tmp,
    plot = plot_obj,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  box_ul(
    file = tmp,
    dir_id = .box_folder_plots
  )
})

iwalk(health_multi_plots, function(plot_obj, question_name) {
  
  final_name <- paste0(.prefix_filename, "health_", question_name, ".png")
  tmp <- file.path(tempdir(), final_name)
  
  ggsave(
    filename = tmp,
    plot = plot_obj,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  box_ul(
    file = tmp,
    dir_id = .box_folder_plots
  )
})


# agriculture ------------------------------------------------------------------


# analytics --------------------------------------------------------------------

hh_indicators <- individuals_midline2 %>%
  mutate(across(
    c(hh_10, hh_12_6, health_5_2, health_5_6, health_5_7,
      health_5_7_a, health_5_8, health_5_9, health_5_5),
    ~na_if(., -9) |> na_if(-99)
  )) %>%
  group_by(hhid) %>%
  summarise(
    
    hh_any_water = any(hh_10 > 0, na.rm = TRUE),
    n_water_members = sum(hh_10 > 0, na.rm = TRUE),
    denom_water = sum(!is.na(hh_10)),
    prop_water_members =
      if_else(denom_water > 0,
              n_water_members / denom_water,
              NA_real_),
    
    hh_any_veg = any(hh_12_6 == 1, na.rm = TRUE),
    n_veg_members = sum(hh_12_6 == 1, na.rm = TRUE),
    
    hh_any_sick = any(health_5_2 == 1, na.rm = TRUE),
    n_sick_members = sum(health_5_2 == 1, na.rm = TRUE),
    denom_sick = sum(!is.na(health_5_2)),
    prop_sick_members =
      if_else(denom_sick > 0,
              n_sick_members / denom_sick,
              NA_real_),
    
    hh_any_schisto_diag = any(health_5_6 == 1, na.rm = TRUE),
    hh_recent_schisto   = any(health_5_7 == 1, na.rm = TRUE),
    
    hh_blood_urine = any(health_5_8 == 1, na.rm = TRUE),
    hh_blood_stool = any(health_5_9 == 1, na.rm = TRUE),
    hh_red_urine   = any(health_5_7_a == 1, na.rm = TRUE),
    
    hh_any_treated = any(health_5_5 == 1, na.rm = TRUE),
    
    hh_child_diag =
      any(health_5_6 == 1 & hh_age_midline2 < 15, na.rm = TRUE),
    
    hh_child_recent =
      any(health_5_7 == 1 & hh_age_midline2 < 15, na.rm = TRUE),
    
    hh_child_red_urine =
      any(health_5_7_a == 1 & hh_age_midline2 < 15, na.rm = TRUE),
    
    hh_child_blood_urine =
      any(health_5_8 == 1 & hh_age_midline2 < 15, na.rm = TRUE),
    
    hh_child_blood_stool =
      any(health_5_9 == 1 & hh_age_midline2 < 15, na.rm = TRUE),
    
    hh_child_treated =
      any(health_5_5 == 1 & hh_age_midline2 < 15, na.rm = TRUE),
    
    .groups = "drop"
  )

hh_indicators <- hh_indicators %>%
  left_join(
    households_midline2 %>%
      select(hhid, treatment_arm) %>%
      mutate(
        treatment_arm = str_remove(treatment_arm, "[AB]$")
      ),
    by = "hhid"
  )



table_hh_water <- hh_indicators %>%
  summarise(
    total_households = n(),
    hh_any_water = sum(hh_any_water),
    pct_any_water = round(hh_any_water / total_households * 100, 1),
    avg_members_exposed = mean(n_water_members),
    avg_prop_exposed = mean(prop_water_members)
  )

print(table_hh_water)


table_hh_veg_treat <- hh_indicators %>%
  bind_rows(
    hh_indicators %>% mutate(treatment_arm = "All treatments")
  ) %>%
  group_by(treatment_arm) %>%
  summarise(
    total_households = n(),
    
    hh_any_veg = sum(hh_any_veg, na.rm = TRUE),
    pct_any_veg = round(hh_any_veg / total_households * 100, 1),
    
    avg_harvesters = mean(n_veg_members, na.rm = TRUE),
    sd_harvesters  = sd(n_veg_members, na.rm = TRUE),
    
    .groups = "drop"
  )

print(table_hh_veg_treat)


print(table_hh_veg_treat)


table_hh_water <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_any_water = sum(hh_any_water),
    pct_any_water = round(hh_any_water / total_households * 100, 1),
    
    avg_members_exposed = mean(n_water_members, na.rm = TRUE),
    sd_members_exposed  = sd(n_water_members, na.rm = TRUE),
    
    avg_prop_exposed = mean(prop_water_members, na.rm = TRUE),
    sd_prop_exposed  = sd(prop_water_members, na.rm = TRUE)
  )

print(table_hh_water)


table_hh_veg <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_any_veg = sum(hh_any_veg),
    pct_any_veg = round(hh_any_veg / total_households * 100, 1),
    
    avg_harvesters = mean(n_veg_members, na.rm = TRUE),
    sd_harvesters  = sd(n_veg_members, na.rm = TRUE)
  )

print(table_hh_veg)


table_hh_illness <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_any_sick = sum(hh_any_sick),
    pct_any_sick = round(hh_any_sick / total_households * 100, 1),
    
    avg_sick_members = mean(n_sick_members, na.rm = TRUE),
    sd_sick_members  = sd(n_sick_members, na.rm = TRUE),
    
    avg_prop_sick = mean(prop_sick_members, na.rm = TRUE),
    sd_prop_sick  = sd(prop_sick_members, na.rm = TRUE)
  )

print(table_hh_illness)


table_hh_schisto <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_diag = sum(hh_any_schisto_diag),
    pct_diag = round(hh_diag / total_households * 100, 1),
    
    hh_recent = sum(hh_recent_schisto),
    pct_recent = round(hh_recent / total_households * 100, 1)
  )

print(table_hh_schisto)


table_hh_symptoms <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_red_urine = sum(hh_red_urine),
    pct_red_urine = round(hh_red_urine / total_households * 100, 1),
    
    hh_blood_urine = sum(hh_blood_urine),
    pct_blood_urine = round(hh_blood_urine / total_households * 100, 1),
    
    hh_blood_stool = sum(hh_blood_stool),
    pct_blood_stool = round(hh_blood_stool / total_households * 100, 1)
  )

print(table_hh_symptoms)


table_hh_treatment <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_any_treated = sum(hh_any_treated),
    pct_any_treated = round(hh_any_treated / total_households * 100, 1)
  )

print(table_hh_treatment)


table_hh_child_schisto <- hh_indicators %>%
  summarise(
    total_households = n(),
    
    hh_child_diag = sum(hh_child_diag),
    pct_child_diag = round(hh_child_diag / total_households * 100, 1),
    
    hh_child_recent = sum(hh_child_recent),
    pct_child_recent = round(hh_child_recent / total_households * 100, 1),
    
    hh_child_treated = sum(hh_child_treated),
    pct_child_treated = round(hh_child_treated / total_households * 100, 1)
  )

print(table_hh_child_schisto)

# knowledge by village ---------------------------------------------------------

pct_correct_by_village <- function(df, var, meta) {
  
  if (meta$type == "single") {
    
    df %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(hhid_village, village_name, treatment_arm) %>%
      summarise(
        n = n(),
        pct_correct = mean(as.character(.data[[var]]) == meta$correct, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      mutate(question_label = meta$label)
    
  } else {
    
    option_vars  <- intersect(names(meta$answers), names(df))
    correct_vars <- meta$correct
    n_correct_total <- length(correct_vars)
    
    df2 <- df %>%
      mutate(
        valid = if_any(all_of(option_vars), ~ !is.na(.x)),
        score = if_else(
          valid,
          rowSums(across(all_of(correct_vars), ~ .x == 1), na.rm = TRUE) /
            n_correct_total,
          NA_real_
        )
      )
    
    df2 %>%
      filter(valid) %>%
      group_by(hhid_village, village_name, treatment_arm) %>%
      summarise(
        n = n(),
        pct_correct = mean(score, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      mutate(question_label = meta$label)
  }
}

village_knowledge_long <- purrr::imap_dfr(
  knowledge_meta_eng,
  ~pct_correct_by_village(households_midline2, .y, .x)
) %>%
  mutate(pct_correct = round(pct_correct, 1))

village_index <- village_knowledge_long %>%
  group_by(hhid_village, village_name, treatment_arm) %>%
  summarise(
    overall_knowledge_index = round(mean(pct_correct, na.rm = TRUE), 1),
    .groups = "drop"
  )

village_knowledge_dashboard <- village_knowledge_long %>%
  select(hhid_village, village_name, treatment_arm, n, question_label, pct_correct) %>%
  tidyr::pivot_wider(
    names_from = question_label,
    values_from = pct_correct
  ) %>%
  left_join(
    village_index,
    by = c("hhid_village", "village_name", "treatment_arm")
  ) %>%
  rename(
    "Village Name" = village_name,
    "Treatment Arm" = treatment_arm,
    "Households" = n,
    "Overall Knowledge (%)" = overall_knowledge_index
  ) %>%
  select(
    "Village Name",
    "Treatment Arm",
    "Households",
    "Overall Knowledge (%)",
    everything()
  ) %>% select(-hhid_village)

knowledge_cols <- village_knowledge_dashboard %>%
  select(-`Village Name`, -`Treatment Arm`, -Households) %>%
  names()

treatment_knowledge_dashboard <-
  village_knowledge_dashboard %>%
  group_by(`Treatment Arm`) %>%
  summarise(
    Households = sum(Households, na.rm = TRUE),
    
    across(
      all_of(knowledge_cols),
      ~{
        m  <- round(mean(.x, na.rm = TRUE), 1)
        sd <- round(sd(.x, na.rm = TRUE), 1)
        paste0(m, " (", sd, ")")
      }
    ),
    .groups = "drop"
  )

print(treatment_knowledge_dashboard)

detailed_knowledge_village_long <-
  purrr::imap_dfr(
    knowledge_meta_eng,
    function(meta, var) {
      
      if (meta$type == "single") {
        
        households_midline2 %>%
          filter(!is.na(.data[[var]])) %>%
          group_by(hhid_village, village_name, treatment_arm) %>%
          count(answer_code = as.character(.data[[var]])) %>%
          group_by(hhid_village, village_name, treatment_arm) %>%
          tidyr::complete(
            answer_code = names(meta$answers),
            fill = list(n = 0)
          ) %>%
          mutate(
            n_total = sum(n),
            pct_option = round(n / n_total * 100, 1),
            question_label = meta$label,
            answer_option = meta$answers[answer_code],
            correct = answer_code == meta$correct
          ) %>%
          ungroup() %>%
          select(
            hhid_village,
            village_name,
            treatment_arm,
            question_label,
            answer_option,
            correct,
            n_total,
            pct_option
          )
        
      } else {
        
        option_vars  <- intersect(names(meta$answers), names(households_midline2))
        correct_vars <- meta$correct
        
        df2 <- households_midline2 %>%
          mutate(valid = if_any(all_of(option_vars), ~ !is.na(.x))) %>%
          filter(valid)
        
        df2 %>%
          group_by(hhid_village, village_name, treatment_arm) %>%
          summarise(
            n_total = n(),
            across(
              all_of(option_vars),
              ~sum(.x == 1, na.rm = TRUE)
            ),
            .groups = "drop"
          ) %>%
          pivot_longer(
            cols = all_of(option_vars),
            names_to = "option_var",
            values_to = "n_selected"
          ) %>%
          mutate(
            pct_option = round(n_selected / n_total * 100, 1),
            question_label = meta$label,
            answer_option = meta$answers[option_var],
            correct = option_var %in% correct_vars
          ) %>%
          select(
            hhid_village,
            village_name,
            treatment_arm,
            question_label,
            answer_option,
            correct,
            n_total,
            pct_option
          )
      }
    }
  )

detailed_knowledge_village_long <-
  detailed_knowledge_village_long %>%
  left_join(
    village_knowledge_long %>%
      select(
        hhid_village,
        village_name,
        treatment_arm,
        question_label,
        village_pct_correct = pct_correct
      ),
    by = c("hhid_village", "village_name", "treatment_arm", "question_label")
  ) %>%
  rename(
    "Village Name" = village_name,
    "Treatment Arm" = treatment_arm,
    "Question" = question_label,
    "Answer Option" = answer_option,
    "Correct" = correct,
    "Households" = n_total,
    "% Selecting Option" = pct_option,
    "% Correct (Village)" = village_pct_correct
  ) %>%
  select(
    "Village Name",
    "Treatment Arm",
    "Question",
    "Answer Option",
    "Correct",
    "Households",
    "% Selecting Option",
    "% Correct (Village)"
  )

saveRDS(
  treatment_knowledge_dashboard,
  "temp/treatment_knowledge_dashboard.rds"
)

saveRDS(
  village_knowledge_dashboard,
  "temp/village_knowledge_dashboard.rds"
)

saveRDS(
  detailed_knowledge_village_long,
  "temp/detailed_knowledge_village_long.rds"
)

detailed_knowledge_village_long_raw <-
  purrr::imap_dfr(
    knowledge_meta_eng,
    function(meta, var) {
      
      if (meta$type == "single") {
        
        households_midline2 %>%
          filter(!is.na(.data[[var]])) %>%
          group_by(hhid_village, village_name, treatment_arm) %>%
          count(answer_code = as.character(.data[[var]])) %>%
          group_by(hhid_village, village_name, treatment_arm) %>%
          tidyr::complete(
            answer_code = names(meta$answers),
            fill = list(n = 0)
          ) %>%
          mutate(
            n_total = sum(n),
            pct_option = round(n / n_total * 100, 1),
            variable = var,    
            question_label = meta$label,
            answer_option = meta$answers[answer_code],
            correct = answer_code == meta$correct
          ) %>%
          ungroup() %>%
          select(
            hhid_village,
            village_name,
            treatment_arm,
            variable,       
            question_label,
            answer_code,
            answer_option,
            correct,
            n_total,
            pct_option
          )
        
      } else {
        
        option_vars  <- intersect(names(meta$answers), names(households_midline2))
        correct_vars <- meta$correct
        
        df2 <- households_midline2 %>%
          mutate(valid = if_any(all_of(option_vars), ~ !is.na(.x))) %>%
          filter(valid)
        
        df2 %>%
          group_by(hhid_village, village_name, treatment_arm) %>%
          summarise(
            n_total = n(),
            across(
              all_of(option_vars),
              ~sum(.x == 1, na.rm = TRUE)
            ),
            .groups = "drop"
          ) %>%
          pivot_longer(
            cols = all_of(option_vars),
            names_to = "option_var",
            values_to = "n_selected"
          ) %>%
          mutate(
            variable = var,                    
            question_label = meta$label,
            answer_option = meta$answers[option_var],
            correct = option_var %in% correct_vars,
            pct_option = round(n_selected / n_total * 100, 1)
          ) %>%
          select(
            hhid_village,
            village_name,
            treatment_arm,
            variable,
            question_label,
            option_var,
            answer_option,
            correct,
            n_total,
            pct_option
          )
      }
    }
  )

box_write(detailed_knowledge_village_long_raw, "dises_midline2b_villagelevel_knowledge.csv", 363536289521)
