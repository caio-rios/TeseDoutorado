if(require(here) == F) install.packages("here"); require(here)
if(require(bibliometrix) == F) install.packages("bibliometrix"); require(bibliometrix)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(summarytools) == F) install.packages("summarytools"); require(summarytools)
if(require(tm) == F) install.packages("tm"); require(tm)
if(require(splitstackshape) == F) install.packages("splitstackshape"); require(splitstackshape)
if(require(FactoMineR) == F) install.packages("FactoMineR"); require(FactoMineR)
if(require(factoextra) == F) install.packages("factoextra"); require(factoextra)
if(require(ca) == F) install.packages("ca"); require(ca)
if(require(scales) == F) install.packages("scales"); require(scales)
if(require(patchwork) == F) install.packages("patchwork"); require(patchwork)
if(require(gridExtra) == F) install.packages("gridExtra"); require(gridExtra)
if(require(grid) == F) install.packages("grid"); require(grid)
if(require(tm) == F) install.packages("tm"); require(tm)
if(require(wordcloud) == F) install.packages("wordcloud"); require(wordcloud)
if(require(RColorBrewer) == F) install.packages("RColorBrewer"); require(RColorBrewer)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)
if(require(vegan) == F) install.packages("vegan"); require(vegan)
if(require(eulerr) == F) install.packages("eulerr"); require(eulerr)
if(require(nFactors) == F) install.packages("nFactors"); require(nFactors)
if(require(psych) == F) install.packages("psych"); require(psych)
if(require(VennDiagram) == F) install.packages("VennDiagram"); require(VennDiagram)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)

# para rodar pela primeira vez o package NMF
# if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# 
# BiocManager::install("Biobase")

if(require(NMF) == F) install.packages("NMF"); require(NMF)

theme_set(theme_bw() + theme(panel.grid = element_blank(),
                             axis.title = element_text(face = "bold"),
                             legend.position = "top",
                             plot.title = element_text(hjust = .5)))


# DADOS DO CAPÍTULO INSTITUCIONALISMO------

setwd(here("Dados", "old psci"))

oldpsci <- convert2df(list.files(), dbsource = "wos", format = "bibtex")

oldpsci <- oldpsci %>% mutate(TIKAB = paste(TI, DE, ID, AB, sep = " - "))

oldpsci_inst <- oldpsci %>% 
  filter(str_detect(TIKAB, regex(pattern = "INSTITUTION", ignore_case = T)) == 1)


ev <- oldpsci_inst %>% group_by(PY) %>% summarise(n = n()) %>% 
  left_join(oldpsci %>% group_by(PY) %>% summarise(ntotal = n())) %>% 
  mutate(prop = n/ntotal)

g1 <- ggplot(ev, aes(x = PY, y = n)) +
  geom_line(size = .3) +
  labs(x = "Ano", y = "Publicação (Abs)",
       title = "a") +
  theme(axis.title = element_text(size = 10))

g2 <- ggplot(ev, aes(x = PY, y = prop)) +
  geom_line(size = .3) +
  labs(x = "Ano", y = "Publicação (%)",
       title = "b") +
  scale_y_continuous(labels = percent) +
  theme(axis.title = element_text(size = 10))

g1 + g2


lm(n ~ PY, data = ev)
lm(prop ~ PY, data = ev)


setwd(here("Dados"))
ggsave("grafico 1.png", width = 16, height = 9, units = "cm", dpi = 300)


# DADOS DO CAPÍTULO EXPERIMENTOS

min(oldpsci$PY)
max(oldpsci$PY)


experiment <- oldpsci %>% 
  mutate(EXP = str_detect(TIKAB, "EXPERIMENT")) %>% 
  filter(EXP == 1)

setwd(here("Dados"))

experiment <- experiment %>% 
  mutate(
  CODE = paste0("EXP", 1:nrow(experiment))
) %>% relocate(CODE, .before = 1)

# write.xlsx(experiment %>% transmute(CODE,
#                                     TI, 
#                                     AB, 
#                                     KW = paste0(DE, ";",ID)),
#            "AJPS PSQ EXPERIMENT.xlsx")

setwd(here("Dados"))

exp_out <- read_xlsx("AJPS PSQ EXPERIMENT.xlsx") %>% 
  filter(CHECK == -1) %>% select(CODE) %>% pull() # VETOR DE PAPERS NÃO EXPERIMENTAIS


experiment <- experiment %>% filter(!CODE %in% exp_out)

ev_exp <- experiment %>% group_by(PY) %>% summarise(n = n())

ev_oldpsci <- oldpsci %>% filter(str_detect(DT, "ARTICLE")) %>% 
  group_by(PY) %>% summarise(n_all = n())


ev_exp <- ev_exp %>% left_join(ev_oldpsci) %>% 
  mutate(prop = scales::percent(n/n_all, accuracy = 0.1))

ggplot(ev_exp,
       aes(x = PY, y = n)) +
  geom_line() +
  geom_point(data = ev_exp %>% 
               filter(PY %in% c(min(experiment$PY), 
                                max(experiment$PY), 
                                2000, 2010, 2020))) +
  geom_text(aes(label = paste0(n, "\n(", prop, ")")), size = 2.5, 
            data = ev_exp %>% 
              filter(PY %in% c(min(experiment$PY), 
                               max(experiment$PY), 
                               2000, 2010)),
            vjust = -.4) +
  geom_text(aes(label = paste0(n, "\n(", prop, ")")), size = 2.5, 
            data = ev_exp %>% 
              filter(PY %in% c(2020)),
            vjust = 1.3) +
  expand_limits(y = c(0,55)) +
  scale_x_continuous(breaks = c(min(experiment$PY), 
                                max(experiment$PY),
                                1980, 2000, 2020)) +
  labs(x = "Ano", y = "Artigos Experimentais")


ggsave("grafico 3.png", width = 16, height = 9, units = "cm", dpi = 300)


lm(n ~ PY, data = ev_exp)

# pre 2000

lm(n ~ PY, data = ev_exp %>% filter(PY <= 2000))

# pos 2000

lm(n ~ PY, data = ev_exp %>% filter(PY > 2000))


sum(ev_exp$n)
sum(ev_exp$n_all)

# growth

(1.125-0.033)/0.033


# esquema zona cinzenta, simular dados forçando uma correlação de 0.7

set.seed(34377732)
n <- 30
r <- 0.7

z1 <- rnorm(n)
z2 <- rnorm(n)

# Combinação linear para induzir correlação
x <- z1
y <- r * z1 + sqrt(1 - r^2) * z2

cor(x, y)  # Aproximadamente 0.7

df <- data.frame(instbeh = c(x, -1.5, -2, -1.8), expobs = c(y, 2, 1.8, 2.1))

library(ggalt)

ggplot(df, aes(x = instbeh, y = expobs)) +
  geom_hline(yintercept = 0, color = "grey30", lty = "dashed") +
  geom_vline(xintercept = 0, color = "grey30", lty = "dashed") +
  geom_smooth(se = F, color = "black", method = "lm", size = .5) +
  geom_point() +
  geom_encircle(data = df %>% filter(instbeh <= -1.5 & expobs >= 1.8)) +
  labs(x = "Grau Comportamental", y = "Grau Experimental")

setwd(here("Dados"))
ggsave("grafico 4.png", width = 16, height = 9, units = "cm", dpi = 300)


# capítulo empírico 1 / metodologia------

setwd(here("Dados", "scopus"))


M_SCOPUS <- convert2df(list.files(pattern = ".*\\.bib"), dbsource = "scopus", format = "bibtex")

unique(M_SCOPUS$SO)
paste(min(M_SCOPUS$PY), max(M_SCOPUS$PY), sep = "-")

M_SCOPUS <- M_SCOPUS %>% mutate(SO_FULL = SO,
                  SO = ifelse(SO == "COMPARATIVE POLITICS", "CPOL",
                              ifelse(SO == "STUDIES IN COMPARATIVE INTERNATIONAL DEVELOPMENT", "SCID",
                                     ifelse(SO == "DEMOCRATIZATION", "DEMZTION",
                                            ifelse(SO == "WORLD POLITICS", "WPOL",
                                                   ifelse(SO == "COMPARATIVE POLITICAL STUDIES", "CPS",
                                                          ifelse(SO == "COMPARATIVE EUROPEAN POLITICS", "CEP",
                                                                 ifelse(SO == "PUBLIUS: THE JOURNAL OF FEDERALISM"|
                                                                          SO == "PUBLIUS PUBLIUS" |
                                                                          SO == "PUBLIUS", "PUBLIUS",
                                                                        "ERROR"))))))))

unique(M_SCOPUS$SO)


# GERANDO CODIGO

M_SCOPUS <- M_SCOPUS %>% rownames_to_column(var = "excluir") %>%
  rownames_to_column(var = "CODE") %>% mutate(CODE = paste0("ID ", CODE)) %>%
  select(-2)

setwd(here("Dados"))

# Base WOS

setwd(here("Dados", "wos"))

M <- convert2df(list.files(pattern = ".*\\.bib"), dbsource = "wos", format = "bibtex")

unique(M$SO)
paste(min(M_SCOPUS$PY), max(M_SCOPUS$PY), sep = "-")

M <- M %>% mutate(SO_FULL = SO,
                                SO = ifelse(SO == "COMPARATIVE POLITICS", "CPOL",
                                            ifelse(SO == "STUDIES IN COMPARATIVE INTERNATIONAL DEVELOPMENT", "SCID",
                                                   ifelse(SO == "DEMOCRATIZATION", "DEMZTION",
                                                          ifelse(SO == "WORLD POLITICS", "WPOL",
                                                                 ifelse(SO == "COMPARATIVE POLITICAL STUDIES", "CPS",
                                                                        ifelse(SO == "COMPARATIVE EUROPEAN POLITICS", "CEP",
                                                                               ifelse(SO == "PUBLIUS-THE JOURNAL OF FEDERALISM", "PUBLIUS",
                                                                                      "ERROR"))))))))

unique(M_SCOPUS$SO)


# GERANDO CODIGO

M <- M %>% rownames_to_column(var = "excluir") %>%
  rownames_to_column(var = "CODE") %>% mutate(CODE = paste0("ID ", CODE)) %>%
  select(-2)

M <- M %>% filter(PY != 2025)

setwd(here("Dados"))
#saveRDS(M, "BASE DE DADOS.RDS")

M <- readRDS("BASE DE DADOS.RDS") 
# mainInfo

papers <- M %>% group_by(SO) %>% summarise(Artigos = n(),
                                           TC = sum(TC),
                                           Tempo = paste0(min(PY), 
                                                          " - ",
                                                          max(PY)))

autores <- M %>% group_by(SO) %>% summarise(Autores = str_count(AU, ";") + 1) %>% 
  group_by(SO) %>% summarise(Autores = sum(Autores))

referencias <- M %>% group_by(SO) %>% summarise(Referencias = str_count(CR, ";") + 1) %>% 
  filter(is.na(Referencias) == F) %>% group_by(SO) %>% summarise(Referencias = sum(Referencias))


MainInfo <- papers %>% left_join(autores, by = "SO") %>% 
  left_join(referencias, by = "SO") %>% 
  mutate(`Distribuicao Relativa` = Artigos/sum(Artigos),
         `Citacoes por Artigo` = round(TC/Artigos,2))
MainInfo <- MainInfo %>% 
  add_row(SO = "Total", Artigos = sum(papers$Artigos),
          Autores = sum(autores$Autores), 
          Referencias = sum(referencias$Referencias),
          TC = sum(papers$TC),
          `Distribuicao Relativa` = sum(MainInfo$`Distribuicao Relativa`),
          `Citacoes por Artigo` = round(mean(MainInfo$`Citacoes por Artigo`),2)) %>% 
  mutate(`Distribuicao Relativa` = scales::percent(`Distribuicao Relativa`, accuracy = 0.01L))

MainInfo <- MainInfo %>% mutate(TC = format(TC, big.mark = "."),
                                Autores = format(Autores, big.mark = "."),
                                Referencias = format(Referencias, big.mark = "."),
                                Artigos = format(Artigos, big.mark = "."))

FI <- data.frame(SO = c("CPS", "WPOL",
                        "DEMZTION", "SCID",
                        "CPOL", "PUBLIUS", "CEP"),
                 FI_5 = c(5.5, 5.1, 4.2, 2.6, 2.5, 2.2, 2.1))


MainInfo <- MainInfo %>% left_join(FI, by = "SO")


setwd(here("Dados"))
write.xlsx(MainInfo, "MAININFO.xlsx")



# calculando IBI

M_SCOPUS <- M_SCOPUS %>% mutate(
  BEHAVIOR = str_count(CR, pattern = regex("BEHAVIOR|BEHAVIOUR|PSYCH", ignore_case = T)),
  INSTITUTION = str_count(CR, pattern = regex("INSTITUTION", ignore_case = T))
)


# IBI

M_SCOPUS$IBI <- log((1+M_SCOPUS$BEHAVIOR)/(1+M_SCOPUS$INSTITUTION)) # formula de IBI

# IDENTIFICANDO PAPERS QUE NÃO POSSUEM NENHUMA INFLUENCIA

M_IBI_0 <- M_SCOPUS %>% filter(BEHAVIOR == 0 & INSTITUTION == 0)

cat(nrow(M_IBI_0), "papers, em tese, não sofrem influenciam de nenhuma das lógicas")

# RETIRANDO-OS DA AMOSTRA

M_SCOPUS <- M_SCOPUS %>% filter(!CODE %in% M_IBI_0$CODE)

# descritivas de IBI

z <- data.frame(descr(M_SCOPUS$IBI)) # ESTATÍSTICA DESCRITIVA
z <- z %>% mutate(IBI = round(IBI,2)) %>% 
  rownames_to_column(var = "Métrica") %>% 
  filter(Métrica %in% c("Mean", "Std.Dev", "Min", "Median", "Max", "Skewness")) %>% 
  mutate(Métrica = case_when(Métrica == "Mean" ~ "Média",
                             Métrica == "Std.Dev" ~ "DP",
                             Métrica == "Median" ~ "Mediana",
                             TRUE ~ Métrica))

tema <- ttheme_default(
  core = list(fg_params = list(cex = 0.6)),
  colhead = list(fg_params = list(cex = 0.7))
)


z <- tableGrob(z, theme = tema)
# DISTRIBUICAO

ggplot(M_SCOPUS, aes(x = IBI)) +
  geom_density(fill = "grey90", alpha = .5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "Densidade") +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  annotation_custom(grob = z, xmin = 1, xmax = 4, ymin = .2, ymax = .4)

  
ggsave("grafico 6.png", width = 16, height = 9, units = "cm", dpi = 300)
# JOURNAL X IBI

g <- ggplot(M_SCOPUS, aes(x = SO, y = IBI)) +
  geom_boxplot(fill = "grey") +
  geom_hline(yintercept = 0, lty = "dashed", color = "red") +
  scale_y_continuous(breaks = seq(-4, 4, 1))
g

ggsave("grafico 5.png", width = 16, height = 9, units = "cm", dpi = 300)


# voltando para corpus wos

# palavras mais citadas tfidf no abstract

M$TIKAB <- paste0(M$TI, M$AB, M$DE, M$ID, sep = " - ")

abs <- M %>% mutate(
  TIKAB = removeNumbers(TIKAB),
  TIKAB = removePunctuation(TIKAB),
  TIKAB = stripWhitespace(TIKAB),
  TIKAB = tolower(TIKAB),
  TIKAB = removeWords(TIKAB, stopwords("english"))
) %>% cSplit('TIKAB', sep = " ", direction = "long")

a <- abs %>% group_by(CODE, TIKAB) %>% summarise(n = n())

library(tidytext)


tfidf <- a %>%
  bind_tf_idf(term = TIKAB, document = CODE, n = n) %>% 
  group_by(TIKAB) %>% summarise(n = sum(n),
                                tfidf = sum(tf_idf)) %>% 
  arrange(-tfidf)

print(tfidf, n = 20)

if(require(treemap) == F) install.packages("treemap"); require(treemap)
if(require(treemapify) == F) install.packages("treemapify"); require(treemapify)


setwd(here("Dados"))
png("grafico 6.png", width = 16, height = 9, units = "cm", res = 300)

treemap(tfidf %>% arrange(-tfidf) %>% head(20),
        index="TIKAB",
        vSize="tfidf",
        type="index",
        title = "",
        palette = "Pastel1"
)

dev.off()


social <- M %>%
  mutate(social = str_extract_all(TIKAB, "\\b\\w+ SOCIAL \\w+\\b")) %>% 
  unnest(social) %>% cSplit('social', sep = " ", direction = "wide")

social_pre <- social %>% filter(nchar(social_1) > 4) %>% group_by(social_1) %>% summarise(n = n()) %>% 
  arrange(-n)

social_pos <- social %>% filter(nchar(social_3) > 4) %>% group_by(social_3) %>% summarise(n = n()) %>% 
  arrange(-n)


public <- M %>%
  mutate(public = str_extract_all(TIKAB, "\\b\\w+ PUBLIC \\w+\\b")) %>% 
  unnest(public) %>% cSplit('public', sep = " ", direction = "wide")

public_pre <- public %>% filter(nchar(public_1) > 4) %>% group_by(public_1) %>% summarise(n = n()) %>% 
  arrange(-n)

public_pos <- public %>% filter(nchar(public_3) > 4) %>% group_by(public_3) %>% summarise(n = n()) %>% 
  arrange(-n)


# consulta direcionada

theme_cs <- M

theme_cs$ACCOUNTABILITY <- str_count(theme_cs$TIKAB, regex(pattern = "ACCOUNTABILIT", ignore_case = T))
theme_cs$AGENDA <- str_count(theme_cs$TIKAB, regex(pattern = "AGENDA", ignore_case = T))
theme_cs$AUTHORITARIANISM <- str_count(theme_cs$TIKAB, regex(pattern = "AUTHORITARIAN", ignore_case = T))
theme_cs$BUDGET <- str_count(theme_cs$TIKAB, regex(pattern = "BUDGET", ignore_case = T))
theme_cs$BUREAUCRACY <- str_count(theme_cs$TIKAB, regex(pattern = "BUREAUCRA|AUTHORIT|AGENCY|AGENCIES", ignore_case = T))
theme_cs$CAMPAIGN <- str_count(theme_cs$TIKAB, regex(pattern = "CAMPAIGN", ignore_case = T))
theme_cs$CITIZENSHIP <- str_count(theme_cs$TIKAB, regex(pattern = "CITIZEN", ignore_case = T))
theme_cs$CIVIL_SOCIETY <- str_count(theme_cs$TIKAB, regex(pattern = "CIVIL SOCIET|COMMUNIT|SOCIAL CAPITAL", ignore_case = T))
theme_cs$CLIENTELISM <- str_count(theme_cs$TIKAB, regex(pattern = "CLIENTELIS|PATRONAGE|BROKER|PORK BARREL|POLITICAL MACHINE", ignore_case = T))
theme_cs$COALITION <- str_count(theme_cs$TIKAB, regex(pattern = "COALITION", ignore_case = T))
theme_cs$COMPETITION <- str_count(theme_cs$TIKAB, regex(pattern = "POLITICAL COMPETITION|ELECTORAL COMPETITION|CLOSE RACE|POLITICAL MARKET|INCUMBENT|INCUMBENC", ignore_case = T))
theme_cs$CONFLICT <- str_count(theme_cs$TIKAB, regex(pattern = "CONFLICT", ignore_case = T))
theme_cs$CONSTITUTION <- str_count(theme_cs$TIKAB, regex(pattern = "CONSTITUTION", ignore_case = T))
theme_cs$CORRUPTION <- str_count(theme_cs$TIKAB, regex(pattern = "CORRUP", ignore_case = T))
theme_cs$CRISIS <- str_count(theme_cs$TIKAB, regex(pattern = "CRISIS|CRISES", ignore_case = T))
theme_cs$DECENTRALIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "DECENTRALIZATION|CENTRALIZATION", ignore_case = T))
theme_cs$DECISION_MAKING <- str_count(theme_cs$TIKAB, regex(pattern = "DECISION MAKING|DECISION-MAKING", ignore_case = T))
theme_cs$DELIBERATION <- str_count(theme_cs$TIKAB, regex(pattern = "DELIBERAT", ignore_case = T))
theme_cs$DEMOCRACY <- str_count(theme_cs$TIKAB, regex(pattern = "DEMOCRAC", ignore_case = T))
theme_cs$DEMOCRATIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "DEMOCRATIZATION", ignore_case = T))
theme_cs$ELECTION <- str_count(theme_cs$TIKAB, regex(pattern = "ELECT", ignore_case = T))
theme_cs$ELITES <- str_count(theme_cs$TIKAB, regex(pattern = "ELITE", ignore_case = T))
theme_cs$EMPOWERMENT <- str_count(theme_cs$TIKAB, regex(pattern = "EMPOWERMENT", ignore_case = T))
theme_cs$EQUALITY <- str_count(theme_cs$TIKAB, regex(pattern = "EQUALITY|EQUITY|INEQUALITY", ignore_case = T))
theme_cs$ETHNIC <- str_count(theme_cs$TIKAB, regex(pattern = "ETHNIC", ignore_case = T))
theme_cs$EXECUTIVE <- str_count(theme_cs$TIKAB, regex(pattern = "EXECUTIVE", ignore_case = T))
theme_cs$FEDERALISM <- str_count(theme_cs$TIKAB, regex(pattern = "FEDERALISM|MULTILEVEL GOV", ignore_case = T))
theme_cs$FISCAL <- str_count(theme_cs$TIKAB, regex(pattern = "FISCAL|EXPENDITURE|SPENDING|DEBT", ignore_case = T))
theme_cs$GENDER_WOMEN <- str_count(theme_cs$TIKAB, regex(pattern = "GENDER|WOMAN|WOMEN|FEMINISM", ignore_case = T))
theme_cs$GLOBALIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "GLOBALIZATION", ignore_case = T))
theme_cs$GOVERNANCE <- str_count(theme_cs$TIKAB, regex(pattern = "GOVERNANCE", ignore_case = T))
theme_cs$IDENTITY <- str_count(theme_cs$TIKAB, regex(pattern = "IDENTIT", ignore_case = T))
theme_cs$IDEOLOGY <- str_count(theme_cs$TIKAB, regex(pattern = "IDEOLOG", ignore_case = T))
theme_cs$INEQUALITY <- str_count(theme_cs$TIKAB, regex(pattern = "INEQUALIT", ignore_case = T))
theme_cs$INFORMAL <- str_count(theme_cs$TIKAB, regex(pattern = "INFORMAL", ignore_case = T))
theme_cs$INTEREST_GROUPS <- str_count(theme_cs$TIKAB, regex(pattern = "INTEREST GROUP|LOBBY", ignore_case = T))
theme_cs$INTERNATIONAL_RELATIONS <- str_count(theme_cs$TIKAB, regex(pattern = "INTERNATIONAL RELATIONS|DIPLOMAC|FOREIGN POLIC|REGIONALISM|INTERNATIONAL COOPERATION", ignore_case = T))
theme_cs$JUDICIARY <- str_count(theme_cs$TIKAB, regex(pattern = "JUDICIARY|JUSTICE|COURT", ignore_case = T))
theme_cs$LEADER <- str_count(theme_cs$TIKAB, regex(pattern = "LEADER", ignore_case = T))
theme_cs$LEGISLATIVE <- str_count(theme_cs$TIKAB, regex(pattern = "LEGISLAT|COMMITTEE|PARLIAMENT|SENATE|DEPUTY", ignore_case = T))
theme_cs$LOCAL_GOVERNMENT <- str_count(theme_cs$TIKAB, regex(pattern = "LOCAL|MUNICIPAL|SUBNATIONAL|MAYOR|GOVERNOR", ignore_case = T))
theme_cs$MILITARY_SECURITY <- str_count(theme_cs$TIKAB, regex(pattern = "MILITARY|SECURITY", ignore_case = T))
theme_cs$NETWORK <- str_count(theme_cs$TIKAB, regex(pattern = "NETWORK", ignore_case = T))
theme_cs$PARTICIPATION <- str_count(theme_cs$TIKAB, regex(pattern = "PARTICIPATION", ignore_case = T))
theme_cs$PARTY <- str_count(theme_cs$TIKAB, regex(pattern = "PARTY|PARTIES|PARTISAN", ignore_case = T))
theme_cs$POLARIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "POLARIZATION|POLARIZAÇÃO|POLARIZACAO", ignore_case = T))
theme_cs$POLICE <- str_count(theme_cs$TIKAB, regex(pattern = "POLICE|POLICIA|POLÍCIA", ignore_case = T))
theme_cs$POLICY <- str_count(theme_cs$TIKAB, regex(pattern = "POLICY|POLICIES|WELFARE STATE", ignore_case = T))
theme_cs$POPULISM <- str_count(theme_cs$TIKAB, regex(pattern = "POPULISM", ignore_case = T))
theme_cs$PRESIDENT <- str_count(theme_cs$TIKAB, regex(pattern = "PRESIDENT", ignore_case = T))
theme_cs$PUBLIC_OPINION <- str_count(theme_cs$TIKAB, regex(pattern = "PUBLIC OPINION|COMMUNICATION|MEDIA|JOURNALIS", ignore_case = T))
theme_cs$RACE <- str_count(theme_cs$TIKAB, regex(pattern = "RACE", ignore_case = T))
theme_cs$REFORM <- str_count(theme_cs$TIKAB, regex(pattern = "REFORM", ignore_case = T))
theme_cs$REGIME <- str_count(theme_cs$TIKAB, regex(pattern = "REGIME", ignore_case = T))
theme_cs$REGIME_CHANGE <- str_count(theme_cs$TIKAB, regex(pattern = "REGIME CHANGE|TRANSITION|BREAKDOWN", ignore_case = T))
theme_cs$REGULATION <- str_count(theme_cs$TIKAB, regex(pattern = "REGULAT", ignore_case = T))
theme_cs$RELIGION <- str_count(theme_cs$TIKAB, regex(pattern = "RELIGION", ignore_case = T))
theme_cs$REPRESENTATION <- str_count(theme_cs$TIKAB, regex(pattern = "REPRESENTATION", ignore_case = T))
theme_cs$STABILITY <- str_count(theme_cs$TIKAB, regex(pattern = "STABILITY", ignore_case = T))
theme_cs$STATE <- str_count(theme_cs$TIKAB, regex(pattern = "STATE", ignore_case = T))
theme_cs$TRADE <- str_count(theme_cs$TIKAB, regex(pattern = "TRADE", ignore_case = T))
theme_cs$TRUST <- str_count(theme_cs$TIKAB, regex(pattern = "TRUST", ignore_case = T))
theme_cs$VIOLENCE <- str_count(theme_cs$TIKAB, regex(pattern = "VIOLENCE|REPRESSION", ignore_case = T))
theme_cs$VOTE <- str_count(theme_cs$TIKAB, regex(pattern = "VOTE|TURNOUT|BALLOT", ignore_case = T))
theme_cs$WAR <- str_count(theme_cs$TIKAB, regex(pattern = "\\bWAR\\b ", ignore_case = T))
theme_cs$MIGRATION <- str_count(theme_cs$TIKAB, regex(pattern = "MIGRATION ", ignore_case = T))
theme_cs$PUBLIC_SECTOR <- str_count(theme_cs$TIKAB, regex(pattern = "PUBLIC SECTOR|PUBLIC EMPLOY|PUBLIC ADMINISTRATION|PUBLIC MANAGEMENT|PUBLIC FINANCE", ignore_case = T))
theme_cs$CRIME <- str_count(theme_cs$TIKAB, regex(pattern = "CRIME|DRUG|MAFIA|CRIMINAL|MILITIA", ignore_case = T))

THEO_TYPE <- theme_cs %>%
  gather(key = TERMO, value = CHECK, ACCOUNTABILITY:CRIME) %>% 
  filter(CHECK != 0)


contagem <- THEO_TYPE %>% group_by(TERMO) %>% summarise(n = sum(CHECK),
                                                        paper = n_distinct(CODE)) %>% 
  mutate(prop = paper/nrow(M),
         FIT = round(n*prop, 2),
         TERMO = gsub("_", " ", TERMO), 
         TERMO = str_to_title(TERMO)) %>% 
  arrange(-FIT)

ggplot(contagem %>% head(15) %>% 
         mutate(label = ifelse(TERMO %in% c("Election", "Vote", "Citizenship", "Public Opinion"), 
                               "Comportamental", "Institucional")), aes(x = reorder(TERMO, FIT), y = FIT)) +
  geom_segment(aes(xend = TERMO,
                   y = 0, yend = FIT), color = "grey") +
  geom_point(size = 3, aes(color = label)) +
  geom_text(aes(label = FIT), hjust = -.2, size = 3) +
  coord_flip() +
  scale_color_manual(values = c("Comportamental" = "tomato3",
                                "Institucional" = "cornflowerblue")) +
  expand_limits(y = c(0, 2500)) +
  labs(x = "Termo", color = "")


setwd(here("Dados"))
ggsave("grafico 7.png", width = 16, height = 10, units = "cm", dpi = 300)


THEO_TYPE <- THEO_TYPE %>% mutate(TERMO = gsub("_", " ", TERMO), 
                                  TERMO = str_to_title(TERMO))

to_ca <- THEO_TYPE %>% filter(TERMO %in% c("State", "Party", "Democracy", "Policy",
                                           "Election", "Bureaucracy", "Regime",
                                           "Local Government", "Vote", "Authoritarianism",
                                           "Citizenship", "Legislative", "Public Opinion",
                                           "Reform", "Democratization")) %>% 
  group_by(TERMO, SO) %>% summarise(n = n()) %>% 
  spread(SO, n)

to_ca[is.na(to_ca)] <- 0

to_ca <- column_to_rownames(to_ca, var = "TERMO")

res.ca <- CA(to_ca, graph = T)
eig <- as.data.frame(get_eigenvalue(res.ca))

fviz_eig(res.ca)

eig <- eig %>% rownames_to_column(var = "Dimensao")

ggplot(eig, aes(x = Dimensao, y = variance.percent)) +
  geom_bar(stat = "identity", fill = "tomato3", width = .6) +
  geom_point() +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = round(variance.percent, 2)), size = 3, vjust = -.5) +
  labs(x = "Dimensão", y = "Variância Explicada")

ggsave("grafico 8.png", width = 16, height = 10, units = "cm", dpi = 300)

df <- rbind(as.data.frame(res.ca$row$coord) %>% rownames_to_column(var = "TERMO") %>% 
              mutate(label = ifelse(TERMO %in% c("Election", "Vote", "Citizenship", "Public Opinion"), 
                                    "Comportamental", "Institucional")),
            as.data.frame(res.ca$col$coord) %>% rownames_to_column(var = "TERMO") %>% mutate(label = "Periódico"))


ggplot(df, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  geom_vline(xintercept = 0, color = "grey", lty = "dashed") +
  geom_point(aes(color = label)) +
  geom_text_repel(aes(label = TERMO, color = label), size = 3) +
  scale_color_manual(values = c("Periódico" = "black",
                                "Comportamental" = "tomato3",
                                "Institucional" = "cornflowerblue")) +
  labs(x = "Dim 1 (69,66%)", y = "Dim 2 (20,93%)", color = "")

ggsave("grafico 9.png", width = 16, height = 12, units = "cm", dpi = 300)


# referencias mais citadas

M_REF <- M %>% cSplit('CR', sep = ";", direction = "long")


most_cr <- M_REF %>% group_by(CR, SO) %>% 
  summarise(n = n()) %>% 
  group_by(CR) %>% summarise(TC = sum(n),
                             invsimp = diversity(n, index = "shannon")/log(7)) %>% 
  filter(TC >= 92)

write.xlsx(most_cr, "tabela1.xlsx")


# países mais estudados

paises <- M


paises$Abkhazia <- str_detect(paises$TIKAB, regex(pattern = "\\bAbkhazia", ignore_case = T))
paises$Afghanistan <- str_detect(paises$TIKAB, regex(pattern = "\\bAfghanistan", ignore_case = T))
paises$Albania <- str_detect(paises$TIKAB, regex(pattern = "\\bAlbania", ignore_case = T))
paises$Algeria <- str_detect(paises$TIKAB, regex(pattern = "\\bAlgeria", ignore_case = T))
paises$Andorra <- str_detect(paises$TIKAB, regex(pattern = "\\bAndorra", ignore_case = T))
paises$Angola <- str_detect(paises$TIKAB, regex(pattern = "\\bAngola", ignore_case = T))
paises$Antigua_and_Barbuda <- str_detect(paises$TIKAB, regex(pattern = "\\bAntigua and Barbuda", ignore_case = T))
paises$Argentina <- str_detect(paises$TIKAB, regex(pattern = "\\bArgentin", ignore_case = T))
paises$Armenia <- str_detect(paises$TIKAB, regex(pattern = "\\bArmenia", ignore_case = T))
paises$Australia <- str_detect(paises$TIKAB, regex(pattern = "\\bAustralia", ignore_case = T))
paises$Austria <- str_detect(paises$TIKAB, regex(pattern = "\\bAustria", ignore_case = T))
paises$Azerbaijan <- str_detect(paises$TIKAB, regex(pattern = "\\bAzerbaijan", ignore_case = T))
paises$Bahamas <- str_detect(paises$TIKAB, regex(pattern = "\\bBahamas", ignore_case = T))
paises$Bahrain <- str_detect(paises$TIKAB, regex(pattern = "\\bBahrain", ignore_case = T))
paises$Bangladesh <- str_detect(paises$TIKAB, regex(pattern = "\\bBangladesh", ignore_case = T))
paises$Barbados <- str_detect(paises$TIKAB, regex(pattern = "\\bBarbados", ignore_case = T))
paises$Belarus <- str_detect(paises$TIKAB, regex(pattern = "\\bBelarus", ignore_case = T))
paises$Belgium <- str_detect(paises$TIKAB, regex(pattern = "\\bBelgi", ignore_case = T))
paises$Belize <- str_detect(paises$TIKAB, regex(pattern = "\\bBelize", ignore_case = T))
paises$Benin <- str_detect(paises$TIKAB, regex(pattern = "\\bBenin", ignore_case = T))
paises$Bhutan <- str_detect(paises$TIKAB, regex(pattern = "\\bBhutan", ignore_case = T))
paises$Bolivia <- str_detect(paises$TIKAB, regex(pattern = "\\bBolivia", ignore_case = T))
paises$Bosnia_and_Herzegovina <- str_detect(paises$TIKAB, regex(pattern = "\\bBosnia and Herzegovina", ignore_case = T))
paises$Botswana <- str_detect(paises$TIKAB, regex(pattern = "\\bBotswana", ignore_case = T))
paises$Brazil <- str_detect(paises$TIKAB, regex(pattern = "\\bBrazil", ignore_case = T))
paises$Brunei <- str_detect(paises$TIKAB, regex(pattern = "\\bBrunei", ignore_case = T))
paises$Bulgaria <- str_detect(paises$TIKAB, regex(pattern = "\\bBulgaria", ignore_case = T))
paises$Burkina_Faso <- str_detect(paises$TIKAB, regex(pattern = "\\bBurkina Faso", ignore_case = T))
paises$Burundi <- str_detect(paises$TIKAB, regex(pattern = "\\bBurundi", ignore_case = T))
paises$Cambodia <- str_detect(paises$TIKAB, regex(pattern = "\\bCambodia", ignore_case = T))
paises$Cameroon <- str_detect(paises$TIKAB, regex(pattern = "\\bCameroon", ignore_case = T))
paises$Canada <- str_detect(paises$TIKAB, regex(pattern = "\\bCanad", ignore_case = T))
paises$Cape_Verde <- str_detect(paises$TIKAB, regex(pattern = "\\bCape Verde", ignore_case = T))
paises$Central_African_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bCentral African Republic", ignore_case = T))
paises$Chad <- str_detect(paises$TIKAB, regex(pattern = "\\bChad", ignore_case = T))
paises$Chile <- str_detect(paises$TIKAB, regex(pattern = "\\bChile", ignore_case = T))
paises$China <- str_detect(paises$TIKAB, regex(pattern = "\\bChina|\\bChinese", ignore_case = T))
paises$Colombia <- str_detect(paises$TIKAB, regex(pattern = "\\bColombia", ignore_case = T))
paises$Comoros <- str_detect(paises$TIKAB, regex(pattern = "\\bComoros", ignore_case = T))
paises$Congo <- str_detect(paises$TIKAB, regex(pattern = "\\bCongo", ignore_case = T))
paises$Cook_Islands <- str_detect(paises$TIKAB, regex(pattern = "\\bCook Islands", ignore_case = T))
paises$Costa_Rica <- str_detect(paises$TIKAB, regex(pattern = "\\bCosta Rica", ignore_case = T))
paises$Cote_divoire <- str_detect(paises$TIKAB, regex(pattern = "\\bCôte d'Ivoire|\\bCote divoire|\\bcote d ivoire|\\bcote d'ivoire", ignore_case = T))
paises$Croatia <- str_detect(paises$TIKAB, regex(pattern = "\\bCroatia", ignore_case = T))
paises$Cuba <- str_detect(paises$TIKAB, regex(pattern = "\\bCuba", ignore_case = T))
paises$Cyprus <- str_detect(paises$TIKAB, regex(pattern = "\\bCyprus", ignore_case = T))
paises$Czech_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bCzech Republic", ignore_case = T))
paises$Denmark <- str_detect(paises$TIKAB, regex(pattern = "\\bDenmark|Danish", ignore_case = T))
paises$Djibouti <- str_detect(paises$TIKAB, regex(pattern = "\\bDjibouti", ignore_case = T))
paises$Dominica <- str_detect(paises$TIKAB, regex(pattern = "\\bDominica ", ignore_case = T))
paises$Dominican_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bDominican Republic", ignore_case = T))
paises$East_Timor <- str_detect(paises$TIKAB, regex(pattern = "\\bEast Timor", ignore_case = T))
paises$Ecuador <- str_detect(paises$TIKAB, regex(pattern = "\\bEcuador", ignore_case = T))
paises$Egypt <- str_detect(paises$TIKAB, regex(pattern = "\\bEgyp", ignore_case = T))
paises$El_Salvador <- str_detect(paises$TIKAB, regex(pattern = "\\bEl Salvador", ignore_case = T))
paises$Equatorial_Guinea <- str_detect(paises$TIKAB, regex(pattern = "\\bEquatorial Guinea", ignore_case = T))
paises$Eritrea <- str_detect(paises$TIKAB, regex(pattern = "\\bEritrea", ignore_case = T))
paises$Estonia <- str_detect(paises$TIKAB, regex(pattern = "\\bEstonia", ignore_case = T))
paises$Ethiopia <- str_detect(paises$TIKAB, regex(pattern = "\\bEthiopia", ignore_case = T))
paises$Fiji <- str_detect(paises$TIKAB, regex(pattern = "\\bFiji", ignore_case = T))
paises$Finland <- str_detect(paises$TIKAB, regex(pattern = "\\bFinland", ignore_case = T))
paises$France <- str_detect(paises$TIKAB, regex(pattern = "\\bFrance|\\bFrench", ignore_case = T))
paises$Gabon <- str_detect(paises$TIKAB, regex(pattern = "\\bGabon", ignore_case = T))
paises$Gambia <- str_detect(paises$TIKAB, regex(pattern = "\\bGambia", ignore_case = T))
paises$Georgia <- str_detect(paises$TIKAB, regex(pattern = "\\bGeorgia", ignore_case = T))
paises$Germany <- str_detect(paises$TIKAB, regex(pattern = "\\bGerman", ignore_case = T))
paises$Ghana <- str_detect(paises$TIKAB, regex(pattern = "\\bGhana", ignore_case = T))
paises$Greece <- str_detect(paises$TIKAB, regex(pattern = "\\bGreece|\\bGreek", ignore_case = T))
paises$Grenada <- str_detect(paises$TIKAB, regex(pattern = "\\bGrenada", ignore_case = T))
paises$Guatemala <- str_detect(paises$TIKAB, regex(pattern = "\\bGuatemala", ignore_case = T))
paises$Guinea <- str_detect(paises$TIKAB, regex(pattern = "\\bGuinea", ignore_case = T))
paises$Guinea_Bissau <- str_detect(paises$TIKAB, regex(pattern = "\\bGuinea-Bissau|\\bGuinea Bissau", ignore_case = T))
paises$Guyana <- str_detect(paises$TIKAB, regex(pattern = "\\bGuyana", ignore_case = T))
paises$Haiti <- str_detect(paises$TIKAB, regex(pattern = "\\bHaiti", ignore_case = T))
paises$Honduras <- str_detect(paises$TIKAB, regex(pattern = "\\bHondura", ignore_case = T))
paises$Hungary <- str_detect(paises$TIKAB, regex(pattern = "\\bHungar", ignore_case = T))
paises$Iceland <- str_detect(paises$TIKAB, regex(pattern = "\\bIceland", ignore_case = T))
paises$India <- str_detect(paises$TIKAB, regex(pattern = "\\bIndia", ignore_case = T))
paises$Indonesia <- str_detect(paises$TIKAB, regex(pattern = "\\bIndonesia", ignore_case = T))
paises$Iran <- str_detect(paises$TIKAB, regex(pattern = "\\bIran", ignore_case = T))
paises$Iraq <- str_detect(paises$TIKAB, regex(pattern = "\\bIraq", ignore_case = T))
paises$Ireland <- str_detect(paises$TIKAB, regex(pattern = "\\bIreland", ignore_case = T))
paises$Israel <- str_detect(paises$TIKAB, regex(pattern = "\\bIsrael", ignore_case = T))
paises$Italy <- str_detect(paises$TIKAB, regex(pattern = "\\bItaly|\\bitalian", ignore_case = T))
paises$Ivory_Coast <- str_detect(paises$TIKAB, regex(pattern = "\\bIvory Coast", ignore_case = T))
paises$Jamaica <- str_detect(paises$TIKAB, regex(pattern = "\\bJamaica", ignore_case = T))
paises$Japan <- str_detect(paises$TIKAB, regex(pattern = "\\bJapan", ignore_case = T))
paises$Jordan <- str_detect(paises$TIKAB, regex(pattern = "\\bJordan", ignore_case = T))
paises$Kazakhstan <- str_detect(paises$TIKAB, regex(pattern = "\\bKazakhstan", ignore_case = T))
paises$Kenya <- str_detect(paises$TIKAB, regex(pattern = "\\bKenya", ignore_case = T))
paises$Kiribati <- str_detect(paises$TIKAB, regex(pattern = "\\bKiribati", ignore_case = T))
paises$Kosovo <- str_detect(paises$TIKAB, regex(pattern = "\\bKosovo", ignore_case = T))
paises$Kuwait <- str_detect(paises$TIKAB, regex(pattern = "\\bKuwait", ignore_case = T))
paises$Kyrgyzstan <- str_detect(paises$TIKAB, regex(pattern = "\\bKyrgyzstan", ignore_case = T))
paises$Laos <- str_detect(paises$TIKAB, regex(pattern = "\\bLaos", ignore_case = T))
paises$Latvia <- str_detect(paises$TIKAB, regex(pattern = "\\bLatvia", ignore_case = T))
paises$Lebanon <- str_detect(paises$TIKAB, regex(pattern = "\\bLebanon", ignore_case = T))
paises$Lesotho <- str_detect(paises$TIKAB, regex(pattern = "\\bLesotho", ignore_case = T))
paises$Liberia <- str_detect(paises$TIKAB, regex(pattern = "\\bLiberia", ignore_case = T))
paises$Libya <- str_detect(paises$TIKAB, regex(pattern = "\\bLibya", ignore_case = T))
paises$Liechtenstein <- str_detect(paises$TIKAB, regex(pattern = "\\bLiechtenstein", ignore_case = T))
paises$Lithuania <- str_detect(paises$TIKAB, regex(pattern = "\\bLithuania", ignore_case = T))
paises$Luxembourg <- str_detect(paises$TIKAB, regex(pattern = "\\bLuxembourg", ignore_case = T))
paises$Macedonia <- str_detect(paises$TIKAB, regex(pattern = "\\bMacedonia", ignore_case = T))
paises$Madagascar <- str_detect(paises$TIKAB, regex(pattern = "\\bMadagascar", ignore_case = T))
paises$Malawi <- str_detect(paises$TIKAB, regex(pattern = "\\bMalawi", ignore_case = T))
paises$Malaysia <- str_detect(paises$TIKAB, regex(pattern = "\\bMalaysia", ignore_case = T))
paises$Maldives <- str_detect(paises$TIKAB, regex(pattern = "\\bMaldives", ignore_case = T))
paises$Mali <- str_detect(paises$TIKAB, regex(pattern = " \\bMali", ignore_case = T))
paises$Malta <- str_detect(paises$TIKAB, regex(pattern = "\\bMalta", ignore_case = T))
paises$Marshall_Islands <- str_detect(paises$TIKAB, regex(pattern = "\\bMarshall Islands", ignore_case = T))
paises$Mauritania <- str_detect(paises$TIKAB, regex(pattern = "\\bMauritania", ignore_case = T))
paises$Mauritius <- str_detect(paises$TIKAB, regex(pattern = "\\bMauritius", ignore_case = T))
paises$Mexico <- str_detect(paises$TIKAB, regex(pattern = "\\bMexic", ignore_case = T))
paises$Micronesia <- str_detect(paises$TIKAB, regex(pattern = "\\bMicronesia", ignore_case = T))
paises$Moldova <- str_detect(paises$TIKAB, regex(pattern = "\\bMoldova", ignore_case = T))
paises$Monaco <- str_detect(paises$TIKAB, regex(pattern = "\\bMonaco", ignore_case = T))
paises$Mongolia <- str_detect(paises$TIKAB, regex(pattern = "\\bMongolia", ignore_case = T))
paises$Montenegro <- str_detect(paises$TIKAB, regex(pattern = "\\bMontenegro", ignore_case = T))
paises$Morocco <- str_detect(paises$TIKAB, regex(pattern = "\\bMorocco", ignore_case = T))
paises$Mozambique <- str_detect(paises$TIKAB, regex(pattern = "\\bMozambique", ignore_case = T))
paises$Myanmar <- str_detect(paises$TIKAB, regex(pattern = "\\bMyanmar|Burma", ignore_case = T))
paises$Nagorno_Karabakh <- str_detect(paises$TIKAB, regex(pattern = "\\bNagorno-Karabakh|\\bNagorno Karabakh", ignore_case = T))
paises$Namibia <- str_detect(paises$TIKAB, regex(pattern = "\\bNamibia", ignore_case = T))
paises$Nauru <- str_detect(paises$TIKAB, regex(pattern = "\\bNauru", ignore_case = T))
paises$Nepal <- str_detect(paises$TIKAB, regex(pattern = "\\bNepal", ignore_case = T))
paises$Netherlands <- str_detect(paises$TIKAB, regex(pattern = "\\bNetherlands", ignore_case = T))
paises$New_Zealand <- str_detect(paises$TIKAB, regex(pattern = "\\bNew Zealand", ignore_case = T))
paises$Nicaragua <- str_detect(paises$TIKAB, regex(pattern = "\\bNicaragua", ignore_case = T))
paises$Niger <- str_detect(paises$TIKAB, regex(pattern = "\\bNiger ", ignore_case = T))
paises$Nigeria <- str_detect(paises$TIKAB, regex(pattern = "\\bNigeria", ignore_case = T))
paises$Niue <- str_detect(paises$TIKAB, regex(pattern = "\\bNiue", ignore_case = T))
paises$North_Korea <- str_detect(paises$TIKAB, regex(pattern = "\\bNorth Korea", ignore_case = T))
paises$Norway <- str_detect(paises$TIKAB, regex(pattern = "\\bNorway|\\bNorwegian", ignore_case = T))
paises$Oman <- str_detect(paises$TIKAB, regex(pattern = "\\bOman", ignore_case = T))
paises$Pakistan <- str_detect(paises$TIKAB, regex(pattern = "\\bPakistan", ignore_case = T))
paises$Palau <- str_detect(paises$TIKAB, regex(pattern = "\\bPalau", ignore_case = T))
paises$Palestine <- str_detect(paises$TIKAB, regex(pattern = "\\bPalestin", ignore_case = T))
paises$Panama <- str_detect(paises$TIKAB, regex(pattern = "\\bPanam", ignore_case = T))
paises$Papua_New_Guinea <- str_detect(paises$TIKAB, regex(pattern = "\\bPapua New Guinea", ignore_case = T))
paises$Paraguay <- str_detect(paises$TIKAB, regex(pattern = "\\bParagu", ignore_case = T))
paises$Peru <- str_detect(paises$TIKAB, regex(pattern = "\\bPeru", ignore_case = T))
paises$Philippines <- str_detect(paises$TIKAB, regex(pattern = "\\bPhilippin", ignore_case = T))
paises$Poland <- str_detect(paises$TIKAB, regex(pattern = "\\bPoland|Polish", ignore_case = T))
paises$Portugal <- str_detect(paises$TIKAB, regex(pattern = "\\bPortug", ignore_case = T))
paises$Qatar <- str_detect(paises$TIKAB, regex(pattern = "\\bQatar", ignore_case = T))
paises$Romania <- str_detect(paises$TIKAB, regex(pattern = "\\bRomania", ignore_case = T))
paises$Russia <- str_detect(paises$TIKAB, regex(pattern = "\\bRussia", ignore_case = T))
paises$Rwanda <- str_detect(paises$TIKAB, regex(pattern = "\\bRwanda", ignore_case = T))
paises$Sahrawi_Arab_Democratic_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bSahrawi|\\bArab Democratic Republic", ignore_case = T))
paises$Saint_Kitts_and_Nevis <- str_detect(paises$TIKAB, regex(pattern = "\\bSaint Kitts and Nevis", ignore_case = T))
paises$Saint_Lucia <- str_detect(paises$TIKAB, regex(pattern = "\\bSaint Lucia", ignore_case = T))
paises$Saint_Vincent_and_the_Grenadines <- str_detect(paises$TIKAB, regex(pattern = "\\bSaint Vincent and the Grenadines", ignore_case = T))
paises$Samoa <- str_detect(paises$TIKAB, regex(pattern = "\\bSamoa", ignore_case = T))
paises$San_Marino <- str_detect(paises$TIKAB, regex(pattern = "\\bSan Marino", ignore_case = T))
paises$Sao_Tome_and_Principe <- str_detect(paises$TIKAB, regex(pattern = "\\bSão Tomé and Príncipe|\\bSao Tome and Principe", ignore_case = T))
paises$Saudi_Arabia <- str_detect(paises$TIKAB, regex(pattern = "\\bSaudi Arabia", ignore_case = T))
paises$Senegal <- str_detect(paises$TIKAB, regex(pattern = "\\bSenegal", ignore_case = T))
paises$Serbia <- str_detect(paises$TIKAB, regex(pattern = "\\bSerbia", ignore_case = T))
paises$Seychelles <- str_detect(paises$TIKAB, regex(pattern = "\\bSeychelles", ignore_case = T))
paises$Sierra_Leone <- str_detect(paises$TIKAB, regex(pattern = "\\bSierra Leone", ignore_case = T))
paises$Singapore <- str_detect(paises$TIKAB, regex(pattern = "\\bSingapor", ignore_case = T))
paises$Slovakia <- str_detect(paises$TIKAB, regex(pattern = "\\bSlovak", ignore_case = T))
paises$Slovenia <- str_detect(paises$TIKAB, regex(pattern = "\\bSlovenia", ignore_case = T))
paises$Solomon_Islands <- str_detect(paises$TIKAB, regex(pattern = "\\bSolomon Islands", ignore_case = T))
paises$Somalia <- str_detect(paises$TIKAB, regex(pattern = "\\bSomali", ignore_case = T))
paises$South_Africa <- str_detect(paises$TIKAB, regex(pattern = "\\bSouth Africa", ignore_case = T))
paises$South_Korea <- str_detect(paises$TIKAB, regex(pattern = "\\bSouth Korea", ignore_case = T))
paises$South_Ossetia <- str_detect(paises$TIKAB, regex(pattern = "\\bSouth Ossetia", ignore_case = T))
paises$Spain <- str_detect(paises$TIKAB, regex(pattern = "\\bSpain|Spanish", ignore_case = T))
paises$Sri_Lanka <- str_detect(paises$TIKAB, regex(pattern = "\\bSri Lanka", ignore_case = T))
paises$Sudan <- str_detect(paises$TIKAB, regex(pattern = "\\bSudan", ignore_case = T))
paises$Suriname <- str_detect(paises$TIKAB, regex(pattern = "\\bSuriname", ignore_case = T))
paises$Swaziland <- str_detect(paises$TIKAB, regex(pattern = "\\bSwaziland", ignore_case = T))
paises$Sweden <- str_detect(paises$TIKAB, regex(pattern = "\\bSwed", ignore_case = T))
paises$Switzerland <- str_detect(paises$TIKAB, regex(pattern = "\\bSwitzerland", ignore_case = T))
paises$Syria <- str_detect(paises$TIKAB, regex(pattern = "\\bSyria", ignore_case = T))
paises$Taiwan <- str_detect(paises$TIKAB, regex(pattern = "\\bTaiwan", ignore_case = T))
paises$Tajikistan <- str_detect(paises$TIKAB, regex(pattern = "\\bTajikistan", ignore_case = T))
paises$Tanzania <- str_detect(paises$TIKAB, regex(pattern = "\\bTanzania", ignore_case = T))
paises$Thailand <- str_detect(paises$TIKAB, regex(pattern = "\\bThailand", ignore_case = T))
paises$Timor_Leste <- str_detect(paises$TIKAB, regex(pattern = "\\bTimor-Leste|\\bEast Timor|\\bTimor Leste", ignore_case = T))
paises$Togo <- str_detect(paises$TIKAB, regex(pattern = "\\bTogo", ignore_case = T))
paises$Tonga <- str_detect(paises$TIKAB, regex(pattern = "\\bTonga", ignore_case = T))
paises$Trinidad_and_Tobago <- str_detect(paises$TIKAB, regex(pattern = "\\bTrinidad and Tobago", ignore_case = T))
paises$Tunisia <- str_detect(paises$TIKAB, regex(pattern = "\\bTunisia", ignore_case = T))
paises$Turkey <- str_detect(paises$TIKAB, regex(pattern = "\\bTurkey|\\bTurkish", ignore_case = T))
paises$Turkmenistan <- str_detect(paises$TIKAB, regex(pattern = "\\bTurkmenistan", ignore_case = T))
paises$Tuvalu <- str_detect(paises$TIKAB, regex(pattern = "\\bTuvalu", ignore_case = T))
paises$Uganda <- str_detect(paises$TIKAB, regex(pattern = "\\bUganda", ignore_case = T))
paises$Ukraine <- str_detect(paises$TIKAB, regex(pattern = "\\bUkraine", ignore_case = T))
paises$United_Arab_Emirates <- str_detect(paises$TIKAB, regex(pattern = "\\bUnited Arab Emirates", ignore_case = T))
paises$United_Kingdom <- str_detect(paises$TIKAB, regex(pattern = "\\bUnited Kingdom|\\bu\\.k\\.|\\bengland|\\bgreat britain", ignore_case = T))
paises$United_States <- str_detect(paises$TIKAB, regex(pattern = "\\bUnited States|\\bu\\.s\\.", ignore_case = T))
paises$Uruguay <- str_detect(paises$TIKAB, regex(pattern = "\\bUrugua", ignore_case = T))
paises$Uzbekistan <- str_detect(paises$TIKAB, regex(pattern = "\\bUzbekistan", ignore_case = T))
paises$Vanuatu <- str_detect(paises$TIKAB, regex(pattern = "\\bVanuatu", ignore_case = T))
paises$Vatican_City <- str_detect(paises$TIKAB, regex(pattern = "\\bVatican City", ignore_case = T))
paises$Venezuela <- str_detect(paises$TIKAB, regex(pattern = "\\bVenezuela", ignore_case = T))
paises$Vietnam <- str_detect(paises$TIKAB, regex(pattern = "\\bVietnam", ignore_case = T))
paises$Yemen <- str_detect(paises$TIKAB, regex(pattern = "\\bYemen", ignore_case = T))
paises$Zambia <- str_detect(paises$TIKAB, regex(pattern = "\\bZambia", ignore_case = T))
paises$Zimbabwe <- str_detect(paises$TIKAB, regex(pattern = "\\bZimbabwe", ignore_case = T))

paises <- paises %>% gather(key = "COUNTRY", value = "CHECK", Abkhazia:Zimbabwe) %>%
  filter(CHECK == 1)

a <- paises %>% group_by(COUNTRY) %>% summarise(n = n()) %>% arrange(-n) %>% 
  mutate(COUNTRY = gsub("_", "\n", COUNTRY),
         label = paste0(n, " (", percent(n/nrow(M), accuracy = .01), ")"))

ggplot(a %>% head(10), aes(reorder(COUNTRY, n), n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "tomato3") +
  geom_text(aes(label = label),
            vjust = -.5, size = 2.5) +
  labs(x = "País", y = "Qtd. Artigo")

setwd(here("Dados"))

ggsave("grafico 10.png", width = 16, height = 12, units = "cm", dpi = 300)

# quantidade de papers que estudam países

length(paises %>% select(CODE) %>% pull() %>% unique())

length(paises %>% select(CODE) %>% pull() %>% unique())/nrow(M)


# SEPARANDO A INFLUENCIA EXPERIMENTAL

D <- M %>% mutate(EXPERIMENT = str_detect(TIKAB, pattern = "EXPERIMENT|RANDOMIZED TRIAL|RCT")) %>% 
  filter(EXPERIMENT == 1) # NECESSÁRIO LIMPEZA MANUAL

# write.xlsx(D, "BASE EXPERIMENTAL.xlsx")

# limpeza manual feita e os seguintes artigos NÃO são experimentais

D <- D %>% filter(
  !CODE %in% c("ID 65",
               "ID 123",
               "ID 212",
               "ID 286",
               "ID 457",
               "ID 467",
               "ID 470",
               "ID 484",
               "ID 496",
               "ID 581",
               "ID 583",
               "ID 614",
               "ID 634",
               "ID 687",
               "ID 730",
               "ID 742",
               "ID 803",
               "ID 863",
               "ID 913",
               "ID 998",
               "ID 1268",
               "ID 1282",
               "ID 1372",
               "ID 1379",
               "ID 1393",
               "ID 1422",
               "ID 1439",
               "ID 1495",
               "ID 1503",
               "ID 1541",
               "ID 1549",
               "ID 1567",
               "ID 1568",
               "ID 1583",
               "ID 1588",
               "ID 1610",
               "ID 1679",
               "ID 1802",
               "ID 1832",
               "ID 1869",
               "ID 1886",
               "ID 1914",
               "ID 1928",
               "ID 1962",
               "ID 1998",
               "ID 2018",
               "ID 2025",
               "ID 2026",
               "ID 2039",
               "ID 2079",
               "ID 2099",
               "ID 2155",
               "ID 2157",
               "ID 2193",
               "ID 2214",
               "ID 2227",
               "ID 2228",
               "ID 2255",
               "ID 2265",
               "ID 2316",
               "ID 2331",
               "ID 2369",
               "ID 2379",
               "ID 2380",
               "ID 2537",
               "ID 2547",
               "ID 2549",
               "ID 2576",
               "ID 2587",
               "ID 2725",
               "ID 2865",
               "ID 2872",
               "ID 2895",
               "ID 3002",
               "ID 3068",
               "ID 3101",
               "ID 3141",
               "ID 3196",
               "ID 3221",
               "ID 3353",
               "ID 3434",
               "ID 3508",
               "ID 3545",
               "ID 3554",
               "ID 3557",
               "ID 3679",
               "ID 3685",
               "ID 3751",
               "ID 3801",
               "ID 3806",
               "ID 4034",
               "ID 4036",
               "ID 773",
               "ID 1098",
               "ID 1187",
               "ID 2407",
               "ID 2764")
)

nrow(D)
# 202 artigos experimentais

nrow(D)/nrow(M)

# POSICIONANDO AS LOGICAS DE CAUSAÇÃO

dp <- M


dp$QEXP<- str_detect(dp$TIKAB, regex(pattern = "PROPENSITY SCORE|REGRESSION DISCONTINUIT|RDD|QUANTILE REGRESSION|DIF-IN-DIF|DIFF-IN-DIFF|DIFFERENCE-IN-DIFFERENCE|DIFFERENCE IN DIFFERENCE|DOUBLE DIFFERENCE|INSTRUMENTAL VARIABLE|2SLS|SYNTHETIC CONTROL|INTERRUPTED TIME SERIES|INTERRUPTED TIME-SERIES|NATURAL EXPERIMENT", ignore_case = T))
dp$NMQ <- str_detect(dp$TIKAB, regex(pattern = "PROCESS TRACING|QCA|QUALITATIVE COMPARATIVE ANALYSIS|FUZZY SET|CRISP SET|COMPARATIVE HISTORICAL ANALYSIS|CASE STUDY|CASE STUDIES", ignore_case = T))
dp <- dp %>% mutate(EXP = ifelse(CODE %in% D$CODE, TRUE, FALSE))
dp <- dp %>% mutate(OUTROS = ifelse(QEXP == 0 &
                                      NMQ == 0 &
                                      EXP == 0, TRUE, FALSE))



QEXP <- dp %>% filter(QEXP == 1) %>% pull(CODE)
NMQ <- dp %>% filter(NMQ == 1) %>% pull(CODE)
REG <- dp %>% filter(OUTROS == 1) %>% pull(CODE)
EXP <- dp %>% filter(EXP == 1) %>% pull(CODE)

cores <- brewer.pal(4, "Pastel1")

# gráfico 11

setwd(here("Dados"))

venn.diagram(
  x = list(QEXP, NMQ, REG, EXP),
  category.names = c("QEXP" , "NMQ" , "OUTRO", "EXP"),
  filename = 'grafico 11.png',
  output=TRUE,
  height = 1417, 
  width = 1890, 
  resolution = 300,
  fill = cores
)


# parte empírica 2------

# D representa os 207 artigos experimentais

# distribuição por periódico

so <- D %>% group_by(SO) %>% 
  summarise(n = n()) %>% 
  mutate(prop = percent(n/sum(n), accuracy = 0.01)) %>% 
  left_join(FI)

cor.test(so$n, so$FI_5)

ggplot(so, aes(x = n, FI_5)) +
  geom_smooth(method = "lm", color = "grey", se = F) +
  geom_point() +
  geom_text_repel(aes(label = paste0(SO, " (", n, ", ", prop, ")")),
                  size = 3) +
  geom_text(label = "R = 0,75", x = 115, y = 2.2, size = 4) +
  labs(x = "Qtd. Artigos Experimentais", y = "Fator de Impacto")

setwd(here("Dados"))
ggsave("grafico 12.png", width = 16, height = 12, units = "cm", dpi = 300)


# evoluçãop

ev <- D %>% group_by(PY) %>% 
  summarise(n = n())


beta <- lm(n ~ PY, data = ev)

ggplot(ev, aes(x = PY, y = n)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = n), size = 3, vjust = -.5, data = ev %>% filter(!PY %in% c(2023, 2020))) +
  geom_text(aes(label = n), size = 3, vjust = 1.5, data = ev %>% filter(PY %in% c(2023, 2020))) +
  geom_text(label = paste0("β = ", round(beta$coefficients[2], 2)), x = 2023, y = 3) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(x = "Ano", y = "Qtd. Artigos Experimentais")

ggsave("grafico 13.png", width = 16, height = 12, units = "cm", dpi = 300)




# DISTRIBUIÇÃO DE IBI

# PRECISAREI FAZER UMA BASE NO SCOPUS BUSCANDO ARTIGOS POR ARTIGO

a <- D %>% select(CODE,TI, DI)

# nao encontrado: ID 227, ID 229, ID 287, ID 454, ID 539, ID 541, ID 1083,
# ID 1086, ID 1578, ID 2820

setwd(here("Dados", "scopus"))

D_SCOPUS <- convert2df("exp_comp.bib", dbsource = "scopus", format = "bibtex")

D_SCOPUS <- D_SCOPUS %>% rownames_to_column(var = "CODE") # codigo simplificado do scopus


# Os papers DAVIDSON MR, 2022, STUD COMP INT DEV, HECAN M, 2021, DEMOCRATIZATION, ADAMS BE, 2020, PUBLIUS, AHUJA A, 2016, STUD COMP INT DEV, CLAYTON A, 2015, COMP POLIT STUD
# não são experimentais

b <- D_SCOPUS %>% select(CODE, TI)

D_SCOPUS <- D_SCOPUS %>% filter(!CODE %in% c("DAVIDSON MR, 2022, STUD COMP INT DEV", 
                                             "HECAN M, 2021, DEMOCRATIZATION", 
                                             "ADAMS BE, 2020, PUBLIUS", 
                                             "AHUJA A, 2016, STUD COMP INT DEV", 
                                             "CLAYTON A, 2015, COMP POLIT STUD"))

D_SCOPUS <- D_SCOPUS %>% mutate(SO_FULL = SO,
                                SO = ifelse(SO == "COMPARATIVE POLITICS", "CPOL",
                                            ifelse(SO == "STUDIES IN COMPARATIVE INTERNATIONAL DEVELOPMENT", "SCID",
                                                   ifelse(SO == "DEMOCRATIZATION", "DEMZTION",
                                                          ifelse(SO == "WORLD POLITICS", "WPOL",
                                                                 ifelse(SO == "COMPARATIVE POLITICAL STUDIES", "CPS",
                                                                        ifelse(SO == "COMPARATIVE EUROPEAN POLITICS", "CEP",
                                                                               ifelse(SO == "PUBLIUS: THE JOURNAL OF FEDERALISM"|
                                                                                        SO == "PUBLIUS PUBLIUS" |
                                                                                        SO == "PUBLIUS", "PUBLIUS",
                                                                                      "ERROR"))))))))



D_SCOPUS <- D_SCOPUS %>% mutate(
  BEHAVIOR = str_count(CR, pattern = regex("BEHAVIOR|BEHAVIOUR|PSYCH", ignore_case = T)),
  INSTITUTION = str_count(CR, pattern = regex("INSTITUTION", ignore_case = T))
)


# IBI

D_SCOPUS$IBI <- log((1+D_SCOPUS$BEHAVIOR)/(1+D_SCOPUS$INSTITUTION)) # formula de IBI

# IDENTIFICANDO PAPERS QUE NÃO POSSUEM NENHUMA INFLUENCIA

MSCOPUS_IBI_0 <- D_SCOPUS %>% filter(BEHAVIOR == 0 & INSTITUTION == 0)

cat(nrow(MSCOPUS_IBI_0), "papers, em tese, não sofrem influenciam de nenhuma das lógicas")

# RETIRANDO-OS DA AMOSTRA

D_SCOPUS <- D_SCOPUS %>% filter(!CODE %in% MSCOPUS_IBI_0$CODE)


ggplot(D_SCOPUS) +
  geom_density(aes(x = IBI), fill = "tomato3", alpha = .5) +
  geom_density(aes(x = IBI), fill = "grey", alpha = .5, data = M_SCOPUS) +
  geom_vline(xintercept = 0, lty = "dashed", color = "red") +
  geom_text(aes(label = "Geral", x = -1, y = 0.3), size = 4) +
  geom_text(aes(label = "Experimental", x = 1, y = 0.3), size = 4) +
  labs(y = "Densidade")

setwd(here("Dados"))

ggsave("grafico 13.png", width = 16, height = 12, units = "cm", dpi = 300)


# descritivas

z <- data.frame(descr(M_SCOPUS$IBI)) # ESTATÍSTICA DESCRITIVA
z <- z %>% mutate(IBI = round(IBI,2)) %>% 
  rownames_to_column(var = "Métrica") %>% 
  filter(Métrica %in% c("Mean", "Std.Dev", "Min", "Median", "Max", "Skewness")) %>% 
  mutate(Métrica = case_when(Métrica == "Mean" ~ "Média",
                             Métrica == "Std.Dev" ~ "DP",
                             Métrica == "Median" ~ "Mediana",
                             TRUE ~ Métrica))



y <- data.frame(descr(D_SCOPUS$IBI)) # ESTATÍSTICA DESCRITIVA
y <- y %>% mutate(IBI = round(IBI,2)) %>% 
  rownames_to_column(var = "Métrica") %>% 
  filter(Métrica %in% c("Mean", "Std.Dev", "Min", "Median", "Max", "Skewness")) %>% 
  mutate(Métrica = case_when(Métrica == "Mean" ~ "Média",
                             Métrica == "Std.Dev" ~ "DP",
                             Métrica == "Median" ~ "Mediana",
                             TRUE ~ Métrica))

z <- z %>% left_join(y, by = "Métrica") %>% 
  rename(Geral = IBI.x,
         Experimental = IBI.y) %>% t()


colnames(z) <- z[1,]
z <- z[-1,]

write.xlsx(z, "tabela 4.xlsx")


# voltando para wos - 20 obras mais citadas

D_REF <- D %>% cSplit('CR', sep = ";", direction = "long")

cr_exp <- D_REF %>% group_by(CR, SO) %>%  summarise(n = n()) %>% 
  group_by(CR) %>% summarise(TC = sum(n),
                             IS = diversity(n, index = "shannon")) %>% 
  mutate(IS = round(IS/log(7), 2)) %>% arrange(-TC) #%>% filter(TC >= 10)

write.xlsx(cr_exp, "tabela 5.xlsx")

D_REF %>% filter(CR == "HAINMUELLER J, 2015, P NATL ACAD SCI USA, V112, P2395, DOI 10.1073/PNAS.1416587112") %>% 
  group_by(SO) %>% summarise(n = n())


# DICIONARIO

theme_cs <- D

theme_cs$ACCOUNTABILITY <- str_count(theme_cs$TIKAB, regex(pattern = "ACCOUNTABILIT", ignore_case = T))
theme_cs$AGENDA <- str_count(theme_cs$TIKAB, regex(pattern = "AGENDA", ignore_case = T))
theme_cs$AUTHORITARIANISM <- str_count(theme_cs$TIKAB, regex(pattern = "AUTHORITARIAN", ignore_case = T))
theme_cs$BUDGET <- str_count(theme_cs$TIKAB, regex(pattern = "BUDGET", ignore_case = T))
theme_cs$BUREAUCRACY <- str_count(theme_cs$TIKAB, regex(pattern = "BUREAUCRA|AUTHORIT|AGENCY|AGENCIES", ignore_case = T))
theme_cs$CAMPAIGN <- str_count(theme_cs$TIKAB, regex(pattern = "CAMPAIGN", ignore_case = T))
theme_cs$CITIZENSHIP <- str_count(theme_cs$TIKAB, regex(pattern = "CITIZEN", ignore_case = T))
theme_cs$CIVIL_SOCIETY <- str_count(theme_cs$TIKAB, regex(pattern = "CIVIL SOCIET|COMMUNIT|SOCIAL CAPITAL", ignore_case = T))
theme_cs$CLIENTELISM <- str_count(theme_cs$TIKAB, regex(pattern = "CLIENTELIS|PATRONAGE|BROKER|PORK BARREL|POLITICAL MACHINE", ignore_case = T))
theme_cs$COALITION <- str_count(theme_cs$TIKAB, regex(pattern = "COALITION", ignore_case = T))
theme_cs$COMPETITION <- str_count(theme_cs$TIKAB, regex(pattern = "POLITICAL COMPETITION|ELECTORAL COMPETITION|CLOSE RACE|POLITICAL MARKET|INCUMBENT|INCUMBENC", ignore_case = T))
theme_cs$CONFLICT <- str_count(theme_cs$TIKAB, regex(pattern = "CONFLICT", ignore_case = T))
theme_cs$CONSTITUTION <- str_count(theme_cs$TIKAB, regex(pattern = "CONSTITUTION", ignore_case = T))
theme_cs$CORRUPTION <- str_count(theme_cs$TIKAB, regex(pattern = "CORRUP", ignore_case = T))
theme_cs$CRISIS <- str_count(theme_cs$TIKAB, regex(pattern = "CRISIS|CRISES", ignore_case = T))
theme_cs$DECENTRALIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "DECENTRALIZATION|CENTRALIZATION", ignore_case = T))
theme_cs$DECISION_MAKING <- str_count(theme_cs$TIKAB, regex(pattern = "DECISION MAKING|DECISION-MAKING", ignore_case = T))
theme_cs$DELIBERATION <- str_count(theme_cs$TIKAB, regex(pattern = "DELIBERAT", ignore_case = T))
theme_cs$DEMOCRACY <- str_count(theme_cs$TIKAB, regex(pattern = "DEMOCRAC", ignore_case = T))
theme_cs$DEMOCRATIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "DEMOCRATIZATION", ignore_case = T))
theme_cs$ELECTION <- str_count(theme_cs$TIKAB, regex(pattern = "ELECT", ignore_case = T))
theme_cs$ELITES <- str_count(theme_cs$TIKAB, regex(pattern = "ELITE", ignore_case = T))
theme_cs$EMPOWERMENT <- str_count(theme_cs$TIKAB, regex(pattern = "EMPOWERMENT", ignore_case = T))
theme_cs$EQUALITY <- str_count(theme_cs$TIKAB, regex(pattern = "EQUALITY|EQUITY|INEQUALITY", ignore_case = T))
theme_cs$ETHNIC <- str_count(theme_cs$TIKAB, regex(pattern = "ETHNIC", ignore_case = T))
theme_cs$EXECUTIVE <- str_count(theme_cs$TIKAB, regex(pattern = "EXECUTIVE", ignore_case = T))
theme_cs$FEDERALISM <- str_count(theme_cs$TIKAB, regex(pattern = "FEDERALISM|MULTILEVEL GOV", ignore_case = T))
theme_cs$FISCAL <- str_count(theme_cs$TIKAB, regex(pattern = "FISCAL|EXPENDITURE|SPENDING|DEBT", ignore_case = T))
theme_cs$GENDER_WOMEN <- str_count(theme_cs$TIKAB, regex(pattern = "GENDER|WOMAN|WOMEN|FEMINISM", ignore_case = T))
theme_cs$GLOBALIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "GLOBALIZATION", ignore_case = T))
theme_cs$GOVERNANCE <- str_count(theme_cs$TIKAB, regex(pattern = "GOVERNANCE", ignore_case = T))
theme_cs$IDENTITY <- str_count(theme_cs$TIKAB, regex(pattern = "IDENTIT", ignore_case = T))
theme_cs$IDEOLOGY <- str_count(theme_cs$TIKAB, regex(pattern = "IDEOLOG", ignore_case = T))
theme_cs$INEQUALITY <- str_count(theme_cs$TIKAB, regex(pattern = "INEQUALIT", ignore_case = T))
theme_cs$INFORMAL <- str_count(theme_cs$TIKAB, regex(pattern = "INFORMAL", ignore_case = T))
theme_cs$INTEREST_GROUPS <- str_count(theme_cs$TIKAB, regex(pattern = "INTEREST GROUP|LOBBY", ignore_case = T))
theme_cs$INTERNATIONAL_RELATIONS <- str_count(theme_cs$TIKAB, regex(pattern = "INTERNATIONAL RELATIONS|DIPLOMAC|FOREIGN POLIC|REGIONALISM|INTERNATIONAL COOPERATION", ignore_case = T))
theme_cs$JUDICIARY <- str_count(theme_cs$TIKAB, regex(pattern = "JUDICIARY|JUSTICE|COURT", ignore_case = T))
theme_cs$LEADER <- str_count(theme_cs$TIKAB, regex(pattern = "LEADER", ignore_case = T))
theme_cs$LEGISLATIVE <- str_count(theme_cs$TIKAB, regex(pattern = "LEGISLAT|COMMITTEE|PARLIAMENT|SENATE|DEPUTY", ignore_case = T))
theme_cs$LOCAL_GOVERNMENT <- str_count(theme_cs$TIKAB, regex(pattern = "LOCAL|MUNICIPAL|SUBNATIONAL|MAYOR|GOVERNOR", ignore_case = T))
theme_cs$MILITARY_SECURITY <- str_count(theme_cs$TIKAB, regex(pattern = "MILITARY|SECURITY", ignore_case = T))
theme_cs$NETWORK <- str_count(theme_cs$TIKAB, regex(pattern = "NETWORK", ignore_case = T))
theme_cs$PARTICIPATION <- str_count(theme_cs$TIKAB, regex(pattern = "PARTICIPATION", ignore_case = T))
theme_cs$PARTY <- str_count(theme_cs$TIKAB, regex(pattern = "PARTY|PARTIES|PARTISAN", ignore_case = T))
theme_cs$POLARIZATION <- str_count(theme_cs$TIKAB, regex(pattern = "POLARIZATION|POLARIZAÇÃO|POLARIZACAO", ignore_case = T))
theme_cs$POLICE <- str_count(theme_cs$TIKAB, regex(pattern = "POLICE|POLICIA|POLÍCIA", ignore_case = T))
theme_cs$POLICY <- str_count(theme_cs$TIKAB, regex(pattern = "POLICY|POLICIES|WELFARE STATE", ignore_case = T))
theme_cs$POPULISM <- str_count(theme_cs$TIKAB, regex(pattern = "POPULISM", ignore_case = T))
theme_cs$PRESIDENT <- str_count(theme_cs$TIKAB, regex(pattern = "PRESIDENT", ignore_case = T))
theme_cs$PUBLIC_OPINION <- str_count(theme_cs$TIKAB, regex(pattern = "PUBLIC OPINION|COMMUNICATION|MEDIA|JOURNALIS", ignore_case = T))
theme_cs$RACE <- str_count(theme_cs$TIKAB, regex(pattern = "RACE", ignore_case = T))
theme_cs$REFORM <- str_count(theme_cs$TIKAB, regex(pattern = "REFORM", ignore_case = T))
theme_cs$REGIME <- str_count(theme_cs$TIKAB, regex(pattern = "REGIME", ignore_case = T))
theme_cs$REGIME_CHANGE <- str_count(theme_cs$TIKAB, regex(pattern = "REGIME CHANGE|TRANSITION|BREAKDOWN", ignore_case = T))
theme_cs$REGULATION <- str_count(theme_cs$TIKAB, regex(pattern = "REGULAT", ignore_case = T))
theme_cs$RELIGION <- str_count(theme_cs$TIKAB, regex(pattern = "RELIGION", ignore_case = T))
theme_cs$REPRESENTATION <- str_count(theme_cs$TIKAB, regex(pattern = "REPRESENTATION", ignore_case = T))
theme_cs$STABILITY <- str_count(theme_cs$TIKAB, regex(pattern = "STABILITY", ignore_case = T))
theme_cs$STATE <- str_count(theme_cs$TIKAB, regex(pattern = "STATE", ignore_case = T))
theme_cs$TRADE <- str_count(theme_cs$TIKAB, regex(pattern = "TRADE", ignore_case = T))
theme_cs$TRUST <- str_count(theme_cs$TIKAB, regex(pattern = "TRUST", ignore_case = T))
theme_cs$VIOLENCE <- str_count(theme_cs$TIKAB, regex(pattern = "VIOLENCE|REPRESSION", ignore_case = T))
theme_cs$VOTE <- str_count(theme_cs$TIKAB, regex(pattern = "VOTE|TURNOUT|BALLOT", ignore_case = T))
theme_cs$WAR <- str_count(theme_cs$TIKAB, regex(pattern = "\\bWAR\\b ", ignore_case = T))
theme_cs$MIGRATION <- str_count(theme_cs$TIKAB, regex(pattern = "MIGRATION ", ignore_case = T))
theme_cs$PUBLIC_SECTOR <- str_count(theme_cs$TIKAB, regex(pattern = "PUBLIC SECTOR|PUBLIC EMPLOY|PUBLIC ADMINISTRATION|PUBLIC MANAGEMENT|PUBLIC FINANCE", ignore_case = T))
theme_cs$CRIME <- str_count(theme_cs$TIKAB, regex(pattern = "CRIME|DRUG|MAFIA|CRIMINAL|MILITIA", ignore_case = T))

THEO_TYPE <- theme_cs %>%
  gather(key = TERMO, value = CHECK, ACCOUNTABILITY:CRIME) %>% 
  filter(CHECK != 0)


contagem <- THEO_TYPE %>% group_by(TERMO) %>% summarise(n = sum(CHECK),
                                                        paper = n_distinct(CODE)) %>% 
  mutate(prop = paper/nrow(M),
         FIT = round(n*prop, 2),
         TERMO = gsub("_", " ", TERMO), 
         TERMO = str_to_title(TERMO)) %>% 
  arrange(-FIT)

print(contagem, n = 15)

ggplot(contagem %>% head(15) %>% 
         mutate(label = ifelse(TERMO %in% c("Election", "Vote", "Citizenship", "Public Opinion", "Gender Women", "Clientelism", "Ethnic", "Identity"), 
                               "Comportamental", "Institucional")), aes(x = reorder(TERMO, FIT), y = FIT)) +
  geom_segment(aes(xend = TERMO,
                   y = 0, yend = FIT), color = "grey") +
  geom_point(size = 3, aes(color = label)) +
  geom_text(aes(label = FIT), hjust = -.5, size = 3) +
  coord_flip() +
  scale_color_manual(values = c("Comportamental" = "tomato3",
                                "Institucional" = "cornflowerblue")) +
  expand_limits(y = c(0, 6.6)) +
  labs(x = "Termo", color = "")


setwd(here("Dados"))
ggsave("grafico 14.png", width = 16, height = 10, units = "cm", dpi = 300)


# NMF DOS PAPERS EXPERIMENTAIS

D_EXP_NMF <- D %>% select(CODE, TIKAB)


D_EXP_NMF <- D_EXP_NMF %>% mutate(TIKAB = tolower(TIKAB),
                                  TIKAB = removeNumbers(TIKAB),
                                  TIKAB = removePunctuation(TIKAB),
                                  TIKAB = removeWords(TIKAB, stopwords("english")),
                                  TIKAB = stripWhitespace(TIKAB))

D_EXP_NMF <- D_EXP_NMF %>% cSplit('TIKAB', sep = " ", direction = "long")

D_EXP_NMF <- D_EXP_NMF %>% group_by(CODE, TIKAB) %>% summarise(qty = n()) %>% 
  spread(CODE, qty)

D_EXP_NMF <- D_EXP_NMF %>% filter(is.na(TIKAB) == F)


D_EXP_NMF[is.na(D_EXP_NMF)] <- 0


D_EXP_NMF <- D_EXP_NMF %>% filter(is.na(TIKAB) == F) %>% 
  column_to_rownames(var = "TIKAB")


set.seed(123)

res <- nmf(D_EXP_NMF, 10, "lee") 

citation("NMF")

w <- basis(res) 
dim(w)

options(scipen = 999)

df <- as.data.frame(w)

df1 <- df %>% mutate(word = rownames(df)) 

setwd(here("Dados"))
write.xlsx(df1, "tabela 6_1.xlsx")


# analise fatorial exploratoria

theme_cs <- D

theme_cs$AUTHORITARIANISM <- str_count(theme_cs$TIKAB, regex(pattern = "AUTHORITARIAN", ignore_case = T))
theme_cs$BUREAUCRACY <- str_count(theme_cs$TIKAB, regex(pattern = "BUREAUCRA|AUTHORIT|AGENCY|AGENCIES", ignore_case = T))
theme_cs$CITIZENSHIP <- str_count(theme_cs$TIKAB, regex(pattern = "CITIZEN", ignore_case = T))
theme_cs$CLIENTELISM <- str_count(theme_cs$TIKAB, regex(pattern = "CLIENTELIS|PATRONAGE|BROKER|PORK BARREL|POLITICAL MACHINE", ignore_case = T))
theme_cs$DEMOCRACY <- str_count(theme_cs$TIKAB, regex(pattern = "DEMOCRAC", ignore_case = T))
theme_cs$ELECTION <- str_count(theme_cs$TIKAB, regex(pattern = "ELECT", ignore_case = T))
theme_cs$ETHNIC <- str_count(theme_cs$TIKAB, regex(pattern = "ETHNIC", ignore_case = T))
theme_cs$GENDER_WOMEN <- str_count(theme_cs$TIKAB, regex(pattern = "GENDER|WOMAN|WOMEN|FEMINISM", ignore_case = T))
theme_cs$LOCAL_GOVERNMENT <- str_count(theme_cs$TIKAB, regex(pattern = "LOCAL|MUNICIPAL|SUBNATIONAL|MAYOR|GOVERNOR", ignore_case = T))
theme_cs$PARTY <- str_count(theme_cs$TIKAB, regex(pattern = "PARTY|PARTIES|PARTISAN", ignore_case = T))
theme_cs$POLICY <- str_count(theme_cs$TIKAB, regex(pattern = "POLICY|POLICIES|WELFARE STATE", ignore_case = T))
theme_cs$PUBLIC_OPINION <- str_count(theme_cs$TIKAB, regex(pattern = "PUBLIC OPINION|COMMUNICATION|MEDIA|JOURNALIS", ignore_case = T))
theme_cs$VOTE <- str_count(theme_cs$TIKAB, regex(pattern = "VOTE|TURNOUT|BALLOT", ignore_case = T))
theme_cs$STATE <- str_count(theme_cs$TIKAB, regex(pattern = "STATE", ignore_case = T))
theme_cs$REGIME <- str_count(theme_cs$TIKAB, regex(pattern = "REGIME", ignore_case = T))
theme_cs$SURVEY <- str_count(theme_cs$TIKAB, regex(pattern = "SURVEY", ignore_case = T))
theme_cs$FIELD <- str_count(theme_cs$TIKAB, regex(pattern = "FIELD", ignore_case = T))
theme_cs$LAB <- str_count(theme_cs$TIKAB, regex(pattern = "LAB", ignore_case = T))

theme_cs <- theme_cs %>% select(CODE, AUTHORITARIANISM:LAB) %>% 
  column_to_rownames(var = "CODE")

a <- cor(theme_cs)

a <- as.matrix(as.dist(max(a) - a))

mds2d <- metaMDS(a, k = 2, trymax = 1000)
mds2d

df <- scores(mds2d) %>% as.data.frame() %>% rownames_to_column(var = "country")

ggplot(df, aes(x = NMDS1, y = NMDS2)) +
  geom_text(aes(label = country)) +
  theme_bw()


# paises estudados

paises <- D


paises$Abkhazia <- str_detect(paises$TIKAB, regex(pattern = "\\bAbkhazia", ignore_case = T))
paises$Afghanistan <- str_detect(paises$TIKAB, regex(pattern = "\\bAfghanistan", ignore_case = T))
paises$Albania <- str_detect(paises$TIKAB, regex(pattern = "\\bAlbania", ignore_case = T))
paises$Algeria <- str_detect(paises$TIKAB, regex(pattern = "\\bAlgeria", ignore_case = T))
paises$Andorra <- str_detect(paises$TIKAB, regex(pattern = "\\bAndorra", ignore_case = T))
paises$Angola <- str_detect(paises$TIKAB, regex(pattern = "\\bAngola", ignore_case = T))
paises$Antigua_and_Barbuda <- str_detect(paises$TIKAB, regex(pattern = "\\bAntigua and Barbuda", ignore_case = T))
paises$Argentina <- str_detect(paises$TIKAB, regex(pattern = "\\bArgentin", ignore_case = T))
paises$Armenia <- str_detect(paises$TIKAB, regex(pattern = "\\bArmenia", ignore_case = T))
paises$Australia <- str_detect(paises$TIKAB, regex(pattern = "\\bAustralia", ignore_case = T))
paises$Austria <- str_detect(paises$TIKAB, regex(pattern = "\\bAustria", ignore_case = T))
paises$Azerbaijan <- str_detect(paises$TIKAB, regex(pattern = "\\bAzerbaijan", ignore_case = T))
paises$Bahamas <- str_detect(paises$TIKAB, regex(pattern = "\\bBahamas", ignore_case = T))
paises$Bahrain <- str_detect(paises$TIKAB, regex(pattern = "\\bBahrain", ignore_case = T))
paises$Bangladesh <- str_detect(paises$TIKAB, regex(pattern = "\\bBangladesh", ignore_case = T))
paises$Barbados <- str_detect(paises$TIKAB, regex(pattern = "\\bBarbados", ignore_case = T))
paises$Belarus <- str_detect(paises$TIKAB, regex(pattern = "\\bBelarus", ignore_case = T))
paises$Belgium <- str_detect(paises$TIKAB, regex(pattern = "\\bBelgi", ignore_case = T))
paises$Belize <- str_detect(paises$TIKAB, regex(pattern = "\\bBelize", ignore_case = T))
paises$Benin <- str_detect(paises$TIKAB, regex(pattern = "\\bBenin", ignore_case = T))
paises$Bhutan <- str_detect(paises$TIKAB, regex(pattern = "\\bBhutan", ignore_case = T))
paises$Bolivia <- str_detect(paises$TIKAB, regex(pattern = "\\bBolivia", ignore_case = T))
paises$Bosnia_and_Herzegovina <- str_detect(paises$TIKAB, regex(pattern = "\\bBosnia and Herzegovina", ignore_case = T))
paises$Botswana <- str_detect(paises$TIKAB, regex(pattern = "\\bBotswana", ignore_case = T))
paises$Brazil <- str_detect(paises$TIKAB, regex(pattern = "\\bBrazil", ignore_case = T))
paises$Brunei <- str_detect(paises$TIKAB, regex(pattern = "\\bBrunei", ignore_case = T))
paises$Bulgaria <- str_detect(paises$TIKAB, regex(pattern = "\\bBulgaria", ignore_case = T))
paises$Burkina_Faso <- str_detect(paises$TIKAB, regex(pattern = "\\bBurkina Faso", ignore_case = T))
paises$Burundi <- str_detect(paises$TIKAB, regex(pattern = "\\bBurundi", ignore_case = T))
paises$Cambodia <- str_detect(paises$TIKAB, regex(pattern = "\\bCambodia", ignore_case = T))
paises$Cameroon <- str_detect(paises$TIKAB, regex(pattern = "\\bCameroon", ignore_case = T))
paises$Canada <- str_detect(paises$TIKAB, regex(pattern = "\\bCanad", ignore_case = T))
paises$Cape_Verde <- str_detect(paises$TIKAB, regex(pattern = "\\bCape Verde", ignore_case = T))
paises$Central_African_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bCentral African Republic", ignore_case = T))
paises$Chad <- str_detect(paises$TIKAB, regex(pattern = "\\bChad", ignore_case = T))
paises$Chile <- str_detect(paises$TIKAB, regex(pattern = "\\bChile", ignore_case = T))
paises$China <- str_detect(paises$TIKAB, regex(pattern = "\\bChina|\\bChinese", ignore_case = T))
paises$Colombia <- str_detect(paises$TIKAB, regex(pattern = "\\bColombia", ignore_case = T))
paises$Comoros <- str_detect(paises$TIKAB, regex(pattern = "\\bComoros", ignore_case = T))
paises$Congo <- str_detect(paises$TIKAB, regex(pattern = "\\bCongo", ignore_case = T))
paises$Cook_Islands <- str_detect(paises$TIKAB, regex(pattern = "\\bCook Islands", ignore_case = T))
paises$Costa_Rica <- str_detect(paises$TIKAB, regex(pattern = "\\bCosta Rica", ignore_case = T))
paises$Cote_divoire <- str_detect(paises$TIKAB, regex(pattern = "\\bCôte d'Ivoire|\\bCote divoire|\\bcote d ivoire|\\bcote d'ivoire", ignore_case = T))
paises$Croatia <- str_detect(paises$TIKAB, regex(pattern = "\\bCroatia", ignore_case = T))
paises$Cuba <- str_detect(paises$TIKAB, regex(pattern = "\\bCuba", ignore_case = T))
paises$Cyprus <- str_detect(paises$TIKAB, regex(pattern = "\\bCyprus", ignore_case = T))
paises$Czech_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bCzech Republic", ignore_case = T))
paises$Denmark <- str_detect(paises$TIKAB, regex(pattern = "\\bDenmark|Danish", ignore_case = T))
paises$Djibouti <- str_detect(paises$TIKAB, regex(pattern = "\\bDjibouti", ignore_case = T))
paises$Dominica <- str_detect(paises$TIKAB, regex(pattern = "\\bDominica ", ignore_case = T))
paises$Dominican_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bDominican Republic", ignore_case = T))
paises$East_Timor <- str_detect(paises$TIKAB, regex(pattern = "\\bEast Timor", ignore_case = T))
paises$Ecuador <- str_detect(paises$TIKAB, regex(pattern = "\\bEcuador", ignore_case = T))
paises$Egypt <- str_detect(paises$TIKAB, regex(pattern = "\\bEgyp", ignore_case = T))
paises$El_Salvador <- str_detect(paises$TIKAB, regex(pattern = "\\bEl Salvador", ignore_case = T))
paises$Equatorial_Guinea <- str_detect(paises$TIKAB, regex(pattern = "\\bEquatorial Guinea", ignore_case = T))
paises$Eritrea <- str_detect(paises$TIKAB, regex(pattern = "\\bEritrea", ignore_case = T))
paises$Estonia <- str_detect(paises$TIKAB, regex(pattern = "\\bEstonia", ignore_case = T))
paises$Ethiopia <- str_detect(paises$TIKAB, regex(pattern = "\\bEthiopia", ignore_case = T))
paises$Fiji <- str_detect(paises$TIKAB, regex(pattern = "\\bFiji", ignore_case = T))
paises$Finland <- str_detect(paises$TIKAB, regex(pattern = "\\bFinland", ignore_case = T))
paises$France <- str_detect(paises$TIKAB, regex(pattern = "\\bFrance|\\bFrench", ignore_case = T))
paises$Gabon <- str_detect(paises$TIKAB, regex(pattern = "\\bGabon", ignore_case = T))
paises$Gambia <- str_detect(paises$TIKAB, regex(pattern = "\\bGambia", ignore_case = T))
paises$Georgia <- str_detect(paises$TIKAB, regex(pattern = "\\bGeorgia", ignore_case = T))
paises$Germany <- str_detect(paises$TIKAB, regex(pattern = "\\bGerman", ignore_case = T))
paises$Ghana <- str_detect(paises$TIKAB, regex(pattern = "\\bGhana", ignore_case = T))
paises$Greece <- str_detect(paises$TIKAB, regex(pattern = "\\bGreece|\\bGreek", ignore_case = T))
paises$Grenada <- str_detect(paises$TIKAB, regex(pattern = "\\bGrenada", ignore_case = T))
paises$Guatemala <- str_detect(paises$TIKAB, regex(pattern = "\\bGuatemala", ignore_case = T))
paises$Guinea <- str_detect(paises$TIKAB, regex(pattern = "\\bGuinea", ignore_case = T))
paises$Guinea_Bissau <- str_detect(paises$TIKAB, regex(pattern = "\\bGuinea-Bissau|\\bGuinea Bissau", ignore_case = T))
paises$Guyana <- str_detect(paises$TIKAB, regex(pattern = "\\bGuyana", ignore_case = T))
paises$Haiti <- str_detect(paises$TIKAB, regex(pattern = "\\bHaiti", ignore_case = T))
paises$Honduras <- str_detect(paises$TIKAB, regex(pattern = "\\bHondura", ignore_case = T))
paises$Hungary <- str_detect(paises$TIKAB, regex(pattern = "\\bHungar", ignore_case = T))
paises$Iceland <- str_detect(paises$TIKAB, regex(pattern = "\\bIceland", ignore_case = T))
paises$India <- str_detect(paises$TIKAB, regex(pattern = "\\bIndia", ignore_case = T))
paises$Indonesia <- str_detect(paises$TIKAB, regex(pattern = "\\bIndonesia", ignore_case = T))
paises$Iran <- str_detect(paises$TIKAB, regex(pattern = "\\bIran", ignore_case = T))
paises$Iraq <- str_detect(paises$TIKAB, regex(pattern = "\\bIraq", ignore_case = T))
paises$Ireland <- str_detect(paises$TIKAB, regex(pattern = "\\bIreland", ignore_case = T))
paises$Israel <- str_detect(paises$TIKAB, regex(pattern = "\\bIsrael", ignore_case = T))
paises$Italy <- str_detect(paises$TIKAB, regex(pattern = "\\bItaly|\\bitalian", ignore_case = T))
paises$Ivory_Coast <- str_detect(paises$TIKAB, regex(pattern = "\\bIvory Coast", ignore_case = T))
paises$Jamaica <- str_detect(paises$TIKAB, regex(pattern = "\\bJamaica", ignore_case = T))
paises$Japan <- str_detect(paises$TIKAB, regex(pattern = "\\bJapan", ignore_case = T))
paises$Jordan <- str_detect(paises$TIKAB, regex(pattern = "\\bJordan", ignore_case = T))
paises$Kazakhstan <- str_detect(paises$TIKAB, regex(pattern = "\\bKazakhstan", ignore_case = T))
paises$Kenya <- str_detect(paises$TIKAB, regex(pattern = "\\bKenya", ignore_case = T))
paises$Kiribati <- str_detect(paises$TIKAB, regex(pattern = "\\bKiribati", ignore_case = T))
paises$Kosovo <- str_detect(paises$TIKAB, regex(pattern = "\\bKosovo", ignore_case = T))
paises$Kuwait <- str_detect(paises$TIKAB, regex(pattern = "\\bKuwait", ignore_case = T))
paises$Kyrgyzstan <- str_detect(paises$TIKAB, regex(pattern = "\\bKyrgyzstan", ignore_case = T))
paises$Laos <- str_detect(paises$TIKAB, regex(pattern = "\\bLaos", ignore_case = T))
paises$Latvia <- str_detect(paises$TIKAB, regex(pattern = "\\bLatvia", ignore_case = T))
paises$Lebanon <- str_detect(paises$TIKAB, regex(pattern = "\\bLebanon", ignore_case = T))
paises$Lesotho <- str_detect(paises$TIKAB, regex(pattern = "\\bLesotho", ignore_case = T))
paises$Liberia <- str_detect(paises$TIKAB, regex(pattern = "\\bLiberia", ignore_case = T))
paises$Libya <- str_detect(paises$TIKAB, regex(pattern = "\\bLibya", ignore_case = T))
paises$Liechtenstein <- str_detect(paises$TIKAB, regex(pattern = "\\bLiechtenstein", ignore_case = T))
paises$Lithuania <- str_detect(paises$TIKAB, regex(pattern = "\\bLithuania", ignore_case = T))
paises$Luxembourg <- str_detect(paises$TIKAB, regex(pattern = "\\bLuxembourg", ignore_case = T))
paises$Macedonia <- str_detect(paises$TIKAB, regex(pattern = "\\bMacedonia", ignore_case = T))
paises$Madagascar <- str_detect(paises$TIKAB, regex(pattern = "\\bMadagascar", ignore_case = T))
paises$Malawi <- str_detect(paises$TIKAB, regex(pattern = "\\bMalawi", ignore_case = T))
paises$Malaysia <- str_detect(paises$TIKAB, regex(pattern = "\\bMalaysia", ignore_case = T))
paises$Maldives <- str_detect(paises$TIKAB, regex(pattern = "\\bMaldives", ignore_case = T))
paises$Mali <- str_detect(paises$TIKAB, regex(pattern = " \\bMali", ignore_case = T))
paises$Malta <- str_detect(paises$TIKAB, regex(pattern = "\\bMalta", ignore_case = T))
paises$Marshall_Islands <- str_detect(paises$TIKAB, regex(pattern = "\\bMarshall Islands", ignore_case = T))
paises$Mauritania <- str_detect(paises$TIKAB, regex(pattern = "\\bMauritania", ignore_case = T))
paises$Mauritius <- str_detect(paises$TIKAB, regex(pattern = "\\bMauritius", ignore_case = T))
paises$Mexico <- str_detect(paises$TIKAB, regex(pattern = "\\bMexic", ignore_case = T))
paises$Micronesia <- str_detect(paises$TIKAB, regex(pattern = "\\bMicronesia", ignore_case = T))
paises$Moldova <- str_detect(paises$TIKAB, regex(pattern = "\\bMoldova", ignore_case = T))
paises$Monaco <- str_detect(paises$TIKAB, regex(pattern = "\\bMonaco", ignore_case = T))
paises$Mongolia <- str_detect(paises$TIKAB, regex(pattern = "\\bMongolia", ignore_case = T))
paises$Montenegro <- str_detect(paises$TIKAB, regex(pattern = "\\bMontenegro", ignore_case = T))
paises$Morocco <- str_detect(paises$TIKAB, regex(pattern = "\\bMorocco", ignore_case = T))
paises$Mozambique <- str_detect(paises$TIKAB, regex(pattern = "\\bMozambique", ignore_case = T))
paises$Myanmar <- str_detect(paises$TIKAB, regex(pattern = "\\bMyanmar|Burma", ignore_case = T))
paises$Nagorno_Karabakh <- str_detect(paises$TIKAB, regex(pattern = "\\bNagorno-Karabakh|\\bNagorno Karabakh", ignore_case = T))
paises$Namibia <- str_detect(paises$TIKAB, regex(pattern = "\\bNamibia", ignore_case = T))
paises$Nauru <- str_detect(paises$TIKAB, regex(pattern = "\\bNauru", ignore_case = T))
paises$Nepal <- str_detect(paises$TIKAB, regex(pattern = "\\bNepal", ignore_case = T))
paises$Netherlands <- str_detect(paises$TIKAB, regex(pattern = "\\bNetherlands", ignore_case = T))
paises$New_Zealand <- str_detect(paises$TIKAB, regex(pattern = "\\bNew Zealand", ignore_case = T))
paises$Nicaragua <- str_detect(paises$TIKAB, regex(pattern = "\\bNicaragua", ignore_case = T))
paises$Niger <- str_detect(paises$TIKAB, regex(pattern = "\\bNiger ", ignore_case = T))
paises$Nigeria <- str_detect(paises$TIKAB, regex(pattern = "\\bNigeria", ignore_case = T))
paises$Niue <- str_detect(paises$TIKAB, regex(pattern = "\\bNiue", ignore_case = T))
paises$North_Korea <- str_detect(paises$TIKAB, regex(pattern = "\\bNorth Korea", ignore_case = T))
paises$Norway <- str_detect(paises$TIKAB, regex(pattern = "\\bNorway|\\bNorwegian", ignore_case = T))
paises$Oman <- str_detect(paises$TIKAB, regex(pattern = "\\bOman", ignore_case = T))
paises$Pakistan <- str_detect(paises$TIKAB, regex(pattern = "\\bPakistan", ignore_case = T))
paises$Palau <- str_detect(paises$TIKAB, regex(pattern = "\\bPalau", ignore_case = T))
paises$Palestine <- str_detect(paises$TIKAB, regex(pattern = "\\bPalestin", ignore_case = T))
paises$Panama <- str_detect(paises$TIKAB, regex(pattern = "\\bPanam", ignore_case = T))
paises$Papua_New_Guinea <- str_detect(paises$TIKAB, regex(pattern = "\\bPapua New Guinea", ignore_case = T))
paises$Paraguay <- str_detect(paises$TIKAB, regex(pattern = "\\bParagu", ignore_case = T))
paises$Peru <- str_detect(paises$TIKAB, regex(pattern = "\\bPeru", ignore_case = T))
paises$Philippines <- str_detect(paises$TIKAB, regex(pattern = "\\bPhilippin", ignore_case = T))
paises$Poland <- str_detect(paises$TIKAB, regex(pattern = "\\bPoland|Polish", ignore_case = T))
paises$Portugal <- str_detect(paises$TIKAB, regex(pattern = "\\bPortug", ignore_case = T))
paises$Qatar <- str_detect(paises$TIKAB, regex(pattern = "\\bQatar", ignore_case = T))
paises$Romania <- str_detect(paises$TIKAB, regex(pattern = "\\bRomania", ignore_case = T))
paises$Russia <- str_detect(paises$TIKAB, regex(pattern = "\\bRussia", ignore_case = T))
paises$Rwanda <- str_detect(paises$TIKAB, regex(pattern = "\\bRwanda", ignore_case = T))
paises$Sahrawi_Arab_Democratic_Republic <- str_detect(paises$TIKAB, regex(pattern = "\\bSahrawi|\\bArab Democratic Republic", ignore_case = T))
paises$Saint_Kitts_and_Nevis <- str_detect(paises$TIKAB, regex(pattern = "\\bSaint Kitts and Nevis", ignore_case = T))
paises$Saint_Lucia <- str_detect(paises$TIKAB, regex(pattern = "\\bSaint Lucia", ignore_case = T))
paises$Saint_Vincent_and_the_Grenadines <- str_detect(paises$TIKAB, regex(pattern = "\\bSaint Vincent and the Grenadines", ignore_case = T))
paises$Samoa <- str_detect(paises$TIKAB, regex(pattern = "\\bSamoa", ignore_case = T))
paises$San_Marino <- str_detect(paises$TIKAB, regex(pattern = "\\bSan Marino", ignore_case = T))
paises$Sao_Tome_and_Principe <- str_detect(paises$TIKAB, regex(pattern = "\\bSão Tomé and Príncipe|\\bSao Tome and Principe", ignore_case = T))
paises$Saudi_Arabia <- str_detect(paises$TIKAB, regex(pattern = "\\bSaudi Arabia", ignore_case = T))
paises$Senegal <- str_detect(paises$TIKAB, regex(pattern = "\\bSenegal", ignore_case = T))
paises$Serbia <- str_detect(paises$TIKAB, regex(pattern = "\\bSerbia", ignore_case = T))
paises$Seychelles <- str_detect(paises$TIKAB, regex(pattern = "\\bSeychelles", ignore_case = T))
paises$Sierra_Leone <- str_detect(paises$TIKAB, regex(pattern = "\\bSierra Leone", ignore_case = T))
paises$Singapore <- str_detect(paises$TIKAB, regex(pattern = "\\bSingapor", ignore_case = T))
paises$Slovakia <- str_detect(paises$TIKAB, regex(pattern = "\\bSlovak", ignore_case = T))
paises$Slovenia <- str_detect(paises$TIKAB, regex(pattern = "\\bSlovenia", ignore_case = T))
paises$Solomon_Islands <- str_detect(paises$TIKAB, regex(pattern = "\\bSolomon Islands", ignore_case = T))
paises$Somalia <- str_detect(paises$TIKAB, regex(pattern = "\\bSomali", ignore_case = T))
paises$South_Africa <- str_detect(paises$TIKAB, regex(pattern = "\\bSouth Africa", ignore_case = T))
paises$South_Korea <- str_detect(paises$TIKAB, regex(pattern = "\\bSouth Korea", ignore_case = T))
paises$South_Ossetia <- str_detect(paises$TIKAB, regex(pattern = "\\bSouth Ossetia", ignore_case = T))
paises$Spain <- str_detect(paises$TIKAB, regex(pattern = "\\bSpain|Spanish", ignore_case = T))
paises$Sri_Lanka <- str_detect(paises$TIKAB, regex(pattern = "\\bSri Lanka", ignore_case = T))
paises$Sudan <- str_detect(paises$TIKAB, regex(pattern = "\\bSudan", ignore_case = T))
paises$Suriname <- str_detect(paises$TIKAB, regex(pattern = "\\bSuriname", ignore_case = T))
paises$Swaziland <- str_detect(paises$TIKAB, regex(pattern = "\\bSwaziland", ignore_case = T))
paises$Sweden <- str_detect(paises$TIKAB, regex(pattern = "\\bSwed", ignore_case = T))
paises$Switzerland <- str_detect(paises$TIKAB, regex(pattern = "\\bSwitzerland", ignore_case = T))
paises$Syria <- str_detect(paises$TIKAB, regex(pattern = "\\bSyria", ignore_case = T))
paises$Taiwan <- str_detect(paises$TIKAB, regex(pattern = "\\bTaiwan", ignore_case = T))
paises$Tajikistan <- str_detect(paises$TIKAB, regex(pattern = "\\bTajikistan", ignore_case = T))
paises$Tanzania <- str_detect(paises$TIKAB, regex(pattern = "\\bTanzania", ignore_case = T))
paises$Thailand <- str_detect(paises$TIKAB, regex(pattern = "\\bThailand", ignore_case = T))
paises$Timor_Leste <- str_detect(paises$TIKAB, regex(pattern = "\\bTimor-Leste|\\bEast Timor|\\bTimor Leste", ignore_case = T))
paises$Togo <- str_detect(paises$TIKAB, regex(pattern = "\\bTogo", ignore_case = T))
paises$Tonga <- str_detect(paises$TIKAB, regex(pattern = "\\bTonga", ignore_case = T))
paises$Trinidad_and_Tobago <- str_detect(paises$TIKAB, regex(pattern = "\\bTrinidad and Tobago", ignore_case = T))
paises$Tunisia <- str_detect(paises$TIKAB, regex(pattern = "\\bTunisia", ignore_case = T))
paises$Turkey <- str_detect(paises$TIKAB, regex(pattern = "\\bTurkey|\\bTurkish", ignore_case = T))
paises$Turkmenistan <- str_detect(paises$TIKAB, regex(pattern = "\\bTurkmenistan", ignore_case = T))
paises$Tuvalu <- str_detect(paises$TIKAB, regex(pattern = "\\bTuvalu", ignore_case = T))
paises$Uganda <- str_detect(paises$TIKAB, regex(pattern = "\\bUganda", ignore_case = T))
paises$Ukraine <- str_detect(paises$TIKAB, regex(pattern = "\\bUkraine", ignore_case = T))
paises$United_Arab_Emirates <- str_detect(paises$TIKAB, regex(pattern = "\\bUnited Arab Emirates", ignore_case = T))
paises$United_Kingdom <- str_detect(paises$TIKAB, regex(pattern = "\\bUnited Kingdom|\\bu\\.k\\.|\\bengland|\\bgreat britain", ignore_case = T))
paises$United_States <- str_detect(paises$TIKAB, regex(pattern = "\\bUnited States|\\bu\\.s\\.", ignore_case = T))
paises$Uruguay <- str_detect(paises$TIKAB, regex(pattern = "\\bUrugua", ignore_case = T))
paises$Uzbekistan <- str_detect(paises$TIKAB, regex(pattern = "\\bUzbekistan", ignore_case = T))
paises$Vanuatu <- str_detect(paises$TIKAB, regex(pattern = "\\bVanuatu", ignore_case = T))
paises$Vatican_City <- str_detect(paises$TIKAB, regex(pattern = "\\bVatican City", ignore_case = T))
paises$Venezuela <- str_detect(paises$TIKAB, regex(pattern = "\\bVenezuela", ignore_case = T))
paises$Vietnam <- str_detect(paises$TIKAB, regex(pattern = "\\bVietnam", ignore_case = T))
paises$Yemen <- str_detect(paises$TIKAB, regex(pattern = "\\bYemen", ignore_case = T))
paises$Zambia <- str_detect(paises$TIKAB, regex(pattern = "\\bZambia", ignore_case = T))
paises$Zimbabwe <- str_detect(paises$TIKAB, regex(pattern = "\\bZimbabwe", ignore_case = T))

paises <- paises %>% gather(key = "COUNTRY", value = "CHECK", Abkhazia:Zimbabwe) %>%
  filter(CHECK == 1)

a <- paises %>% group_by(COUNTRY) %>% summarise(n = n()) %>% arrange(-n) %>% 
  mutate(COUNTRY = gsub("_", " ", COUNTRY),
         label = paste0(n, " (", percent(n/nrow(M), accuracy = .01), ")")) %>% 
  mutate(COUNTRY = ifelse(COUNTRY == "United States", "USA",
                          ifelse(COUNTRY == "United Kingdom", "UK", COUNTRY)))


# inversa de simpson

diversity(a$n, index = "invsimp")

if(require(maps) == F) install.packages("maps"); require(maps)

world <- map_data("world")

world <- world %>% left_join(a, by = c("region" = "COUNTRY"))

ggplot(world, aes(map_id = region)) +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = region, fill = n)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Qtd. Artigos")

ggsave("grafico 15.png")
#write.xlsx(read, "papers ler secao de metodos.xlsx")


# identificação manual dos desenhos de pesquisa

setwd(here())

D_man <- read_xlsx("papers experiementais.xlsx") %>% 
  filter(exp_type != "NA" &
           exp_type != "0") # 195 porque 7 artigos não foram encontrados e nem os autores enviaram o paper por email

nrow(D_man)/nrow(D)

D_out <- read_xlsx("papers experiementais.xlsx") %>% 
  filter(exp_type == "NA") # 195 porque 7 artigos não foram encontrados e nem os autores enviaram o paper por email
table(D_out$SO)


D_man %>% 
  group_by(exp_type) %>% 
  summarise(n = n()) %>% arrange(-n)

exp <- D_man %>% cSplit('exp_type', sep = ";", direction = "long") %>% 
  group_by(exp_type) %>% 
  summarise(n = n()) %>% arrange(-n) %>% 
  mutate(prop = percent(n/nrow(D_man), accuracy = 0.1))

ggplot(exp, aes(x = reorder(exp_type, n), y = n)) +
  geom_bar(stat = "identity", width = .6, fill = "tomato3", color = "black") +
  geom_text(aes(label = paste0(n, " (", prop, ")"), vjust = -.5)) +
  labs(x = "Tipo de Experimento", y = "Qtd. Artigos Experimentais") +
  expand_limits(y = c(0, 110))

setwd(here("Dados"))

ggsave("grafico 16.png", width = 16, height = 10, units = "cm", dpi = 300)


(103+29+27+11+2+1)/nrow(D_man)

exp_py_type <- D_man %>% cSplit('exp_type', sep = ";", direction = "long") %>%
  group_by(exp_type, PY) %>% summarise(n = n())

ggplot(exp_py_type %>% filter(exp_type %in% c("Field", "Laboratory")), aes(x = PY, y = n)) +
  geom_line(aes(color = exp_type))


h <- data.frame(t(coef(res))) %>% 
  rownames_to_column(var = "CODE") %>% 
  filter(!CODE %in% D_out$CODE)

colnames(h) <- c("CODE", "Democracia e Participação", "Opinião Pública", "Atitudes e Identidade Social", "Regime e Fraudes", "Cooperação Internacional", "Estado e Reforma", "Gênero", "Partidos Políticos", "Eleição e Clientelismo", "Políticas Públicas")
head(h)

h <- h %>% gather(key = "type", value = "nmf", `Democracia e Participação`:`Políticas Públicas`)
h <- h %>% group_by(CODE) %>% mutate(check = ifelse(nmf == max(nmf), 1, 0)) %>% 
  filter(check == 1)

h <- h %>% left_join(D_man %>% select(CODE, exp_type) %>% cSplit('exp_type', sep = ";", direction = "long"))


glimpse(h)
to_ca <- h %>% 
  group_by(type, exp_type) %>% summarise(n = n()) %>% 
  spread(type, n)

to_ca[is.na(to_ca)] <- 0

to_ca <- column_to_rownames(to_ca, var = "exp_type")

res.ca <- CA(to_ca, graph = T)
eig <- as.data.frame(get_eigenvalue(res.ca))

fviz_eig(res.ca)

eig <- eig %>% rownames_to_column(var = "Dimensao")

ggplot(eig, aes(x = Dimensao, y = variance.percent)) +
  geom_bar(stat = "identity", fill = "tomato3", width = .6) +
  geom_point() +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = round(variance.percent, 2)), size = 3, vjust = -.5) +
  labs(x = "Dimensão", y = "Variância Explicada")

setwd(here("Dados"))
ggsave("grafico 17.png", width = 16, height = 10, units = "cm", dpi = 300)

df <- rbind(as.data.frame(res.ca$row$coord) %>% rownames_to_column(var = "Termo") %>% mutate(TIPO = "Experimento"),
            as.data.frame(res.ca$col$coord) %>% rownames_to_column(var = "Termo") %>% mutate(TIPO = "Tema"))


df_c <- df %>% dplyr::select(Termo, `Dim 1`, `Dim 2`) %>% column_to_rownames(var = "Termo")
dist_matrix <- dist(df_c)


ward <-  hclust(dist_matrix, method = "ward.D2")

setwd(here("Dados"))

png("grafico 1A.png", width = 16, height = 12, units = "cm", res = 300)
plot(ward, main = "Dendrograma", xlab = "", sub = "")
dev.off()



id_clusters <- cutree(ward, k = 4)

ggplot(df %>% mutate(cluster = id_clusters), aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  geom_vline(xintercept = 0, color = "grey", lty = "dashed") +
  geom_point(aes(color = TIPO)) +
  geom_encircle(aes(color = as.character(cluster))) +
  geom_text_repel(aes(label = Termo, color = TIPO), size = 3) +
  scale_color_manual(values = c("Experimento" = "tomato3",
                                "Tema" = "cornflowerblue")) +
  labs(x = "Dim 1 (40,06%)", y = "Dim 2 (36,57%)", color = "")

ggsave("grafico 18.png", width = 16, height = 12, units = "cm", dpi = 300)


# amostras aleatórias



field <- D_man %>% filter(str_detect(exp_type, "Field"))

set.seed(1995) # meu ano de nascimento

field <- sample(field$CODE, size = 1)

lab <- D_man %>% filter(str_detect(exp_type, "Lab"))

set.seed(1995) # meu ano de nascimento

lab <- sample(lab$CODE, size = 1)

survey <- D_man %>% filter(str_detect(exp_type, "Survey|Conjoint|Vignette|List"))

set.seed(1995) 

survey <- sample(survey$CODE, size = 1)


field
lab
survey



D_man %>% filter(CODE == field) %>% dplyr::select(TI)
D_man %>% filter(CODE == lab) %>% dplyr::select(TI)
D_man %>% filter(CODE == survey) %>% dplyr::select(TI)
D_man %>% filter(CODE == "ID 352") %>% dplyr::select(TI)

# download dos artigos

D_download <- D %>% select(CODE, AU, PY, TI, SO, DI)

setwd(here())
#write.xlsx(D_download, "papers experiementais.xlsx")
