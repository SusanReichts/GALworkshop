# Vorbereitungsübung
  
15/3 #berechnet Funktion und gibt Lösung als Output
ErsteFunktion <- 15/3 #weist Berechnung dem Objekt zu
ErsteFunktion #ruft Objekt auf, Lösung wird als Output angezeigt

# So erstellt man ein Dataframe aus 2 Vektoren

eins <- c("Person A", "Person B", "Person C") #erste Spalte, kategorisch
zwei <- c(5,6,3) #zweite Spalte, numerisch
drei <- data.frame(eins, zwei) #Dataframe

drei #zeigt Datensatz


install.packages("tidyverse")
library(tidyverse)



# Objekte laden
# Link: https://github.com/SusanReichts/GALworkshop


sample <- read_csv("GAL_sample.txt") #liest Datei vom Working Directory

# Alternativ kann die Datei auch direkt von online Ordnern geladen werden

sample <- read_csv("https://raw.githubusercontent.com/SusanReichts/GALworkshop/main/GAL_sample.csv")
ME_data <- read_csv("https://raw.githubusercontent.com/SusanReichts/GALworkshop/main/ME_data.csv")
ME_socialdata <- read_csv("https://raw.githubusercontent.com/SusanReichts/GALworkshop/main/ME_data_sociodemo.csv")
hedge_data <- read_csv("https://raw.githubusercontent.com/SusanReichts/GALworkshop/main/GAL_hedge.csv")
hedge_socialdata <- read_csv("https://raw.githubusercontent.com/SusanReichts/GALworkshop/main/GAL_hedge_sociodemo.csv")


# Objekte beschreiben


dim(sample) #Einblick in die Dimensionen
head(sample) #Einblick in die ersten 6 Beobachtungen 
summary(sample) #Einblick in die Variablen/Vektoren
names(sample) #Einblick in die Variablen/Vektoren Namen
sample$Name #Einblick in die Werte einer Variablen



# Objekteigenschaften beschreiben
# Daten filtern

sample_pause <- sample %>% filter(Var1 == "pause")
sample_pause

# Daten zusammenfassen

sample %>% 
  summarise("distinct names" = n_distinct(Name))

sample %>% 
  summarise("distinct names" = unique(Name))

# Daten gruppieren

sample %>%  group_by(Name) %>% 
  summarise("Name" = unique(Name),
            "Mean_Duration" = mean(Duration)) %>% 
  ungroup()

# Data Cleaning

# Data Cleaning 1: Informationen aus Zellen extrahieren
# Wir teilen die Zellen und nutzen dabei Trennzeichen, die in den Werten enthalten sind. Hierbei hilft das tidyr Paket.

sample <- sample %>% 
  separate(Var2, 
           sep="_", 
           into=c("Category", "Year"))
names(sample)


ME_data <- ME_data %>% 
  separate(File, 
           sep="_", 
           into=c("Date", "Conversation_Pair", "Topic","Focus"))
names(ME_data)

# Data Cleaning 2: Pivot 🛋️
# Fall A: zusammengefasste Variablen aufteilen - pivot_wider()

sample_wide <- sample %>% 
  pivot_wider(names_from = Var1,
              values_from = Duration)

sample_wide

# Fall B: Einzelne Variablen zusammenbringen - pivot_longer()

ME_data_long <- ME_data %>% 
  pivot_longer(cols = c(Anna,Bert,Cassie,Doyle,Erin,Flo,Gemma,Holly,Ike),
               names_to = "speaker",
               values_to = "words")

names(ME_data_long)

# Data Cleaning 3: Leere & doppelte Beobachtungen entfernen

ME_data_long <- ME_data_long %>% 
  subset(words != "") %>% 
  distinct(words, .keep_all = TRUE) 

# Data Cleaning 4: Vektoren zusammenführen
# Da jede Zeile eine Annotation einer Person beschreibt, wird in den Annotationsspalten pro Zeile immer nur ein Wert erfüllt. Somit können wir vergleichbare Annotationsspalten zusammenführen und dabei NA überspringen. 
# Das fasst dann zwei Annotationen in einer Spalte zusammen - abhängig von der Spalte der Sprechenden. 

ME_data_long <-ME_data_long %>% 
  mutate(backchannel.actions = coalesce( `Anna backchannel actions`,
                                         `Bert backchannel actions`,
                                         `Cassie backchannel actions`,
                                         `Doyle backchannel actions`,
                                         `Erin backchannel actions`,
                                         `Flo backchannel actions`,
                                         `Gemma backchannel actions`,
                                         `Holly backchannel actions`,
                                         `Ike backchannel actions`))

ME_data_long <-ME_data_long %>% 
  mutate(wordtype = coalesce( `Anna backchannels`,
                              `Bert backchannels`,
                              `Cassie backchannels`,
                              `Doyle backchannels`,
                              `Erin backchannels`,
                              `Flo backchannels`,
                              `Gemma backchannels`,
                              `Holly backchannels`,
                              `Ike backchannels`))

# Data Cleaning 5: Werte anpassen
# 1. Ersetze alle NA mit "talk"
# 2. Finde alle Zellen die "]" enthalten und ersetze diese mit "backchannel". Wenn "]" nicht vorkommt, behalte den Ursprungswert

ME_data_long <- ME_data_long %>% 
  mutate(wordtype = replace_na(wordtype, "talk"),
         wordtype = case_when(str_detect(wordtype, "]") ~"backchannel",
                              TRUE ~ wordtype))

# Data Cleaning 6: Werte "aufräumen"
# Einige Variablen beinhalten noch die Turn-ID. Da wir damit für diese Analyse nicht arbeiten, können wir diese einfach entfernen. (Alternativ könnte man sie aber auch in eine eigene Spalte schieben)

ME_data_long <- ME_data_long %>% 
  mutate(across(c(words, 
                  Turntype, 
                  `Turn subtype`, 
                  backchannel.actions), ~ str_replace(.x, "\\[.*", "")))

# Data Cleaning 7: Spaltennamen anpassen & auswählen
# Es ist hilfreich, wenn man die Spaltennamen anpasst. 

ME_data_long <- ME_data_long %>% 
  rename(Start = `Begin Time - hh:mm:ss.ms`,
         End = `End Time - hh:mm:ss.ms`,
         Turn_Subtype = `Turn subtype`,
         Words =  words,
         Wordtype = wordtype,
         BC_Functions = backchannel.actions,
         Speaker = speaker) %>% 
  select(Speaker, 
         Conversation_Pair, 
         Words, 
         Wordtype, 
         BC_Functions, 
         Turn_Subtype,
         Start, 
         End)



# Ein paar Beispiele
# Fügt Kommentare hinzu die beschreiben, was in diesen Beispielen passiert!
# Fügt eigene Beispiele hinzu!

ME_data_long %>% 
  group_by(Conversation_Pair) %>% 
  summarise("backchannels (N)" =  sum(Wordtype=="backchannel")) %>% 
  ungroup()


ME_data_long %>% 
  group_by(BC_Functions) %>% 
  summarise("backchannels" = sum(Wordtype=="backchannel")) %>% 
  arrange(desc(backchannels)) %>% 
  ungroup()


ME_data_long %>% 
  subset(Turn_Subtype!="other ") %>% 
  count(Turn_Subtype) %>% 
  arrange(desc(n))



# Hinzufügen weiterer Datensätze
# Für den ME_data Datensatz haben wir eine zweite Tabelle, welche sozio-demographische Infos der Sprechenden beinhaltet. Für die Funktion benötigen wir eine Spalte, die bei beiden Tabellen vorkommt und den Link erzeugt.


ME_joined <- left_join(ME_data_long, 
                       ME_socialdata, 
                       by= "Speaker")
ME_joined


## Visuelle Auswertung

# Balkendiagramm

ME_data_long %>% 
  subset(Turn_Subtype!="other ") %>% 
  ggplot(aes(x = Turn_Subtype)) + 
  geom_bar(stat="count")

# Balkendiagramm mit zusätzlichen Optionen

ME_data_long %>% 
  subset(Turn_Subtype!="other ") %>% 
  ggplot(aes(x = Turn_Subtype)) + 
  geom_bar(stat="count",
           color="black",
           fill="forestgreen")+
  labs(title="Subtypes of Backchannels",
       x="",
       y="Counts of subtypes") +
  theme_bw()



# Boxplot

ME_joined %>% 
  group_by(gender, Speaker) %>% 
  count(Wordtype) %>% 
  ungroup() %>% 
  ggplot(aes(x=gender, y=n)) +
  geom_boxplot()


# Boxplot mit zusätzlichen Optionen

ME_joined %>% 
  group_by(gender, Speaker) %>% 
  count(Wordtype) %>% 
  ungroup() %>% 
  ggplot(aes(x=gender, y=n, fill=gender)) +
  geom_boxplot()+
  theme_bw() +
  scale_fill_brewer(palette=1)+
  labs(title="Backchannel across gender",
       x="",
       y="backchannels (N)") +
  facet_wrap(~Wordtype)

# Scatter/Jitter

ME_joined %>% 
  subset(Wordtype!="talk") %>% 
  subset(Conversation_Pair=="Anna-Cassie") %>% 
  ggplot(aes(x=Start, y=Speaker, color=BC_Functions)) +
  geom_jitter(height = .1,
              alpha=.4)+
  theme_bw() +
  labs(title="Backchannels across a conversation",
       subtitle="Anna and Cassie",
       color="functions",
       y="",
       x="")


---
# Neue csv speichern (aus Objekten)
write_csv()

# Visualisierungen speichern
ggsave()
  

#Zugang zu einem allgemeinen R/Tidyverse Kurs: https://kurzelinks.de/mzaa 