library(udpipe)
library(dplyr)
require(ltm)
require(gtools)
require(psych)
rm(list=ls())

#Pretpostavke####
# - uvodni govor je treći transkript po redu - detekciju uvoda ("PRED_U") treba automatizirati, kao i detekciju ostalih tipova govora
# - šifre izjava i ocjene importirane izvana (v. retke 120-129, odnosno Find "SifraIzjave")
rednibrojuvodnoggovora <- 3
#kultura je 2012941, arhivska je 2012911
IDodabranerasprave <- 2012941

#učitaj podatke - kreće se od Petrinog data.framea t_5[...]9####
    setwd("D:/zadaton/ODD ozujak 2019 parlametar")
    saziv9<-read_delim("9_saziv.txt", delim = "\t") #Petra, i ako zamijeniš ovo čitanje sa svojim objektom t_5[...]_9, trebalo bi funkati
    length(saziv9$Je_najava[saziv9$Je_najava == "TRUE"]) #1420 najava
    length(unique(saziv9$Rasprava_ID)) #ali, samo 811 unique rasprava ID
    
    teme<-saziv9[saziv9$Je_najava=="TRUE",]
    View(teme) #pregledom rasprava ID za najava = TRUE, može se ustanoviti da se unutar istog ID-ja rasprave, ponavljaju iste teme.
    #Dakle, rasprava ID je razina na kojoj trebamo razgraničavati teme
    
    #Za anotaciju, moramo najave izvući van u posebnu kolonu, gdje će tema (=tekst najave) biti ista za jedan ID rasprave (možda neće nužno biti unique)
    #Napravit ćemo to tako da uzmemo prvo pojavljivanje najave unutar jednog ID rasprave u poseban data frame, kojega ćemo zatim mergeati s glavnim frameom prema ID rasprave
    raspravaID_tema <- saziv9 %>%
      filter(grepl("TRUE",Je_najava)) %>%
      group_by(Rasprava_ID) %>%
      top_n(n=1, wt=desc(RedniBrojIzjave)) %>%
      dplyr::select(Rasprava_ID, Transkript, RedniBrojIzjave)
    
    
    nrow(raspravaID_tema) #isti broj redaka kao unique ID-jeva rasprave, dakle OK
    table(raspravaID_tema$RedniBrojIzjave) #sve su teme pridane raspravama po prvoj izjavi unutar ID-ja rasprave, dakle OK
    #prije spajanja s temom rasprave, mičemo najave iz glavnog data.framea, jer bi nam u anotaciji i kasnijoj kakvoj god analizi bile šum
    saziv9_clean <-saziv9[saziv9$Je_najava!="TRUE",]
    
    #preimenovat "Transkript" u tablici s temama u "Tema" radi izbjegavanja duplih imena u tablicama koje spajamo
    raspravaID_tema <- raspravaID_tema %>% rename (Tema = Transkript)
    #a ne treba nam ni redni broj
    raspravaID_tema <- raspravaID_tema[,which(!colnames(raspravaID_tema) %in% "RedniBrojIzjave")]
    
    saziv9_clean <- saziv9_clean %>%
      left_join(x=saziv9_clean, y = raspravaID_tema, by = "Rasprava_ID")

#write_delim(saziv9_clean, "9_saziv_obrada1.txt", delim = "|")

    
#setwd("D:/zadaton/ODD ozujak 2019 parlametar")
#saziv9_clean <-read_delim("9_saziv_obrada1.txt", delim = "|")
  
  rasprava<-saziv9_clean[saziv9_clean$Rasprava_ID==IDodabranerasprave,]

#Isključivanje upadica
#rasprava<-rasprava %>%
          #filter(!grepl("/Upadica", Transkript))

#anotacija####
udmodel<-udpipe_load_model(file="C:/Users/babatusta/Documents/croatian-set-ud-2.3-181115.udpipe")
rasprava_anot<-udpipe_annotate(udmodel, x = rasprava$Transkript, doc_id = rasprava$RedniBrojIzjave, trace = TRUE, parser = "none")
rasprava_anot<-as.data.frame(rasprava_anot)
#View(rasprava_anot)

#selekcija vrste riječi####
length(rasprava_anot$doc_id)
izjave <- unique(rasprava_anot$doc_id)
stats_list <- list()
for (i in 1:length(izjave)) {
  #stats <- subset(rasprava_anot, upos %in% c("ADJ","NOUN","VERB") & doc_id %in% izjave[i])
  stats <- subset(rasprava_anot, upos %in% c("ADJ","NOUN") & doc_id %in% izjave[i])
  if(i==3) {
    #čišćenje prve rečenice iz uvoda gdje je najviše redundancije (poštovanje, kolege, zastupnici, itd.)
    stats <- subset(rasprava_anot, upos %in% c("ADJ","NOUN") & doc_id %in% izjave[i] & sentence_id!=1)
    print(head(stats)) #ako sentence_id na početku nije 1, onda je OK
    }
  stats <- txt_freq(stats$lemma)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  stats$izjava <- rep(izjave[i], nrow(stats))
  stats_list[[i]]<-stats
}

#čišćenje redundantnih riječi - to do ####
# for (i in 3) {
#   i<-3
#   stats_cleaning <- subset(rasprava_anot, 
#                   upos %in% c("ADJ","NOUN") 
#                   & doc_id %in% izjave[i])
#   stats_cleaning$lemma<-tolower(stats_cleaning$lemma)
#   stats_cleaning <- txt_freq(stats_cleaning$lemma)
#   stats_cleaning <- stats_cleaning[!grepl("hvala|kolega|poštovan|imati|moći|biti", stats_cleaning$key), ]
#   stats_cleaning$key <- factor(stats$key, levels = rev(stats$key))
#   stats_cleaning$izjava <- rep(izjave[i], nrow(stats))
#   stats_list[[i]]<-stats_cleaning
# }

#stats_all<-do.call(rbind, stats_list)

###1. skorovi poklapanja S UVODNIM GOVOROM (redni broj 3)####
stats_merge <- list()
stats_uvodnoggovora<-stats_list[[rednibrojuvodnoggovora]]
#i<-3
for (i in 1:length(izjave)) {
  print(i)
  stats_merge[[i]]<-merge(x=stats_list[[i]], y=stats_uvodnoggovora, by = "key", all.x = TRUE)
  stats_merge[[i]]$skor1 <- rep(NA,nrow(stats_merge[[i]]))
  stats_merge[[i]]$skor1[which(!is.na(stats_merge[[i]]$freq_pct.y))]<-stats_merge[[i]]$freq_pct.x[which(!is.na(stats_merge[[i]]$freq_pct.y))]
  stats_merge[[i]]$skor2<-stats_merge[[i]]$freq_pct.x*stats_merge[[i]]$freq_pct.y
}

###sumiranje skorova na data frame svake izjave
  #dva vektora suma
  vektor_sumaskor1 <- sapply(stats_merge, FUN=function(x) sum(x$skor1, na.rm = TRUE))
  vektor_sumaskor2 <- sapply(stats_merge, FUN=function(x) sum(x$skor2, na.rm = TRUE))
  
  # Mesić i Glasnović u kulturi 
  #vektor_sumaskor1[c(5,60)]
  #vektor_sumaskor2[c(5,60)]
  
  rasprava<-data.frame(rasprava, ontopicuvod1 = vektor_sumaskor1, ontopicuvod2 = vektor_sumaskor2) 
#IMPORT ŠIFRI TIPOVA GOVORA I JAKINIH OCJENA####
  #Šifra izjave (član sekvence)
  #Spajanje s izvanjskim šiframa izjave
  #SifraIzjave <- read_delim("sifraizjave_arhiva.txt", delim = "\t")
  SifraIzjave <- read_delim("sifraizjave_kultura.txt", delim = "\t")
  rasprava <- left_join(rasprava, SifraIzjave[,c("RedniBrojIzjave","SifraIzjave")], by = "RedniBrojIzjave")
  #ocjene_uvod <- read_delim("ocjene_ontopicuvod_arhiva.txt", delim = "\t")
  ocjene_uvod <- read_delim("ocjene_ontopicuvod_kultura.txt", delim = "\t")
  rasprava <-left_join(rasprava, ocjene_uvod, by = "RedniBrojIzjave")
  
  #USPOREDBA RUČNOG I STROJNOG KODIRANJA ONTOPIC UVODA####
  #KORELACIJE RAZNI POKAZATELJI####
  rasprava_corr <- rasprava[!is.na(rasprava$ocjena_uvod)&rasprava$ocjena_uvod!=99,]
  cor.test(rasprava_corr$ocjena_uvod,rasprava_corr$ontopicuvod1)
  cor.test(rasprava_corr$ocjena_uvod,rasprava_corr$ontopicuvod2)
  
  #poliserijalna korelacija
  polyserial(x=as.matrix(rasprava_corr[, c("ontopicuvod1")]), y = as.matrix(rasprava_corr[, c("ocjena_uvod")]))
  polyserial(x=as.matrix(rasprava_corr[, c("ontopicuvod2")]), y=as.matrix(rasprava_corr[, c("ocjena_uvod")]))
  
  
  #Cohen's Kappa, trihotomizirani skor (tercili)
  ontopicuvod1_tri<-gtools::quantcut(rasprava_corr$ontopicuvod1, q=3, na.rm=TRUE)
  ontopicuvod1_tri<-as.numeric(ontopicuvod1_tri)
  ontopicuvod2_tri<-gtools::quantcut(rasprava_corr$ontopicuvod2, q=3, na.rm=TRUE)
  ontopicuvod2_tri<-as.numeric(ontopicuvod2_tri)
  data.cohenkappa <- data.frame(ontopicuvod1_tri, ocjena_uvod_rec = rasprava_corr$ocjena_uvod+1)
  psych::cohen.kappa(data.cohenkappa)
  data.cohenkappa <- data.frame(ontopicuvod2_tri, ocjena_uvod_rec = rasprava_corr$ocjena_uvod+1)
  psych::cohen.kappa(data.cohenkappa)
  
###DESKRIPTIVNE STATISTIKE topikalnost u odnosu na Uvod####
      deskriptiva_tipovi <- rasprava_corr %>%
                        group_by(SifraIzjave) %>%
                        summarise(mean_ontopicuvod1 = mean(ontopicuvod1),
                                  mean_ontopicuvod2 = mean(ontopicuvod2),
                                  mean_ocjena_uvod = mean(ocjena_uvod),
                                  count=n())
  
      deskriptiva_osoba <- rasprava_corr %>%
        group_by(Osoba) %>%
        summarise(mean_ontopicuvod1 = mean(ontopicuvod1),
                  mean_ontopicuvod2 = mean(ontopicuvod2),
                  mean_ocjena_uvod = mean(ocjena_uvod),
                  count=n())
  
        
        deskriptiva_klub <- rasprava_corr %>%
          group_by(ZastupnickiKlub) %>%
          summarise(mean_ontopicuvod1 = mean(ontopicuvod1),
                    mean_ontopicuvod2 = mean(ontopicuvod2),
                    mean_ocjena_uvod = mean(ocjena_uvod),
                    count=n())
        
        #filter na rasprave klubova
        deskriptiva_klub_rasprave <- rasprava_corr %>%
          filter(grepl("RAS_K",SifraIzjave)) %>%
          group_by(ZastupnickiKlub) %>%
          summarise(mean_ontopicuvod1 = mean(ontopicuvod1),
                    mean_ontopicuvod2 = mean(ontopicuvod2),
                    mean_ocjena_uvod = mean(ocjena_uvod),
                    count=n())
        
#Relevantnost replika na rasprave klubova i rasprave individua (RAS_K i RAS_i)####
  #sekvence od RAS_K (ili RAS_I) do RAS_K (ili RAS_I)

          #prva pojava RAS je prva referentna rasprava za usporedbu govora s njome
          pozicije_rasprava<-which(grepl("RAS", rasprava$SifraIzjave) %in% "TRUE")
          stats_rasprave<-stats_list[[pozicije_rasprava[1]]]
          stats_merge_ras <- list()
          for (i in 1:nrow(rasprava)) {
            print(i)
            if(is.na(rasprava$SifraIzjave[i])) {
              stats_merge_ras[[i]]<-data.frame(skor1=c(NA),skor2=c(NA))
            }
            
            else if(grepl("RAS", rasprava$SifraIzjave[i])) {
            stats_rasprave<-stats_list[[i]]
            print("Dinamo Zagreb")
               }
            
            else if(grepl("REP$", rasprava$SifraIzjave[i])) {
            print("Hajduk Split")
            stats_merge_ras[[i]]<-merge(x=stats_list[[i]], y=stats_rasprave, by = "key", all.x = TRUE)
            stats_merge_ras[[i]]$skor1 <- rep(NA,nrow(stats_merge_ras[[i]]))
            stats_merge_ras[[i]]$skor1[which(!is.na(stats_merge_ras[[i]]$freq_pct.y))]<-stats_merge_ras[[i]]$freq_pct.x[which(!is.na(stats_merge_ras[[i]]$freq_pct.y))]
            stats_merge_ras[[i]]$skor2<-stats_merge_ras[[i]]$freq_pct.x*stats_merge_ras[[i]]$freq_pct.y
              }
            else {
              stats_merge_ras[[i]]<-data.frame(skor1=c(NA),skor2=c(NA))
             }
            }

        vektor_sumaskor1 <- sapply(stats_merge_ras, FUN=function(x) sum(x$skor1, na.rm = TRUE))
        vektor_sumaskor2 <- sapply(stats_merge_ras, FUN=function(x) sum(x$skor2, na.rm = TRUE))
   
        vektor_sumaskor1[-grep("REP$", rasprava$SifraIzjave)]<-NA
        vektor_sumaskor2[-grep("REP$", rasprava$SifraIzjave)]<-NA
        
        rasprava<-data.frame(rasprava, ontopicrasprava1 = vektor_sumaskor1, ontopicrasprava2 = vektor_sumaskor2) 
        
        #Relevantnost odgovora na replike####
        pozicije_replika<-which(grepl("RAS$", rasprava$SifraIzjave) %in% "TRUE")
        stats_replika<-stats_list[[pozicije_replika[1]]]
        stats_merge_rep <- list()
        for (i in 1:nrow(rasprava)) {
          print(i)
          if(is.na(rasprava$SifraIzjave[i])) {
            stats_merge_rep[[i]]<-data.frame(skor1=c(NA),skor2=c(NA))
          }
          
          else if(grepl("REP$", rasprava$SifraIzjave[i])) {
            stats_rep<-stats_list[[i]]
            print("Dinamo Zagreb")
          }
          
          else if(grepl("ODG", rasprava$SifraIzjave[i])) {
            print("Hajduk Split")
            stats_merge_rep[[i]]<-merge(x=stats_list[[i]], y=stats_rep, by = "key", all.x = TRUE)
            stats_merge_rep[[i]]$skor1 <- rep(NA,nrow(stats_merge_rep[[i]]))
            stats_merge_rep[[i]]$skor1[which(!is.na(stats_merge_rep[[i]]$freq_pct.y))]<-stats_merge_rep[[i]]$freq_pct.x[which(!is.na(stats_merge_rep[[i]]$freq_pct.y))]
            stats_merge_rep[[i]]$skor2<-stats_merge_rep[[i]]$freq_pct.x*stats_merge_rep[[i]]$freq_pct.y
          }
          else {
            stats_merge_rep[[i]]<-data.frame(skor1=c(NA),skor2=c(NA))
          }
        }
        
        vektor_sumaskor1 <- sapply(stats_merge_rep, FUN=function(x) sum(x$skor1, na.rm = TRUE))
        vektor_sumaskor2 <- sapply(stats_merge_rep, FUN=function(x) sum(x$skor2, na.rm = TRUE))
        
        vektor_sumaskor1[-grep("ODG", rasprava$SifraIzjave)]<-NA
        vektor_sumaskor2[-grep("ODG", rasprava$SifraIzjave)]<-NA
        
        
        rasprava<-data.frame(rasprava, ontopicreplika1 = vektor_sumaskor1, ontopicreplika2 = vektor_sumaskor2) 
        
        #dužina transkripta
        rasprava$brojrijeci <- sapply(rasprava$Transkript, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
        write_delim(rasprava, iconv("kulturna_baština_rezultati_v2_jošskorova.txt", to="UTF-8"), delim = "|")
  
  #export_razlika<-data.frame(rasprava_corr, ontopicuvod1_tri, ontopicuvod2_tri, ocjena_uvod_rec = rasprava_corr$ocjena_uvod+1)
   # setwd("D:/zadaton/ODD ozujak 2019 parlametar/rezultati za prezu") 
   #    write_delim(as.data.frame(deskriptiva_tipovi), "primjer2_deskriptiva_tipovi.txt", delim = "|")
   #    write_delim(as.data.frame(deskriptiva_osoba), "primjer2_deskriptiva_osoba.txt", delim = "|")
   #    write_delim(as.data.frame(deskriptiva_klub), "primjer2_deskriptiva_klub.txt", delim = "|")
   #    write_delim(as.data.frame(deskriptiva_klub_rasprave), "primjer2_deskriptiva_rasprave.txt", delim = "|")
   # 
  ###NASTAVAK (TO DO) ISTA PJESMA S PRIJEDLOGOM ZAKONA KAO REFERENCOM####
      #Tekst za primjer s očuvanjem kulturnih dobara
      "Predloženim Zakonom uređuju se sljedeća pitanja:
        - utvrđuje se popis pravnih akata Europske unije čija se provedba osigurava ovim
      Zakonom odnosno koji se temeljem ovoga Zakona prenose u pravni poredak Republike
      Hrvatske,
      - propisuje se rok na koji se može odrediti preventivna zaštita za dobra za koje se
      predmnijeva da imaju svojstva kulturnog dobra, te isključuje mogućnost produljenja trajanja
      preventivne zaštite nakon isteka propisanog roka,
      - precizira se odredba o osiguranju naplate troškova privremenog skrbnika kulturnog
      dobra zasnivanjem zakonskog založnog prava u korist Grada Zagreba, grada ili općine koji su
      podmirili troškove skrbništva,
      - ograničava se vremenski obveza vlasnika u pogledu vraćanja sredstava uloženih iz
      državnog proračuna u zaštitu, očuvanje i obnovu kulturnog dobra, tako da je vlasnik dužan
      vratiti sredstava ako proda kulturno dobro u roku od pet godina od ulaganja sredstava iz
      državnog proračuna,
      - briše se odredba o osnivanju zakonskog založnog prava na kulturnom dobru radi
      osiguranja povrata sredstava iz državnog, županijskog, gradskog odnosno općinskog proračuna
      uloženih u zaštitu i očuvanje kulturnog dobra,
      - precizira se obveza sjecanja stručnih zvanja za obavljanje restauratorskokonzervatorskih poslova 
      na zaštiti i očuvanju kulturnih dobara, tako da se obveza stjecanja
      stručnih zvanja propisuje samo za javne ustanove za zaštitu i očuvanje kulturnih dobara, kojima
      su osnivači Republika Hrvatska, županije, Grad Zagreb, općine i gradovi,
      - smanjuje se visina spomeničke rente koja se mjesečno plaća po četvornom metru
      korisne površine poslovnog prostora koji se nalazi u nepokretnom kulturnom dobru ili na
      području kulturno-povijesne cjeline,
      - precizira se odredba o plaćanju neizravne spomeničke rente s obzirom na obavljanje
      određenih djelatnosti prema Nacionalnoj klasifikaciji djelatnosti,
      - prijelaznim i završnim odredbama propisuje se rok za donošenje provedbenog propisa,
      povrat pogrešno ili više uplaćenih prihoda od spomeničke rente koja se plaća po stopi od
      ukupnog prihoda, kao i odredba o stupanju na snagu zakona.
    
      Ograničit će se mogućnost produljivanja preventivne zaštite dobara za koje se
      predmnijeva da imaju svojstva kulturnog dobra,
      - osigurat će se vlasnicima kulturnih dobara povoljniji uvjeti za korištenje sredstava
      iz državnog proračuna, proračuna županija, Grada Zagreba, gradova i općina,
      - obvezno sjecanje stručnih zvanja za obavljanje restauratorsko-konzervatorskih
      poslova na zaštiti i očuvanju kulturnih dobara odredit će se samo za zaposlenike
      javnih ustanova za zaštitu i očuvanje kulturnih dobara, kojima su osnivači Republika
      Hrvatska, županije, Grad Zagreb, općine i gradovi, dok će ostale fizičke i pravne5
      osobe ove poslove moći obavljati na temelju dopuštenja Ministarstva kulture ako
      ispunjavaju propisane stručne i tehničke uvjete,
      - smanjit će se visina spomeničke rente koja se mjesečno plaća po četvornom metru
      korisne površine poslovnog prostora koji se nalazi u nepokretnom kulturnom dobru
      ili na području kulturno-povijesne cjeline,
      - nastavit će se plaćanje neizravne spomeničke rente i primjena režima smanjenog broja
      obveznika plaćanja ove rente."    
  


