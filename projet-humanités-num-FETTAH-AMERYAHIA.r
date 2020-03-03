library("jsonlite") # lire les donn�es sous format JSON
library("XML") 
library("textcat") #Detecter la langue
library("xlsx") # mettre sous fromat xlsx
library('rvest') #  parcourir et chercher le contenu d'une page web et le rendre exploitable par R
library("magrittr") 
library('tibble')


#************************** PARTIE 1 ************************************
#**************************** API ***************************************


# r�cuperer les donn�es de l'API qui representent les offres d'emplois du site jobs github

url_req <-"https://jobs.github.com/positions.json"

jobs<-fromJSON(url_req)

travaux<-as.data.frame(jobs[-11]) # supprimer derniere colonne (logo de la compagnie)


#--------------------------------------------------------------------------

# fonction de traduction 
# utilisation de l'API translation de google afin de traduire les donn�es r�cuper�es de l'API.

translate <- function(text,
                      API_Key,
                      target = "de",
                      source = "en") {
  b <- paste0(
    '{
    "q": [
    "',
    text,
    '"
    ],
    "target": "',
    target,
    '",
    "source": "',
    source,
    '",
    "format": "text"
}'
  )
  url <-
    paste0("https://translation.googleapis.com/language/translate/v2?key=",
           API_Key)
  x <- httr::POST(url, body = b)
  x <- jsonlite::fromJSON(rawToChar(x$content))
  x <- x$data$translations
  return(x$translatedText[1])
  
}

# stockage des r�sultats de la traduction dans des listes

l_desc <- list()
l_type <- list()
l_apply<- list()

for (i in 1:nrow(travaux)) {
  
  # nettoyage des colonnes description et how_to_apply ( contiennent des balises html )
  
  travaux$description[i] <- read_html(travaux$description[i]) %>% html_text() 
  travaux$how_to_apply[i] <- read_html(travaux$how_to_apply[i]) %>% html_text()
  
  # traduction 
  
  langues <- textcat(travaux$description[i]) # detecter la langue de chaque offre
  
  if (langues == "german" || langues == "dutch") {   langue <- "de" }
  if (langues == "english") { langue <- "en" }
  
  # traduire les champs associ�es
  
  l_type <- c(l_type,translate(as.character(travaux$type[i]),"AIzaSyB_rTK_BcHo7p8I_LOPhxthelriXgijDzw",'fr','en'))
  l_apply <- c(l_apply,translate(as.character(travaux$how_to_apply[i]),"AIzaSyB_rTK_BcHo7p8I_LOPhxthelriXgijDzw",'fr',langue))
  l_desc <- c(l_desc,translate(as.character(travaux$description[i]),"AIzaSyB_rTK_BcHo7p8I_LOPhxthelriXgijDzw",'fr',langue))
  
  
}

#************************** PARTIE 2 *****************************************
#************************ WEB SCRAPING ***************************************

# lire la page html du site pole-emploi

offres_page <- read_html("https://candidat.pole-emploi.fr/offres/recherche?lieux=01P&offresPartenaires=true&rayon=10&range=1-100&tri=0")

# recuperer toutes les offres afin de trouver le nombre d'offres

offres <- html_nodes(offres_page,".t4.media-heading") %>% html_node('a')

# cr�ation matrice contenant les offres extraites

matri <- matrix(ncol = 13)

for (i in 1:length(offres) )
{
  
  # lien de chaque offre
  
  offre_link <- paste("https://candidat.pole-emploi.fr",html_attr(offres[i],'href'),sep="")
  
  # lire la page html correspondante pour chaque offre
  
  info_page <- read_html(offre_link)
  
  # r�cup�rer le titre de l'offre i en utilisant sa classe "t2 title"
  
  titre <- html_node(info_page,".t2.title") %>% html_text()
  
  # r�cuperer l'adresse de l'offre
  
  adress <- as.list(html_node(info_page,".t4.title-complementary") %>% html_nodes("span"))[3] %>% html_attr("content")
  
  # v�rifeir si l'adresse n'est pas d�finie car certaines valeurs d'adresses peuvent etre vide --> mettre adresse � Non determin�
  
  if(is.na(adress)) { adress_name <- "Non determin�"} else adress_name <- adress
  
  # r�cuperer la date publication de l'offre
  
  date <- as.list(html_node(info_page,".t5.title-complementary") %>% html_nodes("span"))[1] %>% html_text()
  
  #  v�rifeir si la date de publication n'est pas d�finie 
  
  if(is.na(date)) { date_pub <- "Non determin�"} else date_pub <- date
  
  # r�cuperer le num�ro de l'offre
  
  id <- as.list(html_node(info_page,".t5.title-complementary") %>% html_nodes("span"))[4] %>% html_text()
  
  # r�cuperer le nom de la compagnie 
  
  comp <- html_node(info_page,".t4.title") %>% html_text()
  
  #  v�rifeir si le nom de la compagnie est mentionn�
  
  if(is.na(comp)) { company_name <- "Non determin�"} else company_name <- comp
  
  # r�cuperer la description du poste
  
  description <- html_node(info_page,".description") %>% html_text()
  
  # r�cuperer le site web de la compagnie
  
  website_company <- html_node(info_page,".media-body") %>% html_nodes('dl') %>% html_text()
  
  #  v�rifier si le site de la compagnie est mentionn�
  
  if (length(website_company) == 0 ) { site_comp <- "non determin�"} else{ site_comp <- website_company }
  
  # r�cuperer : comment candidater � l'offre
  
  modal_apply <- html_node(info_page,".modal-apply") %>% html_children() #html_chidren() -> voir les balises presentes dans le node ".modal-apply"
  
  apply_mail <- html_node(info_page,".apply-block") %>% html_text()     #recup�ration de l'email du recruteur
  
  apply_link <-html_node(info_page,"#idLienPartenaire") %>% html_attr("href") #recup�ration du site pour postuler � l'offre
  
  if (is.na(apply_mail)) { apply <- apply_link } else { apply <- apply_mail } # si l'email n'est pas mentionn� -> on r�cupere le site de la compagnie
  
  # recuperer : experiences �xig�es pour le poste
  
  infos_comp <- as.list(html_nodes(info_page,".skill-name"))
  

  for (i in 1:length(infos_comp)) {
    
    item <- infos_comp[i] %>% html_attr("itemprop")
    
    if ( !(is.na(item)) )
    {
      if ( item == "experienceRequirements") { experience <- infos_comp[i] %>% html_text() }
      #if ( item == "skills") { competences <- as.character(c(competences,infos_comp[i] %>% html_text())) }
      #else { competences <- "Non sp�cifi�" }
      
    }
   
    
  }
  
  View(t_matri)
  
  #---------------------------------------------------------------------
  
  
  
  infos_dl=list()    # cr�ation d'une liste vide
  #r�cup�ration des informations: contrat, horaires, salaire
  #les balises "dl" contiennent � leurs tour des balises "dd" qui contiennent les valeurs relatives au: contrat, horaires, salaire
  #extract---> permet d'extraire la premiere balise dl retrouv�e
  infos_dl <- as.list(info_page %>% html_nodes("dl") %>% extract2(1) %>% html_nodes("dd")) %>% html_text()
  if (length(infos_dl)==3){           # dans ce cas les 3 donn�es sont disponibles
    contrat<- infos_dl[1]
    horaires <- infos_dl[2]
    salaireee<- infos_dl[3]
  }
  else
  {
    if(not(is.na(infos_dl[1])))         # si au moins on a un element "dd" 
    {
      if (substring(infos_dl[1],1,8)=="\nContrat"){ #si cet element dd renvoie la valeur de "contrat"
        contrat<- infos_dl[1] }
    }
    
    if(not(is.na(infos_dl[2]))) { #dans le cas o� le 2 eme element parmis les trois est present --> c�d on a au moins deux elements pr�sent contrat et (salaire/horaires)
      
      if(substring(infos_dl[2],1,8)=="\nSalaire")   # si cet element represente le salaire (qui est cens� etre le 3eme element si on avait tout les champs)
      { salaireee <- infos_dl[2]                    #recuperer le salaire
      horaires<-"non sp�cifi�" }                 #dans ce cas les horaires ne sont pas sp�cifi�es
      else {horaires<-infos_dl[2]}               #sinon l'emlement en deuxieme position est "horaires"
    }
    
    if((is.na(infos_dl[2])))                # si le deuxieme element n'est pas pr�sent ----> certainement le troisieme ne l'est pas aussi
    {
      salaireee <- "non sp�cifi�"               #salaire non sp�cifi�
      horaires <- "non sp�cif�"                 #horaires non sp�cifi�
    }
    
    
  }
  
  
  #---------------------------------------------------------------------
  #### ajout des differentes infos r�cup�r�es relatives � une offre [i] � la matrice cr�e au pr�lable
  matri <- rbind(matri,c(id,offre_link,titre,adress_name,date_pub,company_name,description,experience,apply,site_comp,contrat,salaireee,horaires))
  
}

t_matri <- as.data.frame(matri[-1,])  #cr�ation d'un dataframe � partir de la matrice

names(t_matri)
#attribution des noms aux differentes colonnes du dataframe
colnames(t_matri)<-c("id","offre_link","titre","adress","date","comp","description","experience","apply","site_comp","contrat","salaire","horaires")


#************************************* PARTIE 3 ****************************************************
#**************** concatenation des resultats de l'API et du web scrapping *************************
# ajout des colonnes salaire, horaire,experience au dataframe "travaux" (car l'API ne contenait pas ces infos)
travaux$salaire="non sp�cifi�"     
travaux$horaires="non sp�cifi�"
travaux$experiences="non sp�cifi�"

#concatenation des colonnes des deux dataframes

offres_emplois <- data.frame()
id <- c(as.character(t_matri$id),travaux$id)          
poste <- c(as.character(t_matri$titre),travaux$title)
url_offre<-c(as.character(t_matri$offre_link),travaux$url)
ville <- c(as.character(t_matri$adress),travaux$location)
compagnie <-c(as.character(t_matri$comp),travaux$company)
url_compagnie<-c(as.character(t_matri$site_comp),travaux$company_url)
date_publication<-c(as.character(t_matri$date),travaux$created_at)
contrat<-c(as.character(t_matri$contrat),travaux$type)
salaire<-c(as.character(t_matri$salaire),travaux$salaire)
horaires<-c(as.character(t_matri$horaires),travaux$horaires)
experience<-c(as.character(t_matri$experience),travaux$experience)

description<-c(as.character(t_matri$description),travaux$description)
candidater<-c(as.character(t_matri$apply),travaux$how_to_apply)

#dataframe final (contient travaux + t_matri)
# emplois sans traduction


offres_emplois <- data.frame('id'=id,'poste'=poste,'url_offre'=url_offre,'ville'=ville,
                             'compagnie'=compagnie,'url_compagnie'=url_compagnie,'date_publication'=date_publication,
                             'contrat'=contrat,'salaire'=salaire,'horaires'=horaires,
                             'experience'=experience,'description'=description,'candidater'=candidater)
#transformation du dataframe en tibble
t_offres_emplois<- as.tibble(offres_emplois)
#importantion sous forme csv et xlsx du dataset

write.csv(t_offres_emplois,"C:/Users/ABCOMPUTER/Desktop/autre/Humanit�s num�riques/offres_emplois.csv", row.names = FALSE)
write.xlsx(t_offres_emplois,"C:/Users/ABCOMPUTER/Desktop/autre/Humanit�s num�riques/offres_emplois.xlsx")


# offres emplois apr�s traduction
### Pr�cision: dans certains cas on arrive pas � faire la traduction de toutes les lignes du dataset
### ces lignes sont compl�t�es par "traduction non effectu�e"
if (length(l_type)<length(travaux$id)){       # on compare la longueur de la liste de la colonne"contrat" obtenue apr�s traduction avec le nombre de lignes du dataset
  # dans ce cas il y a des lignes qui n'ont pas �t� traduite ---> traduction non effectu�e
  contrat_t<-c(as.character(t_matri$contrat),as.character(c(l_type,paste0("Traduction non effecut�",1:length(travaux$contrat)-length(l_type)))))
} else{contrat_t<-c(as.character(t_matri$contrat),as.character(l_type))}


# comme pour "contrat" on fait de m�me pour les attributs "description" et "apply"
if (length(l_desc)<length(travaux$id)){
  description_t<-c(as.character(t_matri$description),as.character(c(l_desc,paste0("Traduction non effecut�",1:(length(travaux$description)-length(l_desc))))))
  
} else{description_t<-c(as.character(t_matri$description),as.character(l_desc))}


if (length(l_apply)<length(travaux$id)){
  candidater_t<-c(as.character(t_matri$apply),as.character(c(l_apply,paste0("Traduction non effecut�",1:(length(travaux$how_to_apply)-length(l_apply))))))
} else{candidater_t<-c(as.character(t_matri$apply),as.character(l_apply))}



offres_emplois_traduit <- data.frame('id'=id,'poste'=poste,'url_offre'=url_offre,'ville'=ville,
                                     'compagnie'=compagnie,'url_compagnie'=url_compagnie,'date_publication'=date_publication,
                                     'contrat'=contrat_t,'salaire'=salaire,'horaires'=horaires,
                                     'experience'=experience,'description'=description_t,'candidater'=candidater_t)


View(offres_emplois_traduit)
# transformation du dataframe en tibble

t_offres_emplois_traduit <- as.tibble(offres_emplois_traduit )

#importation sous forme csv et xlsx du dataset

write.csv(t_offres_emplois_traduit,"C:/Users/ABCOMPUTER/Desktop/autre/Humanit�s num�riques/Projet-humanit�s/offres_emplois_traduit.csv", row.names = FALSE)
write.xlsx(t_offres_emplois_traduit,"C:/Users/ABCOMPUTER/Desktop/autre/Humanit�s num�riques/Projet-humanit�s/offres_emplois_traduit.xlsx")

