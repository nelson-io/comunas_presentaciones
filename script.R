library(sf)
library(tidyverse)
library(ggthemes)
library(rio)
library(googledrive)
library(janitor)

#create a couple temp files

temp <- tempfile()
temp2 <- tempfile()

#download the zip folder from the internet save to 'temp' 

download.file("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/comunas-zip.zip",temp)

#unzip the contents in 'temp' and save unzipped content in 'temp2'

unzip(zipfile = temp, exdir = temp2)

#finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
#the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml 

SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)

#read the shapefile. 
comunas <- read_sf(SHP_file)


# generate map plots
for(i in 1:nrow(comunas)){
  ggplot(comunas)+
    geom_sf(aes(fill = COMUNAS == i))+
    theme_map()+
    scale_fill_manual(values = c("gray", '#F8CF14'))+
    theme(legend.position = "none")
  
  ggsave(paste0('maps//comuna_',i,'.png'))
}

# Load EAH data

temp <- tempfile()
temp2 <- tempfile()

download.file('https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2020/06/eah2019_base_usuarios.zip',temp)

unzip(zipfile = temp, exdir = temp2)

eah_path<-list.files(temp2, pattern = 'eah2019_usuarios_ind.txt',full.names=TRUE)
eah_2019 <- import(eah_path)

#expand DF

expand_df <- function(df, fexp = 'fexp'){
  #set id
  fexp_df <- df %>% 
    rownames_to_column('id_expand_df') 
  
  ids <- fexp_df %>% pull('id_expand_df')
                          
  fact_exp <- fexp_df %>% pull(fexp)
  
  expanded_df <- data.frame(id = rep(ids,fact_exp))
  
  out_df <- left_join(expanded_df, fexp_df, by = c('id' = 'id_expand_df'))
  return(out_df)  
}

eah_2019_e <- expand_df(eah_2019, 'fexp')

# generate rango_etario
eah_2019_e$rango_etario <- cut(eah_2019_e$edad, breaks = seq(0,100,5),include.lowest = T)

for(i in 1:15){  
  plot_df <- eah_2019_e %>% 
    filter(comuna == i) %>% 
    group_by(sexo, rango_etario) %>% 
    summarise(total = n())
  
  
  
  plot_df$total[plot_df$sexo == 1] <-
    -1 * plot_df$total[plot_df$sexo == 1]
  
  
  plot_df$sexo <- if_else(plot_df$sexo == 1, 'Hombre', 'Mujer')
  
  # Determine breaks
  
  max_total <- max(plot_df$total)
  min_total <- min(plot_df$total)
  by_total <- 1000
  
  ggplot(plot_df, aes(x = rango_etario, y = total, fill = sexo))+
    geom_bar(stat = "identity")+
    scale_y_continuous(breaks = c(rev(seq(0, min_total, -by_total)), seq(0,max_total, by_total)),
                        labels =c(rev(seq(0, -min_total, by_total)), seq(0,max_total, by_total)))+
    coord_flip()+
    scale_fill_manual(values = c('Hombre' = '#434343', 'Mujer' = '#7f6380'))+
    theme_bw()+
    ggtitle(paste0("Piramide Poblacional de la Comuna ", i))+
    ylab("Habitantes")+
    xlab("Rango etario (años)")+
    labs(fill = "Sexo")
  
  ggsave(paste0('pyramids//comuna_',i,'.png'), width = 15, height = 8)
}


#barplots

actividades <- data.frame(
  code = 1:10,
  actividad = c("Industra Manufacturera", "Construcción", "Comercio", "Transporte y alm", "Act.financieras y prof.",
                "Admin. Pública", "Enseñanza y salud", "Arte y entretenimiento", "Serv. Doméstico", "Otras")
)


eah_2019_e <- eah_2019_e %>%
  left_join(actividades, by = c('t37_coda_2' = 'code'))

for(i in 1:15){
  
  data <- eah_2019_e %>% filter(comuna == i)
  
  tot_empleo<- data.frame(table(data$actividad))
  prop_empleo <- data.frame(prop.table(table(data$actividad)))
  
  ggplot(prop_empleo)+
    geom_col(aes(x = reorder(Var1, Freq), y=Freq), fill = "#f8ce14", col = 'black')+
    geom_text(aes(x = reorder(Var1, Freq), y=Freq, label = paste0(round(100*Freq,2)," %")),hjust = -.1)+
    coord_flip()+
    theme_classic()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          line = element_blank(),
          axis.text.y = element_text(hjust=0, face = 'bold', size = 12.5))+
    ylim(c(0,.35))
  
  ggsave(paste0('employment//comuna_',i,'.png'), width = 10, height = 10)
  
}


#importamos datos de censo de comercios

#comercios

comercios <- read_csv('plot_df.csv', locale = locale(encoding = 'UTF-8')) %>% 
  arrange(comuna, desc(total))


# transforaciones

comercios$actividad_comercial[str_detect(comercios$actividad_comercial, 'hogar')] <- 'Artículos para el hogar y construcción'
comercios$actividad_comercial[str_detect(comercios$actividad_comercial, 'Farmacias')] <- 'Farmacias, perfumerías e insumos médicos'
comercios$actividad_comercial[str_detect(comercios$actividad_comercial, 'Hoteles')] <- 'Hoteles, geriátricos y alojamiento'


for(i in 1:15){
  
  data_0 <- comercios %>% filter(comuna == i)
  
  data_1 <- data_0 %>% head(19)
  
  data_2 <- data_0[c(seq(20,nrow(data))),] %>% 
    summarise(comuna = i,
              actividad_comercial = 'Otros',
              total = sum(total))
  
  
  data <- rbind(data_1, data_2)

  
  ggplot(data)+
    geom_col(aes(x = reorder(actividad_comercial, total), y=total), fill = "#f8ce14", col = 'black')+
    geom_text(aes(x = reorder(actividad_comercial, total), y=total, label = total),hjust = -.1)+
    coord_flip()+
    theme_classic()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          line = element_blank(),
          axis.text.y = element_text(hjust=0, face = 'bold', size = 12.5))+
    ggtitle(paste0('Comuna ',i))
  
  ggsave(paste0('establecimientos//comuna_',i,'.png'), width = 16, height = 10)
  
}



#ploteamos comunas de ejemplo
# 
#   
# 
# # c_14 PALERMO
# 
# c_14 <- data.frame(
#   category = c('ocupados_formales', 'desocupados', 'inactivos', 'ocupados_informales'),
#   count = c(113717,7704, 87206, 18402)
# )
# 
# 
# #ploteamos
# 
# # Create test data.
# 
# 
# # Compute percentages
# c_14$fraction <- c_14$count / sum(c_14$count)
# 
# # Compute the cumulative percentages (top of each rectangle)
# c_14$ymax <- cumsum(c_14$fraction)
# 
# # Compute the bottom of each rectangle
# c_14$ymin <- c(0, head(c_14$ymax, n=-1))
# 
# # Compute label position
# c_14$labelPosition <- (c_14$ymax + c_14$ymin) / 2
# 
# # Compute a good label
# c_14$label <- paste0(c_14$category, "\n value: ", c_14$count)
# 
# # Make the plot
# ggplot(c_14, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
#   geom_rect() +
#   geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
#   scale_fill_manual(values = c('#3e20ab','#7c5a79','#f8ce14','#caa23a'))+
#   coord_polar(theta="y") +
#   xlim(c(2, 4)) +
#   theme_void() +
#   theme(legend.position = "none")
# 
# 
# 
# 
# #download eah data
# 
# temp <- tempfile()
# temp2 <- tempfile()
# 
# #download the zip folder from the internet save to 'temp' 
# 
# download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2020/06/eah2019_base_usuarios.zip",temp)
# 
# #unzip the contents in 'temp' and save unzipped content in 'temp2'
# 
# unzip(zipfile = temp, exdir = temp2)
# 
# # lets see files extracted
# 
# list.files(temp2,full.names=TRUE)
# 
# # import data
# 
# eah_ind <- import(paste0(temp2, "/eah2019_usuarios_ind.txt"))
# 
# #filter column
# 
# eah_c14 <- eah_ind %>% 
#   filter(comuna == 14) %>% 
#   rownames_to_column('id_ind')
# 
# # expand_data
# 
# expanded <- data.frame(id_ind = rep(eah_c14$id_ind, eah_c14$fexp)) %>% 
#   left_join(eah_c14 %>% select(id_ind, t37_coda_2), by = 'id_ind') %>% 
#   filter(t37_coda_2 != 0)
# 
# actividades <- data.frame(
#   code = 1:10,
#   actividad = c("Industra Manufacturera", "Construcción", "Comercio", "Transporte y alm", "Act.financieras y prof.",
#                 "Admin. Pública", "Enseñanza y salud", "Arte y entretenimiento", "Serv. Doméstico", "Otras")
# )
# 
# expanded <- expanded %>%
#   left_join(actividades, by = c('t37_coda_2' = 'code'))
# 
# 
# tot_empleo_c14 <- data.frame(table(expanded$actividad))
# prop_empleo_c14 <- data.frame(prop.table(table(expanded$actividad)))
# 
# ggplot(prop_empleo_c14)+
#   geom_col(aes(x = reorder(Var1, Freq), y=Freq), fill = "#f8ce14", col = 'black')+
#   geom_text(aes(x = reorder(Var1, Freq), y=Freq, label = paste0(round(100*Freq,2)," %")),hjust = -.1)+
#   coord_flip()+
#   theme_classic()+
#   theme(axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         line = element_blank(),
#         axis.text.y = element_text(hjust=0, face = 'bold', size = 12.5))+
#   ylim(c(0,.35))

# 
# # read isib db
# 
# isib <- import("isib_alic_df.csv")
# 
# #Gdrive
# 
# drive_auth(email = 'nelsonshilman.gcba@gmail.com', cache = gargle::gargle_oauth_cache())
# 
# drive_download(as_id('1y3PCty0DFcGe4YwnD_SqlQaYut-vTlHA'))
# 
# cuits <- import("bi_rs_cuit_all.csv") %>% filter(!is.na(latitud)) 
# 
# cuits_sf <- st_as_sf(cuits,coords = c('longitud', 'latitud')) %>% st_set_crs(4326)
# comunas <- st_transform(comunas, crs = 4326)
# 
# #visual check
# 
# ggplot(cuits_sf)+geom_sf(data = comunas)+geom_sf()
# 
# # join
# 
# cuits_sf <- cuits_sf %>% st_join(comunas)
# 
# 
# cuits_sf %>% 
#   st_drop_geometry() %>% 
#   filter(COMUNAS == 14) %>% 
#   arrange(desc(baseimp)) %>% 
#   head(5) %>% 
#   select(CUIT, baseimp, letra, nombre)
#   
# 




