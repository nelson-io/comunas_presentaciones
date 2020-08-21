library(sf)
library(tidyverse)
library(ggthemes)
library(rio)
library(googledrive)

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


#ploteamos comunas de ejemplo
ggplot(comunas)+
  geom_sf(aes(fill = COMUNAS == 14))+
  theme_map()+
  scale_fill_manual(values = c("gray", '#F8CF14'))+
  theme(legend.position = "none")
  

# c_14 PALERMO

c_14 <- data.frame(
  category = c('ocupados_formales', 'desocupados', 'inactivos', 'ocupados_informales'),
  count = c(113717,7704, 87206, 18402)
)


#ploteamos

# Create test data.


# Compute percentages
c_14$fraction <- c_14$count / sum(c_14$count)

# Compute the cumulative percentages (top of each rectangle)
c_14$ymax <- cumsum(c_14$fraction)

# Compute the bottom of each rectangle
c_14$ymin <- c(0, head(c_14$ymax, n=-1))

# Compute label position
c_14$labelPosition <- (c_14$ymax + c_14$ymin) / 2

# Compute a good label
c_14$label <- paste0(c_14$category, "\n value: ", c_14$count)

# Make the plot
ggplot(c_14, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_manual(values = c('#3e20ab','#7c5a79','#f8ce14','#caa23a'))+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")



#download eah data

temp <- tempfile()
temp2 <- tempfile()

#download the zip folder from the internet save to 'temp' 

download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2020/06/eah2019_base_usuarios.zip",temp)

#unzip the contents in 'temp' and save unzipped content in 'temp2'

unzip(zipfile = temp, exdir = temp2)

# lets see files extracted

list.files(temp2,full.names=TRUE)

# import data

eah_ind <- import('C:\\Users\\Nelson\\AppData\\Local\\Temp\\RtmpsbPLYT\\file150c2f382ba5/eah2019_usuarios_ind.txt')

#filter column

eah_c14 <- eah_ind %>% 
  filter(comuna == 14) %>% 
  rownames_to_column('id_ind')

# expand_data

expanded <- data.frame(id_ind = rep(eah_c14$id_ind, eah_c14$fexp)) %>% 
  left_join(eah_c14 %>% select(id_ind, t37_coda_2), by = 'id_ind') %>% 
  filter(t37_coda_2 != 0)

actividades <- data.frame(
  code = 1:10,
  actividad = c("Industra Manufacturera", "Construcción", "Comercio", "Transporte y alm", "Act.financieras y prof.", 
                "Admin. Pública", "Enseñanza y salud", "Arte y entretenimiento", "Serv. Doméstico", "Otras")
)

expanded <- expanded %>% 
  left_join(actividades, by = c('t37_coda_2' = 'code'))


prop_empleo_c14 <- data.frame(prop.table(table(expanded$actividad)))

ggplot(prop_empleo_c14)+
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


# read isib db

isib <- import("isib_alic_df.csv")

#Gdrive

drive_auth(email = 'nelsonshilman.gcba@gmail.com', cache = gargle::gargle_oauth_cache())

drive_download(as_id('1y3PCty0DFcGe4YwnD_SqlQaYut-vTlHA'))

cuits <- import("bi_rs_cuit_all.csv") %>% filter(!is.na(latitud)) 

cuits_sf <- st_as_sf(cuits,coords = c('longitud', 'latitud')) %>% st_set_crs(4326)
comunas <- st_transform(comunas, crs = 4326)

#visual check

ggplot(cuits_sf)+geom_sf(data = comunas)+geom_sf()

# join

cuits_sf <- cuits_sf %>% st_join(comunas)


cuits_sf %>% 
  st_drop_geometry() %>% 
  filter(COMUNAS == 14) %>% 
  arrange(desc(baseimp)) %>% 
  head(5) %>% 
  select(CUIT, baseimp, letra, nombre)
  
