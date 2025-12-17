# Tamaño? 
library(tidyverse)

base = read_csv(
  "https://www.datos.gob.mx/dataset/d8f2a534-bcee-4114-853d-82982a81ce24/resource/551d8f48-5d0c-4d11-964a-e2911f732615/download/imm_2020-3.csv"
)

b1 = base %>% 
  select(CVE_ENT,NOM_ENT,CVE_MUN,NOM_MUN,ANALF)

# ¿Cómo está distribuido el analafabetismo en méxico? 
# ¿Hay zonas donde el analfabetismo parece estar agrupado? o es un proceso aleatorio?
b1 %>% 
  ggplot()+
  geom_density(aes(ANALF))

# primer acercamiento
b1 %>% 
  mutate(promedio=mean(ANALF)) %>% 
  group_by(NOM_ENT) %>% 
  mutate(media_edo = mean(ANALF)) %>%
  ungroup() %>% 
  arrange(desc(media_edo)) %>% 
  mutate(NOM_ENT=forcats::fct_reorder(NOM_ENT,media_edo)) %>% 
  ggplot()+
  geom_point(aes(NOM_ENT,ANALF))+
  coord_flip()

## GUERRERO.
# ahora descargar geometrías

library(sf)


b2 = b1 %>% 
  mutate(promedio=mean(ANALF)) %>% 
  group_by(NOM_ENT) %>% 
  mutate(media_edo = mean(ANALF)) %>%
  ungroup() %>% 
  filter(CVE_ENT==12)


edo = sf::st_read("https://gaia.inegi.org.mx/wscatgeo/v2/geo/mgem/12")

mapview::mapview(edo)

edo2= edo %>% 
  filter(!is.na(pob_total)) %>% 
  select(cvegeo)

edo2

final = b2 %>% 
  select(-promedio,-media_edo) %>%
  mutate(CVE_MUN=as.character(CVE_MUN)) %>% 
  left_join(edo2,by=c("CVE_MUN"="cvegeo")) %>% 
  sf::st_as_sf()

# primer paso distribución espacial del analfabetismo.
final %>% 
  ggplot()+
  geom_sf(aes(fill=ANALF))

library(sfdep)

# la estructura del rezago
# pesos espaciales
# reago espacial

paso1= final %>% 
  as_tibble() %>% 
  mutate(id=1:n()) %>%
  mutate(nb=sfdep::st_contiguity(geometry))

paso_intermedio = paso1 %>% 
  filter(CVE_MUN==12001) %>% 
  select(nb) %>% 
  unnest()


paso1 %>%
  filter(id%in%c(21,29,39,53)) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry))

mean(c(12.3,5.49,12.2,14.9))

paso2=paso1 %>% 
  mutate(wt=sfdep::st_weights(nb)) %>% 
  mutate(rezago=st_lag(nb = nb,wt = wt,ANALF))


matriz=paso2 %>%
  st_as_sf() %>% 
  sfdep::st_as_graph() %>% 
  sf::st_as_sf("edges")

ggplot()+
  geom_sf(data=edo)+
  geom_sf(data=matriz)

paso2 %>% 
  ggplot(aes(ANALF,rezago))+
  geom_point()

# I de moran a través de una regresión lineAL
lm(paso2$rezago~paso2$ANALF) %>% 
  summary()

# I de moran GLOBAL
sfdep::global_moran(x = paso2$ANALF,nb = paso2$nb,wt = paso2$wt)

# 0.6105604
sfdep::global_moran_perm(x = paso2$ANALF,
                         nb = paso2$nb,
                         wt = paso2$wt,
                         nsim = 1000)

# sfdep::global_moran_test()

# i de moran global indica SI EXISTE O NO EXISTE
# una distribución "aleatoria o no del espacio" 

# DONDE
# I DE MORAN LOCAL

# AUTOCORRELACIÓN ESPACIAL POSITIVA: Valores similares en posiciones similares

# AUTOCORRELACIÓN ESPACIAL NEGATIVA: VALOR Dis-simlar en posiciones similares

paso2 %>% 
  ggplot(aes(ANALF,rezago))+
  geom_point()+
  geom_smooth(method = "lm")

paso2 %>% 
  mutate(mlocal=sfdep::local_moran(ANALF,nb = nb,wt = wt)) %>%
  unnest(cols = "mlocal") %>% 
  select(CVE_MUN,geometry,pysal) %>% 
  ggplot()+
  geom_sf(aes(fill=pysal,geometry=geometry))+
  viridis::scale_fill_viridis(discrete = TRUE)

paso3=paso2 %>% 
  mutate(mlocal=sfdep::local_moran(ANALF,nb = nb,wt = wt)) %>%
  unnest(cols = "mlocal") %>% 
  select(CVE_MUN,geometry,pysal) %>% 
  sf::st_as_sf()


library(leaflet)
colores_cluster = colorFactor(palette = "viridis",domain = paso3$pysal)

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data=paso3,
              weight = 1,
              fillColor = ~colores_cluster(pysal),fillOpacity = .7) %>% 
  addLegend(pal = colores_cluster,values = paso3$pysal)
