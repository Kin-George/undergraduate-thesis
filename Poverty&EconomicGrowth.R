# Replication code for the undergraduate thesis
# Author: Jorge M. Orozco

# Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(ggthemes)

# Fig 1-2-3-4: evolution of poverty and per capita gdp
#### DATA ###
# Calculation of GDP per capita using municipal value added data and population data from DANE.
pibmunicipal <- read_excel("databases/anex-PIBDep-ValorAgreMuni-2011-2022p.xlsx", 
                                                   sheet = "Cuadro 1", skip = 10)
pibmunicipal<-na.omit(pibmunicipal)
# List of metropolitan areas according to DANE
areas_metro <- list(
  `Bogota, D.C` = c("Bogotá, D.C."),
  `Barranquilla A.M` = c("Barranquilla", "Malambo", "Galapa", "Puerto Colombia", "Soledad"),
  `Bucaramanga A.M` = c("Bucaramanga", "Floridablanca", "Girón", "Piedecuesta"),
  `Cali A.M` = c("Cali", "Jamundí", "Puerto Tejada"),
  `Cartagena` = c("Cartagena de Indias"),
  `Medellin A.M` = c("Medellín", "Barbosa", "Bello", "Caldas", "Copacabana", "Envigado", "Girardota", "Itagüí", "La Estrella", "Sabaneta")
)

# select the municipalities to metropolitan areas
pib_metro <- pibmunicipal %>%
  rowwise() %>%
  mutate(AM = ifelse(
    any(sapply(areas_metro, function(x) Municipio %in% x)),
    names(areas_metro)[sapply(areas_metro, function(x) Municipio %in% x)],
    NA_character_
  )) %>%
  ungroup()

# Sum of the annual aggregate municipal value
pib_final <- pib_metro %>%
  group_by(AM) %>%
  summarise(across(`2012`:`2021p`, sum, na.rm = TRUE))
# eliminate the extra value
pib_final<-na.omit(pib_final)

# Long format
pib <- pib_final %>%
  pivot_longer(
    cols = starts_with("20"), 
    names_to = "Año",         
    values_to = "PIB"         
  )

# remove p of the character
pib <- pib %>%
  mutate(Año = as.numeric(gsub("p", "", Año)))

# aggregate total national data
total_nacional <- data.frame(
  AM = "Total Nacional",
  Año = 2012:2021,
  PIB = c(666507, 714093, 762903, 804692, 863782, 920471, 987791, 1060068, 998471, 1192634)
)

pib <- bind_rows(pib, total_nacional)

# Population
pobmunicipal <- read_excel("databases/Municipal_area_1985-2020.xls", 
                                       sheet = "Mpios", col_names = FALSE, skip = 9)
# select columns of interest (2010-2021)
pobmunicipal <- pobmunicipal %>%
  select(1:4, 30:40) 

# firts row as colnames
colnames(pobmunicipal) <- as.character(unlist(pobmunicipal[1, ])) 
pobmunicipal <- pobmunicipal[-1, ]

# do the same as pib but with total national

areas_metro2 <- list(
  `Bogota, D.C` = c("Bogotá, D.C."),
  `Barranquilla A.M` = c("Barranquilla", "Malambo", "Galapa", "Puerto Colombia", "Soledad"),
  `Bucaramanga A.M` = c("Bucaramanga", "Floridablanca", "Girón", "Piedecuesta"),
  `Cali A.M` = c("Cali", "Jamundí", "Puerto Tejada"),
  `Cartagena` = c("Cartagena"),
  `Medellin A.M` = c("Medellín", "Barbosa", "Bello", "Caldas", "Copacabana", "Envigado", "Girardota", "Itagüí", "La Estrella", "Sabaneta"),
  `Total Nacional` = c("Total Nacional")
)

pob_metro <- pobmunicipal %>%
  rowwise() %>%
  mutate(AM = ifelse(
    any(sapply(areas_metro2, function(x) MPIO %in% x)),
    names(areas_metro2)[sapply(areas_metro2, function(x) MPIO %in% x)],
    NA_character_
  )) %>%
  ungroup()

pob_final <- pob_metro %>%
  group_by(AM) %>%
  summarise(across(`2012`:`2020`, sum, na.rm = TRUE))
# eliminate the extra value
pob_final<-na.omit(pob_final)

# Long format
pob <- pob_final %>%
  pivot_longer(
    cols = starts_with("20"), 
    names_to = "Año",         
    values_to = "Poblacion"   
  ) %>%
  mutate(Año = as.numeric(Año))

# Merge to pib
pibpob <- left_join(pib, pob, by = c("AM", "Año"))

# Calculate the gdp per capita
pibpob$PIBpc<-(pibpob$PIB*1000000/pibpob$Poblacion)

# Poverty
pov<-read_excel("databases/anexo_pobreza_monetaria_21_nacional.xls", 
                                                              sheet = "Pobreza Monetaria Act.Met.", 
                                                              range = "A15:K29")
colnames(pov)<-c("AM", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
# Long format
pov <- pov %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "Año",          
    values_to = "Pov"          
  ) %>%
  mutate(Año = as.numeric(Año))

# rename
pov <- pov %>%
  mutate(AM = case_when(
    AM == "Barranquilla A.M." ~ "Barranquilla A.M",
    AM == "Bogotá" ~ "Bogota, D.C",
    AM == "Bucaramanga A.M." ~ "Bucaramanga A.M",
    AM == "Cali A.M." ~ "Cali A.M",
    AM == "Medellín A.M." ~ "Medellin A.M",
    AM == "Cartagena" ~ "Cartagena",
    AM == "Nacional" ~ "Total Nacional",
    TRUE ~ AM # Mantener los valores que no coincidan con ningún caso anterior
  ))

# Merge to principal data
pibpob <- pibpob %>%
  left_join(pov, by = c("AM", "Año"))

# Extreme poverty
pov2<-read_excel("databases/anexo_pobreza_monetaria_21_nacional.xls", 
                sheet = "Pobreza Monetaria Act.Met.", 
                range = "A44:K58")

colnames(pov2)<-c("AM", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

pov2 <- pov2 %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "Año",          
    values_to = "PovEx"          
  ) %>%
  mutate(Año = as.numeric(Año))

pov2 <- pov2 %>%
  mutate(AM = case_when(
    AM == "Barranquilla A.M." ~ "Barranquilla A.M",
    AM == "Bogotá" ~ "Bogota, D.C",
    AM == "Bucaramanga A.M." ~ "Bucaramanga A.M",
    AM == "Cali A.M." ~ "Cali A.M",
    AM == "Medellín A.M." ~ "Medellin A.M",
    AM == "Cartagena" ~ "Cartagena",
    AM == "Nacional" ~ "Total Nacional",
    TRUE ~ AM # Mantener los valores que no coincidan con ningún caso anterior
  ))

pibpob <- pibpob %>%
  left_join(pov2, by = c("AM", "Año"))

# write xlsx
write.xlsx(pibpob, "Pov-PBC.xlsx")
# Fill the NA values of gdp per capita with lineal interpolation in excel
data1<-read_excel("POV-PBC.xlsx")

# PLOT fig1
databaq<-subset(data1, AM=="Barranquilla A.M")
# Img
factor<-450
ggplot(databaq, aes(x = Año)) +
  geom_line(aes(y = Pov, color = "Poverty"), size = 1.2) +
  geom_line(aes(y = PovEx, color = "Extreme Poverty"), size = 1.2) +
  geom_line(aes(y = PIBpc / factor, color = "GDP per capita"), size = 1.2, linetype = 5) +
  scale_y_continuous(
    name = "Poverty (%)",
    sec.axis = sec_axis(~ . * factor, name = "GDP per capita (Thousand pesos)")
  ) +
  scale_color_manual(
    values = c("Poverty" = "darkred", "Extreme Poverty" = "red", "GDP per capita" = "black"), 
    name = "Indicators"
  ) +
  scale_x_continuous(breaks = c(2011, 2013, 2016, 2019, 2021)) +
  labs(
    title = "",
    subtitle = "",
    y = "Poverty (%)",
    x = ""
  ) +
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    plot.subtitle = element_text(color = "darkblue", size = 12, face = "italic") 
  )

# PLOT fig2
data1 %>%
  mutate(data1, isBarranquilla = AM=="Barranquilla A.M") %>%
  ggplot(aes(x=Año, y=Pov, color=AM))+
  geom_line(aes(linetype=isBarranquilla), linewidth=1.25, alpha=0.8)+
  theme_bw()+
  scale_x_continuous(breaks=c(2011,2013,2016,2019,2021))+
  labs(title="",
       subtitle="",
       y="Poverty (%)",
       x="",
       color="Metropolitan area")+
  theme(panel.grid.major.x = element_blank(),
        axis.title=element_text(),
        plot.caption.position="plot",legend.position="bottom")+
  scale_color_brewer(palette="Set2")+
  scale_linetype_manual(values=c("dashed","solid"),guide="none")

# PLOT fig3
data2 <- data1 %>%
  select(Año, PIBpc, AM) %>%
  arrange(Año, AM)

# Calculate the index
data2 <- data2 %>%
  group_by(AM) %>% # 
  mutate(IndicePIBpc = if_else(Año == 2012, 100, (PIBpc / PIBpc[Año == 2012]) * 100)) %>% 
  ungroup()

data2 %>%
  mutate(data2, isBarranquilla = AM=="Barranquilla A.M") %>%
  ggplot(aes(x=Año, y=IndicePIBpc, color=AM))+
  geom_line(aes(linetype=isBarranquilla), linewidth=1.25, alpha=0.8)+
  theme_bw()+
  scale_x_continuous(breaks=c(2012, 2014, 2016, 2018, 2020, 2021))+
  labs(title="",
       subtitle="",
       x="",
       y="Porcentual Var. (%)",
       color="Metropolitan area",
       )+
  theme(panel.grid.major.x = element_blank(),
        axis.title=element_text(),
        plot.caption.position="plot",legend.position="bottom")+
  scale_color_brewer(palette="Set2")+
  scale_linetype_manual(values=c("dashed","solid"),guide="none")

# PLOT fig4
factor2=9
data2 %>%
  mutate(data2, isBarranquilla = AM=="Barranquilla A.M") %>%
  ggplot(aes(x=Año, y=PIBpc/factor2, color=AM))+
  geom_line(aes(linetype=isBarranquilla), linewidth=1.25, alpha=0.8)+
  theme_bw()+
  scale_x_continuous(breaks=c(2012, 2014, 2016, 2018, 2020, 2021))+
  labs(title="",
       subtitle="",
       x="",
       y="GDP per capita (Thousand pesos)",
       color="Metropolitan area",
  )+
  theme(panel.grid.major.x = element_blank(),
          axis.title=element_text(),
          plot.caption.position="plot",legend.position="bottom")+
  scale_color_brewer(palette="Set2")+
  scale_linetype_manual(values=c("dashed","solid"),guide="none")

# Fig 5: Social class analysis of poverty
### DATA
pobres <- read_excel("databases/anexo-pobreza-monetaria-caracterizacion-clases-sociales-2021.xls", 
                                                                           sheet = "Clases Sociales % Act.Met", 
                                                                           range = "A15:K39")
colnames(pobres)<-c("AM", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

# Poors
pobres <- pobres %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "Año",          
    values_to = "Pobres"          
  ) %>%
  mutate(Año = as.numeric(Año))

pobres <- pobres %>%
  mutate(AM = case_when(
    AM == "Barranquilla A.M." ~ "Barranquilla A.M",
    AM == "Bogotá" ~ "Bogota, D.C",
    AM == "Bucaramanga A.M." ~ "Bucaramanga A.M",
    AM == "Cali A.M." ~ "Cali A.M",
    AM == "Medellín A.M." ~ "Medellin A.M",
    AM == "Cartagena" ~ "Cartagena",
    AM == "Nacional" ~ "Total Nacional",
    TRUE ~ AM # Mantener los valores que no coincidan con ningún caso anterior
  ))

# almost poors
vulnerables <- read_excel("databases/anexo-pobreza-monetaria-caracterizacion-clases-sociales-2021.xls", 
                     sheet = "Clases Sociales % Act.Met", 
                     range = "A51:K75")
colnames(vulnerables)<-c("AM", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

vulnerables <- vulnerables %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "Año",          
    values_to = "Vulnerables"          
  ) %>%
  mutate(Año = as.numeric(Año))

vulnerables <- vulnerables %>%
  mutate(AM = case_when(
    AM == "Barranquilla A.M." ~ "Barranquilla A.M",
    AM == "Bogotá" ~ "Bogota, D.C",
    AM == "Bucaramanga A.M." ~ "Bucaramanga A.M",
    AM == "Cali A.M." ~ "Cali A.M",
    AM == "Medellín A.M." ~ "Medellin A.M",
    AM == "Cartagena" ~ "Cartagena",
    AM == "Nacional" ~ "Total Nacional",
    TRUE ~ AM # Mantener los valores que no coincidan con ningún caso anterior
  ))

pobres <- pobres %>%
  left_join(vulnerables, by = c("AM", "Año"))

# middle class
media <- read_excel("databases/anexo-pobreza-monetaria-caracterizacion-clases-sociales-2021.xls", 
                          sheet = "Clases Sociales % Act.Met", 
                          range = "A87:K111")
colnames(media)<-c("AM", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

media <- media %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "Año",          
    values_to = "Clase media"          
  ) %>%
  mutate(Año = as.numeric(Año))

media <- media %>%
  mutate(AM = case_when(
    AM == "Barranquilla A.M." ~ "Barranquilla A.M",
    AM == "Bogotá" ~ "Bogota, D.C",
    AM == "Bucaramanga A.M." ~ "Bucaramanga A.M",
    AM == "Cali A.M." ~ "Cali A.M",
    AM == "Medellín A.M." ~ "Medellin A.M",
    AM == "Cartagena" ~ "Cartagena",
    AM == "Nacional" ~ "Total Nacional",
    TRUE ~ AM # Mantener los valores que no coincidan con ningún caso anterior
  ))

pobres <- pobres %>%
  left_join(media, by = c("AM", "Año"))

# High class
alta <- read_excel("databases/anexo-pobreza-monetaria-caracterizacion-clases-sociales-2021.xls", 
                    sheet = "Clases Sociales % Act.Met", 
                    range = "A123:K147")
colnames(alta)<-c("AM", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

alta <- alta %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "Año",          
    values_to = "Clase alta"          
  ) %>%
  mutate(Año = as.numeric(Año))

alta <- alta %>%
  mutate(AM = case_when(
    AM == "Barranquilla A.M." ~ "Barranquilla A.M",
    AM == "Bogotá" ~ "Bogota, D.C",
    AM == "Bucaramanga A.M." ~ "Bucaramanga A.M",
    AM == "Cali A.M." ~ "Cali A.M",
    AM == "Medellín A.M." ~ "Medellin A.M",
    AM == "Cartagena" ~ "Cartagena",
    AM == "Nacional" ~ "Total Nacional",
    TRUE ~ AM # Mantener los valores que no coincidan con ningún caso anterior
  ))

pobres <- pobres %>%
  left_join(alta, by = c("AM", "Año"))

# PLOT fig5
data3 <- pobres %>%
  pivot_longer(cols = c(Pobres, Vulnerables, `Clase media`, `Clase alta`),
               names_to = "Clase_Social",
               values_to = "Porcentaje")

ciudades_interes <- c("Barranquilla A.M", "Bogota, D.C", "Bucaramanga A.M", 
                      "Cali A.M", "Medellin A.M", "Cartagena", "Total Nacional")

data3 <- data3 %>%
  filter(AM %in% ciudades_interes)

ggplot(data3, aes(x = Año, y = Porcentaje, fill = Clase_Social)) +
  geom_bar(stat = "identity", position = "fill", width=0.5) +
  facet_wrap(~ AM, ncol = 3) +  
  scale_fill_manual(values = c("green", "lightgreen", "blue", "darkblue")) +
  labs(title = "",
       x = "",
       y = "Proportion (%)",
       fill = "Social class") +
  theme_minimal()

# Fig 6: Multidimensional poverty index
# In this I already have done the poblation ponderation of IPM of each municipalitie to conform the metropolitan area
# See details in database "IPM ponderado por población.xlsx" archive.

Quinto_grafico <- read_excel("databases/Quinto grafico.xlsx")
Quinto_grafico %>%
  ggplot(aes(x=reorder(Ciudad,-IPM),y=IPM,fill=IPM))+
  geom_bar(stat="identity", width=0.5)+
  geom_label(aes(x=Ciudad,y=IPM,label=IPM),
             position=position_stack(vjust=1),
             fill="white")+
  theme_bw()+
  labs(title="",
       subtitle="",
       x="",
       y="Proportion (%)",
       tag="",
       caption="")+
  theme(panel.grid.major.x = element_blank(),
        axis.title=element_text(),
        plot.caption.position="plot",legend.position="right",
        axis.text.x = element_text(size = 7))+
  scale_color_brewer(palette="Set2")+
  scale_linetype_manual(values=c("dashed","solid"),guide="none")

# Fig 7-8-9-10-11-12: Social class analysis of labor
# For this I already have done the analysis of labor informality in another archive.
# Details in manual of methodology of DANE for this.
# The STATA dofile complement this code.

# Fig 13-14-15
# PLOT fig13
data <- data.frame(
  Efecto = c("Growth", "Redistribution", "Inflation", "Total change"),
  Proporcion = c(-5.15, -2.64, 2.84, -4.95)  # Valores correspondientes a cada efecto
)

data$Efecto <- factor(data$Efecto, levels = c("Growth", "Redistribution", "Inflation", "Total change"))
# Crear el gráfico
ggplot(data, aes(x = Efecto, y = Proporcion, fill = Proporcion > 0)) +
  geom_bar(stat = "identity", width = 0.7) +  # Gráfico de barras
  geom_text(aes(label = round(Proporcion, 2)), vjust = ifelse(data$Proporcion > 0, -0.5, 1.5), size = 3) +  # Etiquetas de texto
  scale_fill_manual(values = c("darkblue", "darkblue")) +  # Colores para barras negativas y positivas
  labs(
    title = "",
    subtitle = "",
    x = "Effect",
    y = "Proportion (%)"
  ) +
  theme_bw() +  # Estilo minimalista
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "none"
  )

# PLOT fig14
data <- data.frame(
  Efecto = c("Growth", "Redistribution", "Inflation", "Total change"),
  Proporcion = c(-9.79, -1.61, 7.81, -3.59)  # Valores correspondientes a cada efecto
)

data$Efecto <- factor(data$Efecto, levels = c("Growth", "Redistribution", "Inflation", "Total change"))
# Crear el gráfico
ggplot(data, aes(x = Efecto, y = Proporcion, fill = Proporcion > 0)) +
  geom_bar(stat = "identity", width = 0.7) +  # Gráfico de barras
  geom_text(aes(label = round(Proporcion, 2)), vjust = ifelse(data$Proporcion > 0, -0.5, 1.5), size = 3) +  # Etiquetas de texto
  scale_fill_manual(values = c("darkblue", "darkblue")) +  # Colores para barras negativas y positivas
  labs(
    title = "",
    subtitle = "",
    x = "Effect",
    y = "Proportion (%)"
  ) +
  theme_bw() +  # Estilo minimalista
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "none"
  )

# PLOT fig15
data <- data.frame(
  Efecto = c("Growth", "Redistribution", "Inflation", "Total change"),
  Proporcion = c(-5.34, 1.17, 2.72, -1.46)  # Valores correspondientes a cada efecto
)

data$Efecto <- factor(data$Efecto, levels = c("Growth", "Redistribution", "Inflation", "Total change"))
# Crear el gráfico
ggplot(data, aes(x = Efecto, y = Proporcion, fill = Proporcion > 0)) +
  geom_bar(stat = "identity", width = 0.7) +  # Gráfico de barras
  geom_text(aes(label = round(Proporcion, 2)), vjust = ifelse(data$Proporcion > 0, -0.5, 1.5), size = 3) +  # Etiquetas de texto
  scale_fill_manual(values = c("darkblue", "darkblue")) +  # Colores para barras negativas y positivas
  labs(
    title = "",
    subtitle = "",
    x = "Effect",
    y = "Proportion (%)"
  ) +
  theme_bw() +  # Estilo minimalista
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "none"
  )

# PLoT fig16
data <- data.frame(
  Efecto = c("Growth", "Redistribution", "Inflation", "Total change"),
  Proporcion = c(3.45, 1.80, 4.23, 9.48)  # Valores correspondientes a cada efecto
)

data$Efecto <- factor(data$Efecto, levels = c("Growth", "Redistribution", "Inflation", "Total change"))
# Crear el gráfico
ggplot(data, aes(x = Efecto, y = Proporcion, fill = Proporcion > 0)) +
  geom_bar(stat = "identity", width = 0.7) +  # Gráfico de barras
  geom_text(aes(label = round(Proporcion, 2)), vjust = ifelse(data$Proporcion > 0, -0.5, 1.5), size = 3) +  # Etiquetas de texto
  scale_fill_manual(values = c("darkblue", "darkblue")) +  # Colores para barras negativas y positivas
  labs(
    title = "",
    subtitle = "",
    x = "Effect",
    y = "Proportion (%)"
  ) +
  theme_bw() +  # Estilo minimalista
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "none"
  )
