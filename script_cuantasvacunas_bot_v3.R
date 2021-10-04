lapply(c("janitor", "lubridate", "glue", "gt", "patchwork", "httr", "rtweet",
         "scales","tidyverse", "zoo", "reticulate", "colorspace", "sf",
         "jsonlite", "rlist"), require, character.only=T)


####################### PRIMER TUIT #################################


vacunados <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  group_by(denominacion) %>% 
  summarize(primera_dosis_cantidad = sum(primeradosis, na.rm = TRUE), 
            segunda_dosis_cantidad=sum(esquemacompleto, na.rm=TRUE), 
            poblacion=max(poblacion)) %>% 
  mutate(pob_dosis_1 = primera_dosis_cantidad / poblacion * 100, 
         pob_dosis_2 = segunda_dosis_cantidad / poblacion * 100,
         pob_soloprimera = pob_dosis_1 - pob_dosis_2)

ymax <- max(vacunados$pob_dosis_1) + 0.1

vacunas_diarias <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  group_by(fecha, denominacion) %>% 
  summarize(primeradosis = sum(primeradosis), 
            segundadosis=sum(segundadosis), 
            poblacion=max(poblacion)) %>% 
  mutate(fecha=ymd(substr(fecha, start=1, stop=10)), 
         primeradosis_mm = rollmean(primeradosis, 7, align="right", fill=NA), 
         segundadosis_mm = rollmean(primeradosis, 7, align="right", fill=NA), 
         primeradosis_pob = primeradosis / poblacion *100,
         segundadosis_pob = segundadosis / poblacion *100) %>% 
  group_by(denominacion) %>% 
  mutate(primeradosis_acum = cumsum(primeradosis), 
         segundadosis_acum = cumsum(segundadosis), 
         primeradosis_pob_acum = cumsum(primeradosis_pob), 
         segundadosis_pob_acum = cumsum(segundadosis_pob))

fecha_ayer <- max(vacunas_diarias$fecha)

dia <- fecha_ayer %>% as.character() %>% substr(start=9, stop=10)
mes <- fecha_ayer %>% as.character() %>% substr(start=6, stop=7)
anio <- fecha_ayer %>% as.character() %>% substr(start=3, stop=4)

fecha_latina <- paste0(dia, "/", mes, "/", anio)

breaks_vacunados <- case_when(max(vacunados$pob_dosis_1)<10 ~1,
                              max(vacunados$pob_dosis_1) > 10 & max(vacunados$pob_dosis_1) < 20 ~ 2,
                              max(vacunados$pob_dosis_1) > 20 & max(vacunados$pob_dosis_1) < 50 ~ 5, 
                              TRUE ~ 10)

jsonlite::fromJSON(url("https://covidstats.com.ar/ws/vacunadosedades")) %>% 
  rlist::list.rbind() %>% 
  t() %>% 
  as.data.frame() %>% 
  select(-idprovincia) %>% 
  group_by(provincia) %>% 
  summarize(poblacion = sum(as.numeric(personas)), 
            dosis1=sum(as.numeric(dosis1)), 
            dosis2=sum(as.numeric(dosis2)), 
            esquemacompleto =sum(as.numeric(esquemacompleto))) %>% 
  mutate(pob_dosis_1 = dosis1/poblacion*100, 
         pob_dosis_2 = dosis2/poblacion*100,
         pob_completo = esquemacompleto / poblacion*100, 
         pob_incompleto = pob_dosis_1 - pob_completo) %>% 
  rename(denominacion = provincia) %>%
  arrange(pob_dosis_1) %>% mutate(orden=row_number()) %>% 
  pivot_longer(cols=c("pob_incompleto", "pob_completo"), names_to = "dosis", 
               values_to = "cantidad") %>% 
  # mutate(dosis=factor(dosis, levels=c("pob_soloprimera", "pob_dosis_2"))) %>% 
  ggplot(mapping=aes(x=reorder(denominacion, orden))) +
  geom_col(aes(y=cantidad, fill=dosis), alpha = 0.9) + 
  geom_text(aes(label = paste0(as.character(round(cantidad,2)), "%"), 
                y= cantidad),
            position = position_stack(vjust = .5), 
            size=3) + coord_flip() +  theme_light()+
  labs(title=paste0("Porcentaje de la población vacunado por provincia, ",fecha_latina), x="", y="", 
       fill="") +
  scale_y_continuous(labels=label_number(accuracy = 1, suffix="%"), breaks=seq(0,ymax,by=breaks_vacunados)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0)) +
  scale_fill_manual(breaks = c("pob_incompleto", "pob_completo"),
                    values=c("steelblue3","palegreen3"), 
                    labels=c("Vacunación parcial","Vacunación completa")) +
  ggsave(filename="vacunados_provincias.png", height = 6, width = 9.44)


break_fechas_porpcia <- case_when(fecha_ayer - ymd("2021-01-19")< days(60) ~ "2 week",
                                  fecha_ayer - ymd("2021-01-19")> days(60) & fecha_ayer - ymd("2021-01-19")< days(120) ~ "1 month",
                                  fecha_ayer - ymd("2021-01-19")> days(120) & fecha_ayer - ymd("2021-01-19")< days(180) ~ "2 month",
                                  TRUE ~ "4 month")

vacunas_diarias %>% 
  mutate(dosis_diarias = primeradosis+ segundadosis, 
         dosis_diarias_pob = dosis_diarias/poblacion * 100) %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  group_by(denominacion) %>% 
  mutate(dosis_diarias_pob_mm = rollmean(dosis_diarias_pob, 7, align="right", fill=NA)) %>% 
  filter(fecha>ymd("2021-01-25")) %>% 
  ggplot(aes(x=fecha))+
  # geom_col(aes(y=dosis_diarias_pob), fill="gray44", alpha=0.3)+
  geom_line(aes(y=dosis_diarias_pob_mm), color="steelblue3", size=1)+
  geom_ribbon(aes(ymin=0,ymax=dosis_diarias_pob_mm), fill="steelblue3", alpha=0.2)+
  facet_wrap(~denominacion, ncol=6) +
  theme_light()+
  scale_x_date( date_breaks = break_fechas_porpcia,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(suffix="%", accuracy=0.1)) +
  labs(x="", y="Dosis diarias totales, % de la población, media movil 7 dias", 
       title= paste0("Dosis diarias reportadas por provincia, ",fecha_latina)) +
  theme(strip.text = element_text(face="bold", hjust = 0.5, color="black"), 
        strip.background = element_rect(fill="white", color="gray66"),
        plot.title =   element_text(face="bold", hjust = 0.5)) +
  ggsave(filename="vacunados_diarios_provincias.png", height = 6, width = 9.44)

break_fechas_totales <- case_when(fecha_ayer - ymd("2021-01-19")< days(80) ~ "1 week",
                                  fecha_ayer - ymd("2021-01-19")> days(80) & fecha_ayer - ymd("2021-01-19")< days(180) ~ "2 week",
                                  TRUE ~ "1 month")


fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  mutate(fecha = ymd(substr(fecha, start=1, stop=10)), 
         totaldiario=primeradosis+segundadosis, 
         tipovacuna_simple = ifelse(tipovacuna_simple == "Covishield", "AstraZeneca", tipovacuna_simple)) %>% 
  group_by(fecha, tipovacuna_simple) %>% 
  summarize(totaldiario = sum(totaldiario)) %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  mutate(tipovacuna_simple = factor(tipovacuna_simple, levels=c("Pfizer", "Cansino", "Moderna", "Sinopharm",
                                                                "AstraZeneca", 
                                                                "Sputnik"))) %>% 
  ggplot(aes(x=fecha, y =totaldiario, fill=tipovacuna_simple))+
  geom_col(alpha=0.8) +
  scale_fill_manual(breaks = c("Sputnik", "AstraZeneca", 
                               "Sinopharm", "Moderna", "Cansino", "Pfizer"),
                    values=c("indianred3","steelblue3", "springgreen3", "yellow4", "palevioletred3", "darkorchid3"), 
                    labels=c("Sputnik V","AstraZeneca", "Sinopharm", "Moderna", "Cansino", "Pfizer"))+
  theme_light() + labs(x="", y="", title=paste0("Total de vacunas reportadas por dia, ",fecha_latina), fill="Vacuna") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0)) + guides(fill = guide_legend(nrow = 1)) +
  ggsave(filename="vacunados_totales_diarios.png", height = 6, width = 9.44)



a <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  mutate(fecha = ymd(substr(fecha, start=1, stop=10))) %>% 
  pivot_longer(cols=c("primeradosis", "segundadosis"), names_to = "orden", values_to = "cantidad") %>% 
  group_by(fecha, orden) %>% 
  summarize(total = sum(cantidad)) %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  ggplot(aes(x=fecha, y =total, fill=orden))+
  geom_col(alpha=0.8, show.legend = FALSE) +
  scale_fill_manual(breaks = c("primeradosis", "segundadosis"),
                    values=c("steelblue3", "springgreen3"), 
                    labels=c("Primera dosis","Segunda dosis"))+
  theme_light() + labs(x="", y="", title=paste0("Dosis totales reportadas por día por orden de dosis, ",fecha_latina), fill="Dosis") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0))


b <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  mutate(fecha = ymd(substr(fecha, start=1, stop=10))) %>% 
  pivot_longer(cols=c("primeradosis", "segundadosis"), names_to = "orden", values_to = "cantidad") %>% 
  group_by(fecha, orden) %>% 
  summarize(total = sum(cantidad)) %>% 
  ungroup() %>% group_by(fecha) %>% 
  mutate(totaldiario = sum(total), 
         proporcion = total/totaldiario*100) %>% ungroup() %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  ggplot(aes(x=fecha, y =proporcion, fill=orden))+
  geom_area(alpha=0.8) +
  scale_fill_manual(breaks = c("primeradosis", "segundadosis"),
                    values=c("steelblue3", "springgreen3"), 
                    labels=c("Primera dosis","Segunda dosis"))+
  theme_light() + labs(x="", y="", title="", fill="Dosis") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(suffix="%", accuracy=1))+
  theme(plot.title = element_blank(), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0))

combinado_dosis <- a/b

ggsave(combinado_dosis, filename="combinado_dosis.png", height=6, width=9.44)

c <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  mutate(fecha = ymd(substr(fecha, start=1, stop=10)), 
         totaldiario=primeradosis+segundadosis, 
         tipovacuna_simple = ifelse(tipovacuna_simple == "Covishield", "AstraZeneca", tipovacuna_simple)) %>% 
  group_by(fecha, tipovacuna_simple) %>% 
  summarize(totaldiario = sum(totaldiario)) %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  mutate(tipovacuna_simple = factor(tipovacuna_simple, levels=c("Pfizer", "Cansino", "Moderna", "Sinopharm",
                                                                "AstraZeneca", 
                                                                "Sputnik"))) %>% 
  ggplot(aes(x=fecha, y =totaldiario, fill=tipovacuna_simple))+
  geom_col(alpha=0.8) +
  scale_fill_manual(breaks = c("Sputnik", "AstraZeneca", 
                               "Sinopharm", "Moderna", "Cansino", "Pfizer"),
                    values=c("indianred3","steelblue3", "springgreen3", "yellow4", "palevioletred3", "darkorchid3"), 
                    labels=c("Sputnik V","AstraZeneca", "Sinopharm", "Moderna", "Cansino", "Pfizer"))+
  theme_light() + labs(x="", y="", title=paste0("Dosis totales reportadas por día por tipo de vacuna, ",fecha_latina), fill="Vacuna") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0)) + guides(fill = guide_legend(nrow = 1))

d <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  mutate(fecha = ymd(substr(fecha, start=1, stop=10)), 
         totaldiario=primeradosis+segundadosis, 
         tipovacuna_simple = ifelse(tipovacuna_simple == "Covishield", "AstraZeneca", tipovacuna_simple)) %>% 
  group_by(fecha, tipovacuna_simple) %>% 
  summarize(totaldiario = sum(totaldiario)) %>% 
  ungroup() %>% group_by(fecha) %>% 
  mutate(total = sum(totaldiario), 
         proporcion = totaldiario/total*100) %>% ungroup() %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  mutate(tipovacuna_simple = factor(tipovacuna_simple, levels=c("Pfizer", "Cansino", "Moderna", "Sinopharm",
                                                                "AstraZeneca", 
                                                                "Sputnik"))) %>% 
  ggplot(aes(x=fecha, y =proporcion, fill=tipovacuna_simple))+
  geom_area(alpha=0.8) +
  scale_fill_manual(breaks = c("Sputnik", "AstraZeneca", 
                               "Sinopharm", "Moderna", "Cansino", "Pfizer"),
                    values=c("indianred3","steelblue3", "springgreen3", "yellow4", "palevioletred3", "darkorchid3"), 
                    labels=c("Sputnik V","AstraZeneca", "Sinopharm", "Moderna", "Cansino", "Pfizer"))+
  theme_light() + labs(x="", y="", title="", fill="Vacuna") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, suffix="%"))+
  theme(plot.title = element_blank(), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0)) + guides(fill = guide_legend(nrow = 1))

combinado_vacunas <- c/d

ggsave(combinado_vacunas, filename="combinado_vacunas.png", height=6, width=9.44)

primeradosis_ayer <- vacunas_diarias %>% filter(fecha == fecha_ayer) %>% 
  pull(primeradosis) %>% sum() 

primeradosis_ayer_print <- vacunas_diarias %>% filter(fecha == fecha_ayer) %>% 
  pull(primeradosis) %>% sum() %>% format(big.mark=".", decimal.mark=",")

segundadosis_ayer <- vacunas_diarias %>% filter(fecha == fecha_ayer) %>% 
  pull(segundadosis) %>% sum() 

segundadosis_ayer_print <- vacunas_diarias %>% filter(fecha == fecha_ayer) %>% 
  pull(segundadosis) %>% sum() %>% format(big.mark=".", decimal.mark=",")

primeradosis_total <- vacunas_diarias %>% 
  pull(primeradosis) %>% sum() 

primeradosis_total_print <- vacunas_diarias %>% 
  pull(primeradosis) %>% sum() %>% format(big.mark=".", decimal.mark=",")

segundadosis_total <- vacunas_diarias %>% 
  pull(segundadosis) %>% sum() 

segundadosis_total_print <- vacunas_diarias %>% 
  pull(segundadosis) %>% sum() %>% format(big.mark=".", decimal.mark=",")

poblacion_total <- vacunas_diarias %>% filter(fecha == fecha_ayer) %>%
  pull(poblacion) %>% sum() 

primeradosis_pob_print <- format(round(primeradosis_total / poblacion_total * 100, 2), decimal.mark = ",")

segundadosis_pob_print <- format(round(segundadosis_total / poblacion_total * 100, 2), decimal.mark = ",")

texto_tweet_1 <- paste0("Ayer, ", fecha_latina, ", se reportaron ",primeradosis_ayer_print, " primeras dosis y ", 
                        segundadosis_ayer_print, " segundas dosis de vacunas contra el coronavirus en Argentina. En total, se aplicaron ",
                        primeradosis_total_print, " primeras dosis y ", segundadosis_total_print,
                        " segundas dosis, ", primeradosis_pob_print,"% y ",
                        segundadosis_pob_print, "% de la población respectivamente.")


####################### SEGUNDO TUIT #################################

vacunados_edades <- jsonlite::fromJSON(url("https://covidstats.com.ar/ws/vacunadosedades")) %>% 
  rlist::list.rbind() %>% 
  t() %>% 
  as.data.frame() %>% 
  select(-idprovincia) 

summary_row <- vacunados_edades %>% 
  group_by(grupo) %>% 
  summarize(total_personas = sum(as.numeric(personas)), 
            total_dosis1 = sum(as.numeric(dosis1)), 
            proporcion_total_pais = round(total_dosis1/total_personas*100,1)) %>% 
  select(grupo, proporcion_total_pais) %>% pivot_wider(names_from = grupo, values_from = proporcion_total_pais) %>% 
  bind_cols(c("Total Pais")) %>% rename(provincia = `...11`) %>% mutate(orden = 1) %>% 
  relocate(provincia)

jsonlite::fromJSON(url("https://covidstats.com.ar/ws/vacunadosedades")) %>% 
  rlist::list.rbind() %>% 
  t() %>% 
  as.data.frame() %>% 
  select(-idprovincia) %>%
  mutate(proporcion = round(as.numeric(dosis1)/as.numeric(personas)*100, 1)) %>% 
  select(provincia, grupo, proporcion) %>% 
  pivot_wider(names_from = grupo, values_from=proporcion) %>% 
  relocate(">=100", .after = "90-99") %>% 
  ungroup() %>% 
  mutate(orden = row_number()+1) %>% 
  bind_rows(summary_row) %>% 
  arrange(orden) %>% 
  select(-orden) %>% 
  rename(" " = provincia) %>% 
  gt() %>%  data_color(
    columns = vars(`<18`,
                   `18-29`,
                   `30-39`,
                   `40-49`,
                   `50-59`,
                   `60-69`,
                   `70-79`,
                   `80-89`,
                   `90-99`,
                   `>=100`),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = c(0,100), na.color = "#1B5E20")) %>% 
  tab_header(title=paste0("Primeras dosis cada 100 habitantes, ", fecha_latina), subtitle = NULL) %>% 
  tab_style(
    style = list(
      cell_fill(color = "white"),
      cell_text(weight = "bold")),
    locations = cells_body(
      columns = vars(" "),
      rows = everything())) %>% 
  tab_options(column_labels.font.weight = 'bold',
              heading.title.font.size = 26,
              heading.title.font.weight = 'bold') %>% 
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(vars(`<18`,
                                         `18-29`,
                                         `30-39`,
                                         `40-49`,
                                         `50-59`,
                                         `60-69`,
                                         `70-79`,
                                         `80-89`,
                                         `90-99`,
                                         `>=100`))) %>% 
  tab_footnote(
    footnote = "Fuente: MSAL e INDEC. El margen de error de la proyección de población puede generar valores mayores a 100.",
    locations = cells_title()) %>% 
  gtsave("vacunados_provincias_edades.png")

summary_row <- vacunados_edades %>% 
  group_by(grupo) %>% 
  summarize(total_personas = sum(as.numeric(personas)), 
            total_dosis1 = sum(as.numeric(esquemacompleto)), 
            proporcion_total_pais = round(total_dosis1/total_personas*100,1)) %>% 
  select(grupo, proporcion_total_pais) %>% pivot_wider(names_from = grupo, values_from = proporcion_total_pais) %>% 
  bind_cols(c("Total Pais")) %>% rename(provincia = `...11`) %>% mutate(orden = 1) %>% 
  relocate(provincia)


jsonlite::fromJSON(url("https://covidstats.com.ar/ws/vacunadosedades")) %>% 
  rlist::list.rbind() %>% 
  t() %>% 
  as.data.frame() %>% 
  select(-idprovincia) %>%
  mutate(proporcion = round(as.numeric(esquemacompleto)/as.numeric(personas)*100, 1)) %>% 
  select(provincia, grupo, proporcion) %>% 
  pivot_wider(names_from = grupo, values_from=proporcion) %>% 
  relocate(">=100", .after = "90-99") %>% 
  ungroup() %>% 
  mutate(orden = row_number()+1) %>% 
  bind_rows(summary_row) %>% 
  arrange(orden) %>% 
  select(-orden) %>% 
  rename(" " = provincia) %>% 
  gt() %>%  data_color(
    columns = vars(`<18`,
                   `18-29`,
                   `30-39`,
                   `40-49`,
                   `50-59`,
                   `60-69`,
                   `70-79`,
                   `80-89`,
                   `90-99`,
                   `>=100`),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = c(0,100), na.color = "#1B5E20")) %>% 
  tab_header(title=paste0("Vacunaciones completas cada 100 habitantes, ", fecha_latina), subtitle = NULL) %>% 
  tab_style(
    style = list(
      cell_fill(color = "white"),
      cell_text(weight = "bold")),
    locations = cells_body(
      columns = vars(" "),
      rows = everything())) %>% 
  tab_options(column_labels.font.weight = 'bold',
              heading.title.font.size = 26,
              heading.title.font.weight = 'bold') %>% 
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(vars(`<18`,
                                         `18-29`,
                                         `30-39`,
                                         `40-49`,
                                         `50-59`,
                                         `60-69`,
                                         `70-79`,
                                         `80-89`,
                                         `90-99`,
                                         `>=100`))) %>% 
  tab_footnote(
    footnote = "Fuente: MSAL e INDEC. El margen de error de la proyección de población puede generar valores mayores a 100.",
    locations = cells_title()) %>% 
  gtsave("vacunados_provincias_edades_segundadosis.png")

mayores_menores_18 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor60 =  ifelse(grupo == "<18", 1, 0)) %>% 
  group_by(mayor60) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0))


mayores_18_vacunados <- mayores_menores_18 %>% filter(mayor60==0) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

menores_18_vacunados <- mayores_menores_18 %>% filter(mayor60==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")


mayores_menores_60 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor60 =  ifelse(grupo %in% c(">=100", "60-69", "70-79", "80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor60) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0))


mayores_60_vacunados <- mayores_menores_60 %>% filter(mayor60==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

menores_60_vacunados <- mayores_menores_60 %>% filter(mayor60==0) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

mayores_menores_70 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor70 =  ifelse(grupo %in% c(">=100", "70-79", "80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor70) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0))

mayores_70_vacunados <- mayores_menores_70 %>% filter(mayor70==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

mayores_menores_80 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor80 =  ifelse(grupo %in% c(">=100","80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor80) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0))

mayores_80_vacunados <- mayores_menores_80 %>% filter(mayor80==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

##

mayores_menores_18_dosis2 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor60 =  ifelse(grupo == "<18", 1, 0)) %>% 
  group_by(mayor60) %>% 
  summarize(dosis2 = sum(esquemacompleto), 
            personas = sum(personas), 
            proporcion = round(dosis2/personas*100, 0))

mayores_18_dosis2 <- mayores_menores_18_dosis2 %>% filter(mayor60==0) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

menores_18_dosis2 <- mayores_menores_18_dosis2 %>% filter(mayor60==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

mayores_menores_60_dosis2 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor60 =  ifelse(grupo %in% c(">=100", "60-69", "70-79", "80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor60) %>% 
  summarize(dosis2 = sum(esquemacompleto), 
            personas = sum(personas), 
            proporcion = round(dosis2/personas*100, 0))

mayores_60_dosis2 <- mayores_menores_60_dosis2 %>% filter(mayor60==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

menores_60_dosis2 <- mayores_menores_60_dosis2 %>% filter(mayor60==0) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

mayores_menores_70_dosis2 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor70 =  ifelse(grupo %in% c(">=100", "70-79", "80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor70) %>% 
  summarize(dosis2 = sum(esquemacompleto), 
            personas = sum(personas), 
            proporcion = round(dosis2/personas*100, 0))

mayores_70_dosis2 <- mayores_menores_70_dosis2 %>% filter(mayor70==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

mayores_menores_80_dosis2 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(mayor80 =  ifelse(grupo %in% c(">=100","80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor80) %>% 
  summarize(dosis2 = sum(esquemacompleto), 
            personas = sum(personas), 
            proporcion = round(dosis2/personas*100, 0))

mayores_80_dosis2 <- mayores_menores_80_dosis2 %>% filter(mayor80==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados6069 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="60-69") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados7079 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="70-79") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados8089 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="80-89") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados9099 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="90-99") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados100 <- vacunados_edades %>% 
  mutate(across(3:6, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo==">=100") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")


texto_tweet_2_bis <- paste0("Hasta ayer, ", 
                            fecha_latina, ": \n \n",
                            "Primera dosis: \n", 
                            "Menores de 18: ", menores_18_vacunados, "\n", 
                            "Mayores de 18: ", mayores_18_vacunados, "\n",
                            "Mayores de 60: ", mayores_60_vacunados, "\n",
                            "Mayores de 70: ", mayores_70_vacunados, "\n",
                            "Mayores de 80: ", mayores_80_vacunados, "\n \n",
                            "Vacunación completa: \n", 
                            "Menores de 18: ", menores_18_dosis2, "\n", 
                            "Mayores de 18: ", mayores_18_dosis2, "\n",
                            "Mayores de 60: ", mayores_60_dosis2, "\n",
                            "Mayores de 70: ", mayores_70_dosis2, "\n",
                            "Mayores de 80: ", mayores_80_dosis2)


###################################################### TERCER TUIT ########################################################


request <- '{"app":"dashboard",
    "requestId":"Q101",
    "timezone":"",
    "panelId":4,
    "dashboardId":3,
    "range":{"from":"2020-12-29T03:00:00.000Z","to":"2021-03-17T22:57:37.460Z",
      "raw":{"from":"2020-12-29T03:00:00.000Z","to":"now"}},
    "timeInfo":"",
    "interval":"3h",
    "intervalMs":10800000,
    "targets":[{"data":null,
      "target":"distribucion_aplicacion_utilidad_provincia_tabla",
      "refId":"A",
      "hide":false,
      "type":"table"}],
    "maxDataPoints":668,
    "scopedVars":{"_from":{"text":"1609210800000","value":"1609210800000"},"to":{"text":"1616021853504","value":"1616021853504"},
      "dashboard":{"value":{"name":"Seguimiento vacunación Covid","uid":"8wdHBOsMk"}},
      "org":{"value":{"name":"minsal","id":0}},"interval":{"text":"3h","value":"3h"},
      "_interval_ms":{"text":"10800000","value":10800000}},
    "startTime":1616021857461,
    "rangeRaw":{"from":"2020-12-29T03:00:00.000Z","to":"now"},
    "adhocFilters":[]}' %>% 
  POST("https://coronavirus.msal.gov.ar/vacunas/d/8wdHBOsMk/seguimiento-vacunacion-covid/api/datasources/proxy/1/query",
       body = ., encode = "raw", content_type("application/json"))


aplicadas_recibidas <- content(request,type="application/json") %>% pluck(1,"rows") %>% 
  rlist::list.rbind() %>% 
  as.data.frame() %>% 
  setNames(content(request, type="application/json") %>% pluck(1,"columns") %>% 
             rlist::list.rbind() %>% as.data.frame() %>% pull(text)) %>% clean_names() %>% 
  filter(provincia != "Totales") %>% mutate(across(contains("_"), as.numeric)) %>% 
  mutate(aplicadas_repartidas = aplicadas_total/distribuidas_total*100, provincia=unlist(provincia))

aplicadas_recibidas %>%
  ggplot(mapping = aes(x = reorder(provincia, aplicadas_repartidas), y=aplicadas_repartidas)) + 
  geom_bar(stat = "identity", color="steelblue3", fill="steelblue3", alpha=0.9) + 
  geom_text(aes(label = paste0(as.character(round(aplicadas_repartidas,1)), "%"), 
                y= aplicadas_repartidas),
            position = position_stack(vjust = .5), 
            size=3) +coord_flip() +
  theme_light()+
  labs(title=paste0("Porcentaje aplicado de las vacunas recibidas por provincia, ", fecha_latina), x="", y="") +
  scale_y_continuous(labels=label_number(accuracy = 1, suffix="%"), 
                     breaks = seq(0,100, by=10), limits=c(0,110)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold")) +
  ggsave(filename="aplicadas_recibidas.png", height = 6, width = 9.44)

provincias <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosmonitor")) %>% 
  pluck("provincias") %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(provincia = rowname) %>% 
  mutate(poblacion = as.numeric(poblacion))

evolucion <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosmonitor")) %>% 
  pluck("evolucion") %>% 
  modify_depth(1, pluck, "provincias") %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(timestamp = rowname) %>% 
  pivot_longer(cols=2:25, names_to = "provincia", values_to = "data") %>% 
  unnest_wider(col=data) %>% 
  left_join(provincias) %>% 
  mutate(timestamp = parse_datetime(timestamp), 
         time = as_datetime(timestamp)) %>% 
  group_by(provincia) %>% 
  mutate(denominacion = as.character(denominacion),
         dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         aplicadas_total = dosis1 + dosis2, 
         aplicadas_total_acum = cumsum(aplicadas_total),
         aplicadas_total_acum_poblacion = aplicadas_total_acum/poblacion*100,
         distribuidas_acum = as.numeric(cumsum(distribuidas)), 
         distribuidas_acum_poblacion = distribuidas_acum/poblacion*100,
         disponibles_acum = distribuidas_acum - aplicadas_total_acum, 
         disponibles_acum_poblacion = disponibles_acum / poblacion * 100)

evolucion %>% 
  mutate(disponibles_acum_miles = disponibles_acum/1000) %>% 
  ggplot(aes(x=time))+
  geom_line(aes(y=disponibles_acum_poblacion), color="steelblue3", size=0.8)+
  geom_ribbon(aes(ymin=0,ymax=disponibles_acum_poblacion), fill="steelblue3", alpha=0.2)+
  facet_wrap(~denominacion, ncol=6) +
  theme_light()+
  labs(x="", y="Dosis disponibles cada 100 habitantes",
       title= paste0("Stock de dosis sin aplicar por provincia, ", fecha_latina)) +
  scale_y_continuous(labels=label_number(accuracy = 1, big.mark = ".")) +
  scale_x_datetime(date_breaks = break_fechas_porpcia,date_labels = "%d/%m", expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        axis.title = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0),
        strip.text = element_text(face="bold", hjust = 0.5, color="black"), 
        strip.background = element_rect(fill="white", color="gray66")) +
  ggsave(filename="stock_disponible_provincias.png", height = 6, width = 9.44)

evolucion %>% 
  pivot_longer(cols= c("distribuidas_acum_poblacion", "aplicadas_total_acum_poblacion"), names_to = "curva", values_to="cantidad") %>% 
  ggplot(aes(x=time))+
  geom_line(aes(y=cantidad, color = curva), size=0.8)+
  geom_ribbon(data=evolucion, aes(ymin=aplicadas_total_acum_poblacion,ymax=distribuidas_acum_poblacion), fill="indianred3", alpha=0.2)+
  scale_color_manual(breaks = c("aplicadas_total_acum_poblacion", "distribuidas_acum_poblacion"), 
                     labels = c("Dosis aplicadas", "Dosis distribuidas"), 
                     values = c("#F8766D", "#00BFC4"))+
  facet_wrap(~denominacion, ncol=6, scales = "free_y") +
  theme_light()+
  labs(x="", y="Dosis cada 100 habitantes",
       title= paste0("Cantidad acumulada de dosis distribuidas y aplicadas por provincia, ", fecha_latina), 
       color="") +
  scale_y_continuous(labels=label_number(accuracy = 1, big.mark = ".")) +
  scale_x_datetime(date_breaks = break_fechas_porpcia,date_labels = "%d/%m", expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        axis.title = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0),
        strip.text = element_text(face="bold", hjust = 0.5, color="black"), 
        strip.background = element_rect(fill="white", color="gray66")) +
  ggsave(filename="aplicadas_distribuidas_provincias_acumuladas.png", height = 6, width = 9.44)


aplicadas_total  <- fromJSON("https://raw.githubusercontent.com/j-e-d/datosvacunas/main/vacunas.json") %>% 
  pull(rows) %>% as.data.frame() %>% 
  setNames(fromJSON("https://raw.githubusercontent.com/j-e-d/datosvacunas/main/vacunas.json") %>% 
             pull(columns) %>% as.data.frame() %>% pull(text)) %>% clean_names() %>% 
  mutate(across(contains("_"), as.numeric)) %>% 
  mutate(aplicadas_repartidas = aplicadas_total/distribuidas_total*100) %>% 
  filter(provincia=="Totales") %>% pull(aplicadas_total) %>% format(big.mark=".", decimal.mark=",")

distribuidas_total  <- fromJSON("https://raw.githubusercontent.com/j-e-d/datosvacunas/main/vacunas.json") %>% 
  pull(rows) %>% as.data.frame() %>% 
  setNames(fromJSON("https://raw.githubusercontent.com/j-e-d/datosvacunas/main/vacunas.json") %>% 
             pull(columns) %>% as.data.frame() %>% pull(text)) %>% clean_names() %>% 
  mutate(across(contains("_"), as.numeric)) %>% 
  mutate(aplicadas_repartidas = aplicadas_total/distribuidas_total*100) %>% 
  filter(provincia=="Totales") %>% pull(distribuidas_total) %>% format(big.mark=".", decimal.mark=",")

uso_stock <- fromJSON("https://raw.githubusercontent.com/j-e-d/datosvacunas/main/vacunas.json") %>% 
  pull(rows) %>% as.data.frame() %>% 
  setNames(fromJSON("https://raw.githubusercontent.com/j-e-d/datosvacunas/main/vacunas.json") %>% 
             pull(columns) %>% as.data.frame() %>% pull(text)) %>% clean_names() %>% 
  mutate(across(contains("_"), as.numeric)) %>% 
  mutate(aplicadas_repartidas = aplicadas_total/distribuidas_total*100) %>% 
  filter(provincia=="Totales") %>% pull(aplicadas_repartidas) %>% round(1) %>% as.character() %>% paste0(.,"%", sep="")

texto_tweet_3 <- paste0("Hasta ayer, ", 
                        fecha_latina, 
                        ", se aplicaron en total ",
                        aplicadas_total, 
                        " dosis, ", 
                        uso_stock, 
                        " de las ",
                        distribuidas_total,
                        " dosis distribuidas entre las provincias.")


###################################################### CUARTO TUIT ########################################################


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

vacunacion_deptos <- fromJSON(url("https://covidstats.com.ar/ws/mapa?porprovincia=false&pordepartamento=true&datosadicionales=true")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  separate(codigo, c("provincia", "departamento")) %>% 
  mutate( provincia = ifelse(nchar(provincia)==1, paste0("0", provincia, sep=""), provincia), 
          departamento = ifelse(nchar(departamento)==2, paste0("0", departamento, sep=""), 
                                ifelse(nchar(departamento)==1, paste0("00", departamento, sep=""), departamento)), 
          link = paste0(provincia, departamento, sep="")) %>% 
  group_by(link) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-03-01"), length.out = n(), by="days")) %>% ungroup()

deptos_hoy <- vacunacion_deptos %>% filter(fecha == max(fecha)) %>% 
  mutate(link2 = ifelse(provincia=="02", "99999", link)) %>% 
  group_by(link2) %>% 
  summarize(dosis1 = sum(dosis1), 
            dosis2 = sum(dosis2), 
            poblacion = sum(poblacion)) %>%  ungroup() %>% 
  mutate(pob_dosis_1 = dosis1/poblacion*100, 
         pob_dosis_2 = dosis2/poblacion*100, 
         pob_soloprimera = pob_dosis_1 - pob_dosis_2)

shape <- read_sf("Codgeo_Pais_x_dpto_con_datos (1)") %>% 
  mutate(link2 = ifelse(substr(link, start=1, stop=2)=="02", "99999", link)) %>% 
  left_join(deptos_hoy, by="link2")

sf_use_s2(FALSE)

amba_dosis1 <- shape %>% st_simplify(dTolerance = 0.001) %>% 
  filter( link2 %in% c("06035", "06805", "06515", "06749", "06756", 
                       "06861", "06371", "06840", "06760", "06412", 
                       "06560", "06408", "06410", "06568", "06539", 
                       "06427", "06270", "06260", "06434", "06490", 
                       "06028", "06658", "06091", "06274", "99999")) %>% 
  ggplot() + geom_sf(aes(fill=pob_dosis_1), size=0.04, show.legend = FALSE) +
  colorspace::scale_fill_continuous_sequential(palette="Greens", limits=c(0,106)) +
  coord_sf(datum=NA) + theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


pais_dosis1 <- shape %>% st_simplify(dTolerance = 0.005) %>% 
  filter(! departamen %in% c("Antártida Argentina", "Islas del Atlántico Sur")) %>% 
  ggplot() + geom_sf(aes(fill=pob_dosis_1), size=0.05) +
  colorspace::scale_fill_continuous_sequential(palette="Greens", limits=c(0,106)) +
  coord_sf(datum=NA) + theme_minimal() +
  labs(title=paste0("Porcentaje de la población \n con al menos una dosis, ", fecha_latina), 
       fill="%")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom", 
        legend.key.size = unit(0.6, 'cm'), #change legend key size
        legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.6, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8),
        legend.margin=margin(-15), 
        plot.title=element_text(hjust=0.5, vjust=-3, face="bold", size=16)) +
  inset_element(amba_dosis1,  left =  0.5, bottom =  0.05, right = 0.8, top = 0.4)

pais_dosis1$patches$layout$widths  <- 1
pais_dosis1$patches$layout$heights <- 1

amba_dosis2 <- shape %>% st_simplify(dTolerance = 0.001) %>% 
  filter( link2 %in% c("06035", "06805", "06515", "06749", "06756", 
                       "06861", "06371", "06840", "06760", "06412", 
                       "06560", "06408", "06410", "06568", "06539", 
                       "06427", "06270", "06260", "06434", "06490", 
                       "06028", "06658", "06091", "06274", "99999")) %>% 
  ggplot() + geom_sf(aes(fill=pob_dosis_2), size=0.04, show.legend = FALSE) +
  colorspace::scale_fill_continuous_sequential(palette="Greens", limits=c(0,106)) +
  coord_sf(datum=NA) + theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


pais_dosis2 <- shape %>% st_simplify(dTolerance = 0.005) %>% 
  filter(! departamen %in% c("Antártida Argentina", "Islas del Atlántico Sur")) %>% 
  ggplot() + geom_sf(aes(fill=pob_dosis_2), size=0.05) +
  colorspace::scale_fill_continuous_sequential(palette="Greens", limits=c(0,106)) +
  coord_sf(datum=NA) + theme_minimal() +
  labs(title=paste0("Porcentaje de la población \n con dos dosis, ", fecha_latina), 
       fill="%")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom", 
        legend.key.size = unit(0.6, 'cm'), #change legend key size
        legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.6, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8),
        legend.margin=margin(-15), 
        plot.title=element_text(hjust=0.5, vjust=-3, face="bold", size=16)) +
  inset_element(amba_dosis2,  left =  0.5, bottom =  0.05, right = 0.8, top = 0.4)

pais_dosis2$patches$layout$widths  <- 1
pais_dosis2$patches$layout$heights <- 1

ggsave(pais_dosis1, height=12, width=6, filename="mapa_dosis1.png")
ggsave(pais_dosis2, height=12, width=6, filename="mapa_dosis2.png")

maximo_vacunacion <- shape %>% filter(pob_dosis_1 == max(pob_dosis_1, na.rm=TRUE))

minimo_vacunacion <- shape %>% filter(pob_dosis_1 == min(pob_dosis_1, na.rm=TRUE))

texto_tweet_4 <- paste0("El departamento con mayor avance en la vacunación es ", 
                        maximo_vacunacion %>% pull(departamen), 
                        " (", 
                        maximo_vacunacion %>% pull(provincia), 
                        "), con ", 
                        maximo_vacunacion %>% pull(pob_dosis_1) %>% round(1), 
                        "% de la población con al menos una dosis. El de menor avance es ", 
                        minimo_vacunacion %>% pull(departamen), 
                        " (", 
                        minimo_vacunacion %>% pull(provincia), 
                        "), con ", 
                        minimo_vacunacion %>% pull(pob_dosis_1) %>% round(1),
                        "%.")

###################################################### QUINTO TUIT ########################################################


astrazeneca <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosargentina?comprimido=1&tiposvacuna%5B%5D=7&tiposvacuna%5B%5D=2")) %>% 
  list.rbind() %>% t() %>% 
  as.data.frame() %>% 
  select(c(dosis1, dosis2)) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-01-01"), length.out = nrow(.), by="days"))

sinopharm <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosargentina?comprimido=1&tiposvacuna%5B%5D=3")) %>% 
  list.rbind() %>% t() %>% 
  as.data.frame() %>% 
  select(c(dosis1, dosis2)) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-01-01"), length.out = nrow(.), by="days"))

sputnik <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosargentina?comprimido=1&tiposvacuna%5B%5D=1")) %>% 
  list.rbind() %>% t() %>% 
  as.data.frame() %>% 
  select(c(dosis1, dosis2)) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-01-01"), length.out = nrow(.), by="days"))

moderna <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosargentina?comprimido=1&tiposvacuna%5B%5D=8")) %>% 
  list.rbind() %>% t() %>% 
  as.data.frame() %>% 
  select(c(dosis1, dosis2)) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-01-01"), length.out = nrow(.), by="days"))

cansino <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosargentina?comprimido=1&tiposvacuna%5B%5D=9")) %>% 
  list.rbind() %>% t() %>% 
  as.data.frame() %>% 
  select(c(dosis1, dosis2)) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-01-01"), length.out = nrow(.), by="days"))

pfizer <- fromJSON(url("https://covidstats.com.ar/ws/vacunadosargentina?comprimido=1&tiposvacuna%5B%5D=6")) %>% 
  list.rbind() %>% t() %>% 
  as.data.frame() %>% 
  select(c(dosis1, dosis2)) %>% 
  mutate(fecha = seq.Date(from = ymd("2020-01-01"), length.out = nrow(.), by="days"))

sputnik.plot <- sputnik %>% mutate(dosis1_acum = cumsum(dosis1), 
                                   dosis2_acum = cumsum(dosis2)) %>% 
  pivot_longer(cols=c(dosis1_acum, dosis2_acum), 
               names_to = "dosis", 
               values_to = "cantidad") %>% 
  filter(fecha>ymd("2020-12-24")) %>% 
  ggplot(aes(x=fecha, y=cantidad, color=dosis)) +
  geom_line(size=0.8, show.legend=FALSE)+
  scale_color_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                     values= c("steelblue3", "springgreen3")) +
  theme_light() + labs(x="", y="", title=paste0("Dosis de Sputnik totales, ",fecha_latina), fill="Vacuna") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0))


az.plot <- astrazeneca %>% mutate(dosis1_acum = cumsum(dosis1), 
                                  dosis2_acum = cumsum(dosis2)) %>% 
  pivot_longer(cols=c(dosis1_acum, dosis2_acum), 
               names_to = "dosis", 
               values_to = "cantidad") %>% 
  filter(fecha>ymd("2020-12-24")) %>% 
  ggplot(aes(x=fecha, y=cantidad, color=dosis)) +
  geom_line(size=0.8, show.legend=FALSE)+
  scale_color_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                     values= c("steelblue3", "springgreen3")) +
  theme_light() + labs(x="", y="", title=paste0("Dosis de AstraZeneca totales, ",fecha_latina), fill="Vacuna") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0))

sinopharm.plot <- sinopharm %>% mutate(dosis1_acum = cumsum(dosis1), 
                                       dosis2_acum = cumsum(dosis2)) %>% 
  pivot_longer(cols=c(dosis1_acum, dosis2_acum), 
               names_to = "dosis", 
               values_to = "cantidad") %>% 
  filter(fecha>ymd("2020-12-24")) %>% 
  ggplot(aes(x=fecha, y=cantidad, color=dosis)) +
  geom_line(size=0.8, show.legend=FALSE)+
  scale_color_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                     values= c("steelblue3", "springgreen3"), 
                     labels = c("Primera dosis", "Segunda dosis")) +
  theme_light() + labs(x="", y="", title=paste0("Dosis de Sinopharm totales, ",fecha_latina), color="") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=11),
        legend.margin=margin(-15, 0, 0, 0))

moderna.plot <- moderna %>% mutate(dosis1_acum = cumsum(dosis1), 
                                   dosis2_acum = cumsum(dosis2)) %>% 
  pivot_longer(cols=c(dosis1_acum, dosis2_acum), 
               names_to = "dosis", 
               values_to = "cantidad") %>% 
  filter(fecha>ymd("2020-12-24")) %>% 
  ggplot(aes(x=fecha, y=cantidad, color=dosis)) +
  geom_line(size=0.8, show.legend=FALSE)+
  scale_color_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                     values= c("steelblue3", "springgreen3"), 
                     labels = c("Primera dosis", "Segunda dosis")) +
  theme_light() + labs(x="", y="", title=paste0("Dosis de Moderna totales, ",fecha_latina), color="") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=11),
        legend.margin=margin(-15, 0, 0, 0))

cansino.plot <- cansino %>% mutate(dosis1_acum = cumsum(dosis1), 
                                   dosis2_acum = cumsum(dosis2)) %>% 
  pivot_longer(cols=c(dosis1_acum, dosis2_acum), 
               names_to = "dosis", 
               values_to = "cantidad") %>% 
  filter(fecha>ymd("2020-12-24")) %>% 
  ggplot(aes(x=fecha, y=cantidad, color=dosis)) +
  geom_line(size=0.8, show.legend=FALSE)+
  scale_color_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                     values= c("steelblue3", "springgreen3"), 
                     labels = c("Primera dosis", "Segunda dosis")) +
  theme_light() + labs(x="", y="", title=paste0("Dosis de Cansino totales, ",fecha_latina), color="") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=11),
        legend.margin=margin(-15, 0, 0, 0))

pfizer.plot <- pfizer %>% mutate(dosis1_acum = cumsum(dosis1), 
                                   dosis2_acum = cumsum(dosis2)) %>% 
  pivot_longer(cols=c(dosis1_acum, dosis2_acum), 
               names_to = "dosis", 
               values_to = "cantidad") %>% 
  filter(fecha>ymd("2020-12-24")) %>% 
  ggplot(aes(x=fecha, y=cantidad, color=dosis)) +
  geom_line(size=0.8, show.legend=TRUE)+
  scale_color_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                     values= c("steelblue3", "springgreen3"), 
                     labels = c("Primera dosis", "Segunda dosis")) +
  theme_light() + labs(x="", y="", title=paste0("Dosis de Pfizer totales, ",fecha_latina), color="") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=11),
        legend.margin=margin(-15, 0, 0, 0))

combinado_marcas <- sputnik.plot/az.plot/sinopharm.plot/moderna.plot/cansino.plot/pfizer.plot

ggsave(combinado_marcas, filename="combinado_marcas.png", height=10, width=10)


az_total <- astrazeneca %>%
  mutate(dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         vacuna = "AstraZeneca") %>% 
  filter(fecha == max(fecha))

sputnik_total <- sputnik %>%
  mutate(dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         vacuna = "Sputnik V") %>% 
  filter(fecha == max(fecha))

sinopharm_total <- sinopharm %>%
  mutate(dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         vacuna = "Sinopharm") %>% 
  filter(fecha == max(fecha))

moderna_total <- moderna %>%
  mutate(dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         vacuna = "Moderna") %>% 
  filter(fecha == max(fecha))

cansino_total <- cansino %>%
  mutate(dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         vacuna = "Cansino") %>% 
  filter(fecha == max(fecha))

pfizer_total <- pfizer %>%
  mutate(dosis1_acum = cumsum(dosis1), 
         dosis2_acum = cumsum(dosis2), 
         vacuna = "Pfizer") %>% 
  filter(fecha == max(fecha))

az_total %>% 
  bind_rows(sputnik_total) %>% 
  bind_rows(sinopharm_total) %>% 
  bind_rows(moderna_total) %>% 
  bind_rows(cansino_total) %>% 
  bind_rows(pfizer_total) %>%
  pivot_longer(cols = c(dosis1_acum, dosis2_acum)) %>% 
  mutate(value_format = format(value, big.mark=".", decimal.mark=",")) %>% 
  ggplot(aes(x=vacuna, y=value, fill=name)) + geom_col(alpha=0.9) +
  geom_text(aes(label =value_format, 
                y= value),
            position = position_stack(vjust = .5), 
            size=3.5) +
  scale_fill_manual(breaks=c("dosis1_acum", "dosis2_acum"), 
                    values= c("steelblue3", "springgreen3"), 
                    labels = c("Primera dosis", "Segunda dosis")) +
  scale_y_continuous(labels=label_number(accuracy = 1, scale = 1, 
                                         big.mark = ".", decimal.mark = ",")) +
  labs(title=paste0("Dosis aplicadas totales por tipo de vacuna, ", fecha_latina), x="", y="", fill="")+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text.y = element_text(size=9,face="bold"), 
        axis.text.x = element_text(size=12,face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=10),
        legend.margin=margin(-15, 0, 0, 0)) +
  ggsave(filename="vacunas_portipo.png", height = 9, width = 9.44)

az_total_agregado <- (az_total$dosis1_acum + az_total$dosis2_acum) %>% format(big.mark=".", decimal.mark=",")

sputnik_total_agregado <- (sputnik_total$dosis1_acum + sputnik_total$dosis2_acum) %>% format(big.mark=".", decimal.mark=",")

sinopharm_total_agregado <-  (sinopharm_total$dosis1_acum + sinopharm_total$dosis2_acum) %>% format(big.mark=".", decimal.mark=",") 

moderna_total_agregado <-  (moderna_total$dosis1_acum + moderna_total$dosis2_acum) %>% format(big.mark=".", decimal.mark=",") 

cansino_total_agregado <-  (cansino_total$dosis1_acum + cansino_total$dosis2_acum) %>% format(big.mark=".", decimal.mark=",")

pfizer_total_agrupado <-  (pfizer_total$dosis1_acum + pfizer_total$dosis2_acum) %>% format(big.mark=".", decimal.mark=",") 


texto_tweet_5_bis <- paste0("Dosis aplicadas hasta ayer, ", fecha_latina, ": \n \n", 
                            "AstraZeneca: ", az_total_agregado, "\n",
                            "Cansino: ", cansino_total_agregado, "\n",
                            "Moderna: ", moderna_total_agregado, "\n",
                            "Pfizer: ", pfizer_total_agrupado, "\n",
                            "Sinopharm: ", sinopharm_total_agregado, "\n",
                            "Sputnik V: ", sputnik_total_agregado) %>% 

# Postear los tuits

get_token()

post_tweet(status = texto_tweet_1, media=c("vacunados_provincias.png",
                                         "vacunados_diarios_provincias.png",
                                         "combinado_dosis.png",
                                         "combinado_vacunas.png"))


my_timeline <- get_timeline(home_user())
reply_id <- my_timeline$status_id[1]


post_tweet(status = texto_tweet_2,
           media = c("vacunados_provincias_edades.png", 
                     "vacunados_provincias_edades_segundadosis.png"),
           in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(status = texto_tweet_3,
           media = c("aplicadas_recibidas.png",
                     "stock_disponible_provincias.png",
                     "aplicadas_distribuidas_provincias_acumuladas.png"),
                      in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(status = texto_tweet_4,
           media = c("mapa_dosis1.png",
                     "mapa_dosis2.png"),
           in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(status = texto_tweet_5,
           media = c("combinado_marcas.png",
                     "vacunas_portipo.png"),
           in_reply_to_status_id = reply_id)


