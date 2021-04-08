lapply(c("janitor", "lubridate","rtweet", "glue", "httr",
         "scales","tidyverse", "rtweet", "zoo", "reticulate", "paletteer",
         "jsonlite","gt", "rlist"), require, character.only=T)

# Primer tuit

vacunados <- fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  group_by(denominacion) %>% 
  summarize(primera_dosis_cantidad = sum(primeradosis), 
            segunda_dosis_cantidad=sum(segundadosis), 
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

breaks_vacunados <- case_when(max(vacunados$pob_dosis_1)<10 ~0.5,
                              max(vacunados$pob_dosis_1) > 10 & max(vacunados$pob_dosis_1) < 20 ~ 2,
                              max(vacunados$pob_dosis_1) > 20 & max(vacunados$pob_dosis_1) < 50 ~ 5, 
                              TRUE ~ 10)

vacunados %>% arrange(pob_dosis_1) %>% mutate(orden=row_number()) %>% 
  pivot_longer(cols=c("pob_soloprimera", "pob_dosis_2"), names_to = "dosis", 
               values_to = "cantidad") %>% 
  # mutate(dosis=factor(dosis, levels=c("pob_soloprimera", "pob_dosis_2"))) %>% 
  ggplot(mapping=aes(x=reorder(denominacion, orden))) +
  geom_col(aes(y=cantidad, fill=dosis), alpha = 0.9) + 
  geom_text(aes(label = paste0(as.character(round(cantidad,2)), "%"), 
                y= cantidad),
            position = position_stack(vjust = .5), 
            size=3) + coord_flip() +  theme_light()+
  labs(title=paste0("Porcentaje de la poblacion vacunado por provincia, ",fecha_latina), x="", y="", 
       fill="") +
  scale_y_continuous(labels=label_number(accuracy = 0.1, suffix="%"), breaks=seq(0,ymax,by=breaks_vacunados)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0)) +
  scale_fill_manual(breaks = c("pob_soloprimera", "pob_dosis_2"),
                    values=c("steelblue3","palegreen3"), 
                    labels=c("Solo primera dosis","Vacunacion completa")) +
  ggsave(filename="vacunados_provincias.png", height = 6, width = 9.44)

break_fechas_porpcia <- case_when(fecha_ayer - ymd("2021-01-19")< days(90) ~ "2 week",
                                  fecha_ayer - ymd("2021-01-19")> days(90) & fecha_ayer - ymd("2021-01-19")< days(120) ~ "1 month",
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
  scale_y_continuous(labels=label_number(suffix="%", accuracy=0.01)) +
  labs(x="", y="Dosis diarias totales, % de la poblacion, media movil 7 dias", 
       title= paste0("Dosis diarias reportadas por provincia, ",fecha_latina)) +
  theme(strip.text = element_text(face="bold", hjust = 0.5, color="black"), 
        strip.background = element_rect(fill="white", color="gray66"),
        plot.title =   element_text(face="bold", hjust = 0.5)) +
  ggsave(filename="vacunados_diarios_provincias.png", height = 6, width = 9.44)

break_fechas_totales <- case_when(fecha_ayer - ymd("2021-01-19")< days(90) ~ "1 week",
               fecha_ayer - ymd("2021-01-19")> days(90) & fecha_ayer - ymd("2021-01-19")< days(180) ~ "2 week",
               TRUE ~ "1 month")


fromJSON(url("https://covidstats.com.ar/ws/vacunados")) %>% 
  list.rbind() %>% 
  as.data.frame() %>% 
  unnest() %>% 
  mutate(fecha = ymd(substr(fecha, start=1, stop=10)), 
         totaldiario=primeradosis+segundadosis, 
         tipovacuna = ifelse(tipovacuna == "AstraZeneca ChAdOx1 S recombinante", "COVISHIELD ChAdOx1nCoV COVID 19", tipovacuna)) %>% 
  group_by(fecha, tipovacuna) %>% 
  summarize(totaldiario = sum(totaldiario)) %>% 
  filter(fecha>ymd("2021-01-19")) %>% 
  mutate(tipovacuna = factor(tipovacuna, levels=c("Sinopharm Vacuna SARSCOV 2 inactivada",
                                                  "COVISHIELD ChAdOx1nCoV COVID 19", 
                                                  "Sputnik V COVID19 Instituto Gamaleya"))) %>% 
  ggplot(aes(x=fecha, y =totaldiario, fill=tipovacuna))+
  geom_col(alpha=0.8) +
  scale_fill_manual(breaks = c("Sputnik V COVID19 Instituto Gamaleya", "COVISHIELD ChAdOx1nCoV COVID 19", 
                               "Sinopharm Vacuna SARSCOV 2 inactivada"),
                    values=c("indianred3","steelblue3", "springgreen3"), 
                    labels=c("Sputnik V","AstraZeneca", "Sinopharm"))+
  theme_light() + labs(x="", y="", title=paste0("Total de vacunas reportadas por dia, ",fecha_latina), fill="Vacuna") +
  scale_x_date( date_breaks = break_fechas_totales,date_labels = "%d/%m", expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold"), 
        legend.position="bottom", 
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        legend.margin=margin(-15, 0, 0, 0)) +
  ggsave(filename="vacunados_totales_diarios.png", height = 6, width = 9.44)


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
                     breaks = seq(0,100, by=10), limits=c(0,100)) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        axis.text = element_text(face="bold")) +
  ggsave(filename="aplicadas_recibidas.png", height = 6, width = 9.44)

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

uso_stock <- aplicadas_recibidas %>% 
  summarize(aplicadas_total = sum(aplicadas_total),
            distribuidas_total = sum(distribuidas_total), 
            aplicadas_repartidas = aplicadas_total/distribuidas_total*100) %>%
 pull(aplicadas_repartidas) %>% 
  round(0) %>% as.character() %>% paste0(.,"%", sep="")

primeradosis_pob_print <- format(round(primeradosis_total / poblacion_total * 100, 2), decimal.mark = ",")

texto_tweet_1 <- paste0("Ayer, ", fecha_latina, ", se reportaron ",primeradosis_ayer_print, " primeras dosis y ", 
                      segundadosis_ayer_print, " segundas dosis de vacunas contra el coronavirus en Argentina. En total, se aplicaron ",
                      primeradosis_total_print, " primeras dosis y ", segundadosis_total_print,
                      " segundas dosis (", uso_stock," de las distribuidas). ",
                      primeradosis_pob_print, "% de la poblacion recibio al menos una dosis.")


# Segundo tuit

vacunados_edades <- jsonlite::fromJSON(url("https://covidstats.com.ar/ws/vacunadosedades")) %>% 
  rlist::list.rbind() %>% 
  t() %>% 
  as.data.frame() %>% 
  select(-idprovincia)

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
  rename(" " = provincia) %>% 
  gt() %>%  data_color(
    columns = vars(`15-29`,
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
      domain = c(0,100))) %>% 
  tab_header(title=paste0("Personas vacunadas cada 100 habitantes, ", fecha_latina), subtitle = NULL) %>% 
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
    locations = cells_column_labels(vars(`15-29`,
                                         `30-39`,
                                         `40-49`,
                                         `50-59`,
                                         `60-69`,
                                         `70-79`,
                                         `80-89`,
                                         `90-99`,
                                         `>=100`))) %>% 
  gtsave("vacunados_provincias_edades.png")

mayores_menores_60 <- vacunados_edades %>% 
  mutate(across(3:5, as.numeric)) %>% 
  mutate(mayor60 =  ifelse(grupo %in% c(">=100", "60-69", "70-79", "80-89", "90-99"), 1, 0)) %>% 
  group_by(mayor60) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0))


mayores_60_vacunados <- mayores_menores_60 %>% filter(mayor60==1) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

menores_60_vacunados <- mayores_menores_60 %>% filter(mayor60==0) %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")


vacunados6069 <- vacunados_edades %>% 
  mutate(across(3:5, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="60-69") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados7079 <- vacunados_edades %>% 
  mutate(across(3:5, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="70-79") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")


vacunados8089 <- vacunados_edades %>% 
  mutate(across(3:5, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="80-89") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados9099 <- vacunados_edades %>% 
  mutate(across(3:5, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo=="90-99") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

vacunados100 <- vacunados_edades %>% 
  mutate(across(3:5, as.numeric)) %>% 
  group_by(grupo) %>% 
  summarize(dosis1 = sum(dosis1), 
            personas = sum(personas), 
            proporcion = round(dosis1/personas*100, 0)) %>% 
  filter(grupo==">=100") %>% 
  pull(proporcion) %>% as.character() %>% paste0("%")

texto_tweet_2 <- paste0("Hasta ayer, ", 
                        fecha_latina, 
                        ", recibió al menos una dosis el ",
                        menores_60_vacunados, 
                        " de los menores de 60 años y el ", 
                        mayores_60_vacunados, 
                        " de los mayores de 60 (",
                        vacunados6069,
                        " de la franja 60-69, ",
                        vacunados7079,
                        " de la franja 70-79, ",
                        vacunados8089,
                        " de la franja 80-89, ",
                        vacunados9099,
                        " de la franja 90-99 y ",
                        vacunados100,
                        " de los mayores de 100 años).")

# Postear los tuits

get_token()

post_tweet(status = texto_tweet_1, media=c("vacunados_provincias.png",
                                         "vacunados_diarios_provincias.png",
                                         "vacunados_totales_diarios.png",
                                         "aplicadas_recibidas.png"))


my_timeline <- get_timeline(home_user())
reply_id <- my_timeline$status_id[1]


post_tweet(status = texto_tweet_2,
           media = "vacunados_provincias_edades.png",
           in_reply_to_status_id = reply_id)



