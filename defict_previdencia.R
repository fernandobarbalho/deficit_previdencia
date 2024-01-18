devtools::install_github("tchiluanda/rsiconfi")

library(rsiconfi)
library(jsonlite)

rsiconfi::df_reports


get_rreo_local<- function(year, period, report_tp, annex, entity, In_RGPS=FALSE, In_RPPS= FALSE ){
  
  
  library(dplyr)
  library(purrr)
  library(stringr)
  library(jsonlite)
  
  
  #test if some variables have just one element
  
  print(length(report_tp))
  if (length(report_tp)>1){
    stop("Must inform just one report type")
  }
  
  if (length(annex)>1){
    stop("Must inform just one annex")
  }
  
  
  #Test business rules
  if(In_RGPS && In_RPPS){
    stop("Must choose between RGPS and RPPS")
  }
  
  if(report_tp>2 ){
    stop("Wrong report type")
  }
  
  df_esf_entidade = tibble(entidade = entity )
  
  df_esf_entidade<-df_esf_entidade%>%
    mutate(esfera= case_when(
      str_length(entity)== 1 ~"U",
      str_length(entity)== 2 ~"E",
      str_length(entity)== 7 ~"M"
    ) )
  
  
  annex_txt<-paste0("RREO-Anexo ",annex, ifelse(In_RGPS," - RGPS", ifelse(In_RPPS," - RPPS","")))
  
  # test<- df_esf_entidade %>%
  #   anti_join(df_reports%>%
  #               filter(anexo==annex_txt))
  # 
  # if (NROW(test)>0){
  #   stop("One or more entities not suitable for the annex and/or RGPS/RPPS flag informed")
  # }
  
  
  map_df(year, function(ref_year){
    
    map_df(period, function(ref_per){
      
      map_df(entity, function(ref_entity){
        
        
        base_address<- "http://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo"
        annex_conv<-paste0("RREO-Anexo%20",annex)
        if (In_RGPS ){
          annex_conv<-paste0(annex_conv,"%20-%20RGPS")
        }
        
        if (In_RPPS ){
          annex_conv<-paste0(annex_conv,"%20-%20RPPS")
        }
        
        exp<- paste0(base_address,
                     "?an_exercicio=", ref_year,
                     "&nr_periodo=", ref_per,
                     "&co_tipo_demonstrativo=", c("RREO", "RREO+Simplificado")[report_tp],
                     "&no_anexo=", annex_conv,
                     "&id_ente=",ref_entity)
        
        print(exp)
        
        ls_siconfi<-jsonlite::fromJSON(exp)
        
        
        print(ls_siconfi$count)
        if (ls_siconfi$count==0){
          return (tibble())
        }
        df_siconfi<- ls_siconfi[["items"]]
        
        df_siconfi$valor <- as.numeric(df_siconfi$valor)
        
        df_siconfi
        
        
      })
      
    })
  })
  
}

teste_rreo<-
get_rreo_local(2017,1,1,"0","1")

teste_rreo<-
get_rreo_local(2017,1,1,"01","1")

teste_rreo<-
get_rreo(2017,1,1,"04","1")

previdencia_civil_fcdf_2023<-
  get_rreo(year= 2023,
           period = 5,
           report_tp = 1,
           annex = "04.2",
           entity = 1)

previdencia_civil_fcdf_2023<-
previdencia_civil_fcdf_2023 %>%
  filter(coluna == "DESPESAS LIQUIDADAS ATÉ O BIMESTRE / 2023",
         str_detect(conta, "RESULTADO"))


previdencia_militares_2023<-
  get_rreo_local(year= 2023,
           period = 5,
           report_tp = 1,
           annex = "04.3",
           entity = 1)

previdencia_militares_2023<-
previdencia_militares_2023%>%
  filter(coluna == "DESPESAS LIQUIDADAS ATÉ O BIMESTRE / 2023",
         str_detect(conta, "RESULTADO"))


previdencia_rgps_2023<-
  get_rreo_local(year= 2023,
           period = 5,
           report_tp = 1,
           annex = "04.4",
           In_RGPS = TRUE,
           entity = 1)

previdencia_rgps_2023<-
previdencia_rgps_2023%>%
  filter(coluna == "DESPESAS LIQUIDADAS ATÉ O BIMESTRE / 2023",
         str_detect(conta, "Resultado"))

previdencia_civil_fcdf_2019_2022<-
  
purrr::map_dfr(2019:2022, function(ano_siconfi){
    get_rreo(year= ano_siconfi,
             period = 6,
             report_tp = 1,
             annex = "04.1",
             entity = 1)%>%
    filter( coluna == paste0("DESPESAS LIQUIDADAS ATÉ O BIMESTRE",ifelse(ano_siconfi %in% c(2021,2022),"" ," / "),ifelse(ano_siconfi%in% c(2021,2022),"",ano_siconfi)),
            str_detect(conta, "RESULTADO"))
  
})



previdencia_militar_2019_2022<-
  purrr::map_dfr(2019:2022, function(ano_siconfi){
    get_rreo(year= ano_siconfi,
             period = 6,
             report_tp = 1,
             annex = "04.2",
             entity = 1)%>%
      filter( coluna == paste0("DESPESAS LIQUIDADAS ATÉ O BIMESTRE",ifelse(ano_siconfi %in% c(2021,2022),"" ," / "),ifelse(ano_siconfi%in% c(2021,2022),"",ano_siconfi)),
             str_detect(conta, "RESULTADO"))
    
  })


previdencia_rgps_2019__2021_2022<-
  purrr::map_dfr(c(2019,2021,2022), function(ano_siconfi){
    get_rreo(year= ano_siconfi,
             period = 6,
             report_tp = 1,
             annex = "04.3",
             In_RGPS = TRUE,
             entity = 1)%>%
      filter( coluna == paste0("DESPESAS LIQUIDADAS ATÉ O BIMESTRE",ifelse(ano_siconfi %in% c(2021,2022),"" ,"/ "),ifelse(ano_siconfi%in% c(2021,2022),"",ano_siconfi)),
              str_detect(conta, "Resultado"))
    
    
  })


previdencia_rgps_2020<-
    get_rreo(year= 2020,
             period = 6,
             report_tp = 1,
             annex = "04.3",
             In_RGPS = TRUE,
             entity = 1)%>%
      filter( coluna =="DESPESAS LIQUIDADAS ATÉ O BIMESTRE / 2020",# coluna == paste0("DESPESAS LIQUIDADAS ATÉ O BIMESTRE",ifelse(ano_siconfi %in% c(2021,2022),"" ," / "),ifelse(ano_siconfi%in% c(2021,2022),"",ano_siconfi)),
              str_detect(conta, "Resultado"))
    
    



todas_previdencias<-
previdencia_civil_fcdf_2023 %>%
  bind_rows(previdencia_civil_fcdf_2019_2022,
            previdencia_militares_2023,
            previdencia_militar_2019_2022,
            previdencia_rgps_2023,
            previdencia_rgps_2019__2021_2022,
            previdencia_rgps_2020)

todas_previdencias<-
todas_previdencias %>%
  arrange(exercicio)


tipos<- rep(c("RPPS-civil","FCDF","Pensões militares","Inativos militares","RGPS"),5) 

todas_previdencias$tipo<- tipos

hjust =  c(rep(-0.1,29),1)

todas_previdencias %>%
  summarise (.by = exercicio,
      total = sum(valor)*-1) %>%
  ggplot(aes(x=exercicio, y= total/10^9))+
  geom_col()+
  geom_text(aes(label= round(total/10^9 )),vjust=-0.1)+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(
    title = "Evolução déficit previdência",
    subtitle = "Valores em RS bi (2023) até outubro",
    x = "",
    y = "",
    caption = "Fonte: siconfi"
  )


valor_total_ano<-
  todas_previdencias %>%
  summarise(.by = exercicio,
            total = sum(valor)*-1)
  

todas_previdencias %>%
  summarise (.by = c(exercicio, tipo),
             total = sum(valor)*-1) %>%
  ggplot(aes(x=exercicio, y= total/10^9))+
  geom_col( aes(fill= tipo))+
  geom_text(data= valor_total_ano, aes(label= round(total/10^9 )),vjust=-0.1)+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(
    title = "Evolução défict previdência",
    subtitle = "Valores em RS bi (2023) até outubro",
    x = "",
    y = ""
  )

