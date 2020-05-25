ComposicaoIDM <- function(Eixo){
  
  load(file = "Dados/POCV.RData")
  
  Municipio <- IDM %>% 
    filter(!is.na(Valor) & Localidade == LocRef$Localidade & IDM == Eixo)
  
  #Número de eixos de avaliação em cada ano
  eixos <- Municipio %>% 
    group_by(Ano) %>% 
    count()
  
  Municipio %>% 
    select(Ano, Variável, Valor) %>% 
    merge(eixos) %>% 
    ggplot(aes(x = Ano, y = Valor/n)) +
    geom_bar(aes(fill = Variável), stat="identity") +
    scale_fill_manual(values = mypallete) +
    theme_bw() +
    theme(legend.position="bottom",
          strip.text = element_text(size = 6),
          legend.title = element_blank(),
          legend.direction = "vertical") +
    labs(y = paste("IDM",Eixo), x=NULL,
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.")

  }
