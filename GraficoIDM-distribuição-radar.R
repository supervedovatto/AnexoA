ComposicaoIDM <- function(Eixo){
    
  load(file = "Dados/POCV.RData")
  
  level1 <- paste("Mun. de",LocRef$Localidade)
  tabela1 <- IDM %>% 
    filter(!is.na(Valor) & Localidade == LocRef$Localidade & Ano == max(Ano)) %>% 
    group_by(IDM) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    mutate(Referencia = level1)
  
  level2 <- paste("RP",LocRef$RPSEGPLAN)
  tabela2 <- IDM %>% 
    merge(RegioesGoias) %>% 
    filter(!is.na(Valor) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>% 
    group_by(Localidade,IDM) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    group_by(IDM) %>% 
    summarise(Valor = median(Valor,rm.na = TRUE)) %>% 
    mutate(Referencia = level2)
  
  level3 <- "Estado de Goiás"
  tabela3 <- IDM %>% 
    filter(!is.na(Valor) & Ano == max(Ano)) %>% 
    group_by(Localidade,IDM) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    group_by(IDM) %>% 
    summarise(Valor = median(Valor,rm.na = TRUE)) %>% 
    mutate(Referencia = level3)
  
  rbind(tabela1,tabela2,tabela3) %>% 
    mutate(Referencia = factor(Referencia,ordered = T,levels = c(level1,level2,level3))) %>% 
    dcast(Referencia~IDM,value.var = "Valor") %>% 
    ggradar(grid.max = 10,values.radar = "",
            legend.position = "bottom",
            legend.text.size = 8,group.line.width = 0.5,group.point.size = 3,
            axis.label.size = 3)
  
}