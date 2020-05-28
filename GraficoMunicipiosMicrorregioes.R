grafico <- RegioesGoias %>% 
  filter(Mesorregiao == LocRef$Mesorregiao) %>% 
  ggplot(aes(x=factor(Microrregiao),fill=Microrregiao)) +
  geom_bar(stat = "count") +
  scale_fill_manual(values = mypallete) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x="", y="",fill = paste("Microrregiões do",LocRef$Mesorregiao))
