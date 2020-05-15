rm(list = ls())

# Mesoregiões
library(readxl)
library(tidyverse)
library(reshape2)

Meso <- tibble(read_delim("Dados/MesorregiõesIBGE.csv", delim = ";", escape_double = FALSE, comment = "#", trim_ws = TRUE))
colnames(Meso) <- c("Mesorregiao","Microrregiao")
Meso$Mesorregiao <- factor(Meso$Mesorregiao)
Meso$Microrregiao <- factor(Meso$Microrregiao)

# Microrregiões
Micro <- tibble(read_delim("Dados/MicrorregiõesIBGE.csv", delim = ";", escape_double = FALSE, comment = "#", trim_ws = TRUE))
colnames(Micro) <- c("Microrregiao","Localidade")
Micro$Localidade <- factor(Micro$Localidade)
Micro$Microrregiao <- factor(Micro$Microrregiao)

# Merge das Micro e Macrorregiões 
MesoMicro <- merge(Meso,Micro,by = "Microrregiao",all = TRUE)

# Regiões de PLanejamento SEGPLAN
SEGPLAN <- tibble(read_delim("Dados/RegiõesSEGPLAN.csv", delim = ";", escape_double = FALSE, comment = "#", trim_ws = TRUE))
colnames(SEGPLAN) <- c("RPSEGPLAN","Localidade")
SEGPLAN$Localidade <- factor(SEGPLAN$Localidade)
SEGPLAN$RPSEGPLAN <- factor(SEGPLAN$RPSEGPLAN)

# Merge Regiões  e SEGPLAN
RegioesGoias <- merge(MesoMicro,SEGPLAN,by = "Localidade",all = TRUE)

# Area Territorial
Area <- tibble(read_delim("Dados/Área Territorial.csv",delim = ";", escape_double = FALSE, comment = "#", col_types = cols(Ano = col_date(format = "%Y")), locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE))
Area$Localidade <- factor(Area$Localidade)
Area <- merge(RegioesGoias,Area,by = "Localidade",all = TRUE)

# Densidade Demográfica
DensidadeDemografica <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Densidade Demográfica"))
DensidadeDemografica$Localidade <- factor(DensidadeDemografica$Localidade)
DensidadeDemografica <- merge(RegioesGoias,DensidadeDemografica,by = "Localidade",all = TRUE)

# Taxa de Alfabetizaçao
TaxaAlfabetizacao <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Taxa de Alfabetização"))
TaxaAlfabetizacao$Localidade <- factor(TaxaAlfabetizacao$Localidade)
TaxaAlfabetizacao <- merge(RegioesGoias,TaxaAlfabetizacao,by = "Localidade",all = TRUE)

# Emprego - CAGED - Admitidos
AdmitidosCAGED <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - CAGED - Admitidos"))
AdmitidosCAGED$Localidade <- factor(AdmitidosCAGED$Localidade)
AdmitidosCAGED$Total <- NULL
AdmitidosCAGED <- reshape2::melt(data = AdmitidosCAGED,id.vars = c("Localidade","Ano"),variable.name = "Setor",value.name = "Admitidos")

# Emprego - CAGED - Desligados
DesligadosCAGED <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - CAGED - Desligados"))
DesligadosCAGED$Localidade <- factor(DesligadosCAGED$Localidade)
DesligadosCAGED$Total <- NULL
DesligadosCAGED <- reshape2::melt(data = DesligadosCAGED,id.vars = c("Localidade","Ano"),variable.name = "Setor",value.name = "Desligados")

# Dados CAGED
CAGED <- merge(AdmitidosCAGED,DesligadosCAGED)

# Emprego - RAIS - Setores
EmpregoRAIS <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - RAIS - Setores"))
EmpregoRAIS$Total <- NULL
EmpregoRAIS$Localidade <- factor(EmpregoRAIS$Localidade)
EmpregoRAIS <- EmpregoRAIS %>% 
  reshape2::melt(id.vars = c("Localidade","Ano"),
                 variable.name="Setor",
                 value.name="Empregos")

# Rendimento - RAIS - Setores
RendimentoRAIS <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Rendimento - RAIS -  Setores"))
RendimentoRAIS$`Rendimento Médio` <- NULL
RendimentoRAIS$Localidade <- factor(RendimentoRAIS$Localidade)
RendimentoRAIS <- RendimentoRAIS %>% 
  reshape2::melt(id.vars = c("Localidade","Ano"),
                 variable.name="Setor",
                 value.name="Rendimento Médio")

# Rendimento + Empregos RAIS - Setores
RAIS <- merge(x = EmpregoRAIS,y = RendimentoRAIS)

Emprego <- merge(CAGED,RAIS,by = c("Localidade","Ano","Setor"),all = TRUE) %>% 
  merge(RegioesGoias)

# Emprego - RAIS - Comércio
ComercioRAIS <- tibble(read_delim("Dados/Emprego - RAIS - Comércio.csv",delim = ";", escape_double = FALSE, comment = "#", col_types = cols(Ano = col_date(format = "%Y")), locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE))
ComercioRAIS$Localidade <- factor(ComercioRAIS$Localidade)
ComercioRAIS <- ComercioRAIS %>% 
  reshape2::melt(id.vars = c("Localidade","Ano"),
                 variable.name="Subsetor",
                 value.name="Vagas")

# Emprego - RAIS - Serviços
ServicosRAIS <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - RAIS - Serviços"))
ServicosRAIS$Localidade <- factor(ServicosRAIS$Localidade)
ServicosRAIS <- ServicosRAIS %>% 
  reshape2::melt(id.vars = c("Localidade","Ano"),
                 variable.name="Subsetor",
                 value.name="Vagas")

# Emprego - RAIS - Indústria de Transformação
TransformacaoRAIS <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - RAIS - Indústria de Transformação"))
TransformacaoRAIS$Localidade <- factor(TransformacaoRAIS$Localidade)
TransformacaoRAIS <- TransformacaoRAIS %>% 
  reshape2::melt(id.vars = c("Localidade","Ano"),
                 variable.name="Subsetor",
                 value.name="Vagas")

# Projeçao Projeçao
PopulacaoProjecao <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "PopulacaoProjeção"))
PopulacaoProjecao$Localidade <- factor(PopulacaoProjecao$Localidade)
LocalAno <- select(PopulacaoProjecao, -starts_with(c("Masculina","Feminina")))
Masculina <- select(PopulacaoProjecao, starts_with("Masculina"))
Feminina <- select(PopulacaoProjecao, starts_with("Feminina"))
FaixasEtarias <- sub(pattern = "Masculina ",replacement = "",colnames(Masculina))
colnames(Masculina) <- colnames(Feminina) <- FaixasEtarias
Masculina$Sexo <- factor("Masculino")
Feminina$Sexo <- factor("Feminino")
Masculina <- cbind(LocalAno,Masculina)
Feminina <- cbind(LocalAno,Feminina)
PopulacaoProjecao <- rbind(Masculina,Feminina)
rm(Feminina,Masculina)
PopulacaoProjecao <- reshape2::melt(data = PopulacaoProjecao,id.vars = c("Localidade","Ano","Sexo"),variable.name = "Faixa",value.name = "Quantidade")
PopulacaoProjecao <- merge(x = RegioesGoias,y = PopulacaoProjecao,by=c("Localidade"), all = TRUE)
PopulacaoProjecao$Faixa <- factor(PopulacaoProjecao$Faixa,ordered = TRUE)
PopulacaoProjecao$FaseVida <- NA
P1 <- PopulacaoProjecao %>% filter(Faixa %in% c("30 a 39","20 a 29","40 a 49","50 a 59")) %>% mutate(FaseVida = "Adultos")
P2 <- PopulacaoProjecao %>% filter(Faixa %in% c("15 a 19")) %>% mutate(FaseVida = "Jovens")
P3 <- PopulacaoProjecao %>% filter(Faixa %in% c("10 a 14")) %>% mutate(FaseVida = "Adolescentes")
P4 <- PopulacaoProjecao %>% filter(Faixa %in% c("0 a 4","5 a 9")) %>% mutate(FaseVida = "Crianças")
P5 <- PopulacaoProjecao %>% filter(Faixa %in% c("60 a 69","70 a 79","80 ou mais")) %>%  mutate(FaseVida = "Idosos")
PopulacaoProjecao <- rbind(P1,P2,P3,P4,P5)
PopulacaoProjecao$FaseVida <- factor(PopulacaoProjecao$FaseVida,
                                     ordered = TRUE,
                                     levels = c("Crianças","Adolescentes","Jovens","Adultos","Idosos"))


# Abastacimento de Agua e Esgoto
AtendimentoAgua <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Abastecimento de Água"))
AtendimentoAgua$Localidade <- factor(AtendimentoAgua$Localidade)
Esgoto <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Esgoto"))
Esgoto$Localidade <- factor(Esgoto$Localidade)
AguaEsgoto <- merge(x = AtendimentoAgua,y = Esgoto,by=c("Localidade","Ano"),all = TRUE)
rm(AtendimentoAgua,Esgoto)
Ligacoes <- select(AguaEsgoto,-c("PercentualPopulacaoAtendidaAgua","PercentualPopulacaoAtendidaEsgoto"))
colnames(Ligacoes) <- c("Localidade","Ano","Agua","Esgoto")
AguaEsgotoLigacoes <- reshape2::melt(Ligacoes,id.vars = c("Localidade","Ano"),value.name = "Ligacoes",variable.name = "Servico")
Percentual <- select(AguaEsgoto,-c("LigacoesAgua","LigacoesEsgoto"))
colnames(Percentual) <- c("Localidade","Ano","Agua","Esgoto")
AguaEsgotoPercentual <- reshape2::melt(Percentual,
                             id.vars = c("Localidade","Ano"),
                             value.name = "Percentual",
                             variable.name = "Servico")
AguaEsgoto <- merge(x = AguaEsgotoLigacoes,
                    y = AguaEsgotoPercentual,
                    by = c("Localidade","Ano","Servico"))
AguaEsgoto <- merge(x = RegioesGoias,
                    y = AguaEsgoto,
                    by = c("Localidade"))

#IDEB
IDEB <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDEB"))
IDEB$Localidade <- factor(IDEB$Localidade)
IDEB <- merge(x = RegioesGoias,y = IDEB,by=c("Localidade"),all = TRUE)

#IDM - Economia
IDMEconomia <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Economia"))
IDMEconomia$Localidade <- factor(IDMEconomia$Localidade)
IDMEconomia <- merge(x = RegioesGoias,y = IDMEconomia,by=c("Localidade"),all = TRUE)

#IDM - Educação
IDMEducacao <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Educação"))
IDMEducacao$Localidade <- factor(IDMEducacao$Localidade)
IDMEducacao <- merge(x = RegioesGoias,y = IDMEducacao,by=c("Localidade"),all = TRUE)

#IDM - Infraestrutura
IDMInfraestrutura <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Infraestrutura"))
IDMInfraestrutura$Localidade <- factor(IDMInfraestrutura$Localidade)
IDMInfraestrutura <- merge(x = RegioesGoias,y = IDMInfraestrutura,by=c("Localidade"),all = TRUE)

#IDM - Saúde
IDMSaude <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Saúde"))
IDMSaude$Localidade <- factor(IDMSaude$Localidade)
IDMSaude <- merge(x = RegioesGoias,y = IDMSaude,by=c("Localidade"),all = TRUE)

#IDM - Segurança
IDMSeguranca <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Segurança"))
IDMSeguranca$Localidade <- factor(IDMSeguranca$Localidade)
IDMSeguranca <- merge(x = RegioesGoias,y = IDMSeguranca,by=c("Localidade"),all = TRUE)

#IDM - Trabalho
IDMTrabalho <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Trabalho"))
IDMTrabalho$Localidade <- factor(IDMTrabalho$Localidade)
IDMTrabalho <- merge(x = RegioesGoias,y = IDMTrabalho,by=c("Localidade"),all = TRUE)

#Docentes
Docentes <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Docentes"))
Docentes$Localidade <- factor(Docentes$Localidade)
Docentes <- reshape2::melt(Docentes,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Quantidade")
Docentes <- merge(x = RegioesGoias,y = Docentes,by=c("Localidade"),all = TRUE)

#Estabelescimentos de Ensino
EstabelecimentosESalas <- tibble(read_excel("Dados/IMB-GYN.xlsx",sheet = "Estabelecimentos de Ensino"))
EstabelecimentosESalas$Localidade <- factor(EstabelecimentosESalas$Localidade)
Estabelecimentos <- select(EstabelecimentosESalas, -starts_with("Salas"))
SalasExistentes <- select(EstabelecimentosESalas, -starts_with("Estabelecimentos") & -contains("Utilizadas"))
SalasUtilizadas <- select(EstabelecimentosESalas, -starts_with("Estabelecimentos") & -contains("Existentes"))
colnames(Estabelecimentos)[-c(1,2)] <- colnames(SalasExistentes)[-c(1,2)] <- colnames(SalasUtilizadas)[-c(1,2)] <- c("Federal","Estadual","Municipal","Particular")
Estabelecimentos <- reshape2::melt(Estabelecimentos,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Estabelecimentos")
SalasExistentes <- reshape2::melt(SalasExistentes,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Salas Existentes")
SalasUtilizadas <- reshape2::melt(SalasUtilizadas,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Salas Utilizadas")
EstabelecimentosESalas <- Estabelecimentos %>% 
  merge(SalasExistentes,by=c("Localidade","Ano","Rede"),all = TRUE) %>% 
  merge(SalasUtilizadas,by=c("Localidade","Ano","Rede"),all = TRUE) %>% 
  merge(RegioesGoias,by=c("Localidade"),all = TRUE)

#Matrículas
Matrículas <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Matrículas"))
Matrículas$Localidade <- factor(Matrículas$Localidade)
Matrículas <- merge(x = RegioesGoias,y = Matrículas,by=c("Localidade"),all = TRUE)

#Abandono
TaxaAbandono <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Taxa de Abandono"))
TaxaAbandono$Localidade <- factor(TaxaAbandono$Localidade)
TaxaAbandono <- merge(x = RegioesGoias,y = TaxaAbandono,by=c("Localidade"),all = TRUE)

#Reprovação
TaxaReprovação <- tibble(read_excel("Dados/IMB-GYN.xlsx", sheet = "Taxa de Reprovação"))
TaxaReprovação$Localidade <- factor(TaxaReprovação$Localidade)
TaxaReprovação <- merge(x = RegioesGoias,y = TaxaReprovação,by=c("Localidade"),all = TRUE)

rm(LocalAno,Meso,Micro,MesoMicro,SEGPLAN,P1,P2,P3,P4,P5,FaixasEtarias)

save.image(file = "Dados/BDE.RData")