rm(list = ls())

# Mesoregiões
library(readxl)
library(data.table)
library(dplyr)
Meso <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "MesorregiõesIBGE"))
colnames(Meso) <- c("Mesorregiao","Microrregiao")
Meso$Mesorregiao <- factor(Meso$Mesorregiao)
Meso$Microrregiao <- factor(Meso$Microrregiao)

# Microrregiões
Micro <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "MicrorregiõesIBGE"))
colnames(Micro) <- c("Microrregiao","Localidade")
Micro$Localidade <- factor(Micro$Localidade)
Micro$Microrregiao <- factor(Micro$Microrregiao)

# Merge das Micro e Macrorregiões 
MesoMicro <- merge(Meso,Micro,by = "Microrregiao",all = TRUE)

# Regiões de PLanejamento SEGPLAN
SEGPLAN <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "RegiõesSEGPLAN"))
colnames(SEGPLAN) <- c("RPSEGPLAN","Localidade")
SEGPLAN$Localidade <- factor(SEGPLAN$Localidade)
SEGPLAN$RPSEGPLAN <- factor(SEGPLAN$RPSEGPLAN)

# Merge Regiões  e SEGPLAN
RegioesGoias <- merge(MesoMicro,SEGPLAN,by = "Localidade",all = TRUE)

# Area Territorial
Area <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Área Territorial"))
Area$Localidade <- factor(Area$Localidade)
Area <- merge(RegioesGoias,Area,by = "Localidade",all = TRUE)

# Densidade Demográfica
DensidadeDemografica <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Densidade Demográfica"))
DensidadeDemografica$Localidade <- factor(DensidadeDemografica$Localidade)
DensidadeDemografica <- merge(RegioesGoias,DensidadeDemografica,by = "Localidade",all = TRUE)

# Taxa de Alfabetizaçao
TaxaAlfabetizacao <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Taxa de Alfabetização"))
TaxaAlfabetizacao$Localidade <- factor(TaxaAlfabetizacao$Localidade)
TaxaAlfabetizacao <- merge(RegioesGoias,TaxaAlfabetizacao,by = "Localidade",all = TRUE)

# Emprego CAGED
EmpregoCAGED <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - CAGED"))
EmpregoCAGED$Localidade <- factor(EmpregoCAGED$Localidade)
LocalAno <- select(EmpregoCAGED, -ends_with(c("Admitidos","Desligados")))
Admitidos <- select(EmpregoCAGED, ends_with("Admitidos"))
EmpregoLista <- sub(pattern = " Admitidos",replacement = "",colnames(Admitidos))
colnames(Admitidos) <- EmpregoLista
Admitidos <- cbind(LocalAno,Admitidos)
Admitidos$Situacao <- factor("Admitidos")
Desligados <- select(EmpregoCAGED, ends_with("Desligados"))
colnames(Desligados) <- EmpregoLista
Desligados$Situacao <- factor("Desligados")
Desligados <- cbind(LocalAno,Desligados)
EmpregoCAGED <- rbind(Admitidos,Desligados)
rm(Admitidos,Desligados)
EmpregoCAGED$Total <- NULL
EmpregoCAGED <- melt(data = EmpregoCAGED,id.vars = c("Localidade","Ano","Situacao"),variable.name = "Setor",value.name = "Quantidade")
EmpregoCAGED <- merge(x = RegioesGoias,y = EmpregoCAGED,by=c("Localidade"), all = TRUE)

# Emprego RAIS
EmpregoRAIS <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Emprego - RAIS"))
EmpregoRAIS$Localidade <- factor(EmpregoRAIS$Localidade)
EmpregoRAIS <- merge(x = RegioesGoias,y = EmpregoRAIS,by=c("Localidade"),all = TRUE)

# Projeçao Projeçao
PopulacaoProjecao <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "PopulacaoProjeção"))
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
PopulacaoProjecao <- melt(data = PopulacaoProjecao,id.vars = c("Localidade","Ano","Sexo"),variable.name = "Faixa",value.name = "Quantidade")
PopulacaoProjecao <- merge(x = RegioesGoias,y = PopulacaoProjecao,by=c("Localidade"), all = TRUE)
PopulacaoProjecao$Faixa <- factor(PopulacaoProjecao$Faixa,ordered = TRUE)

# Abastacimento de Agua e Esgoto
AtendimentoAgua <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Abastecimento de Água"))
AtendimentoAgua$Localidade <- factor(AtendimentoAgua$Localidade)
Esgoto <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Esgoto"))
Esgoto$Localidade <- factor(Esgoto$Localidade)
AguaEsgoto <- merge(x = AtendimentoAgua,y = Esgoto,by=c("Localidade","Ano"),all = TRUE)
rm(AtendimentoAgua,Esgoto)
Ligacoes <- select(AguaEsgoto,-c("PercentualPopulacaoAtendidaAgua","PercentualPopulacaoAtendidaEsgoto"))
colnames(Ligacoes) <- c("Localidade","Ano","Agua","Esgoto")
AguaEsgotoLigacoes <- melt(Ligacoes,id.vars = c("Localidade","Ano"),value.name = "Ligacoes",variable.name = "Servico")
Percentual <- select(AguaEsgoto,-c("LigacoesAgua","LigacoesEsgoto"))
colnames(Percentual) <- c("Localidade","Ano","Agua","Esgoto")
AguaEsgotoPercentual <- melt(Percentual,
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
IDEB <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDEB"))
IDEB$Localidade <- factor(IDEB$Localidade)
IDEB <- merge(x = RegioesGoias,y = IDEB,by=c("Localidade"),all = TRUE)

#IDM - Economia
IDMEconomia <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Economia"))
IDMEconomia$Localidade <- factor(IDMEconomia$Localidade)
IDMEconomia <- merge(x = RegioesGoias,y = IDMEconomia,by=c("Localidade"),all = TRUE)

#IDM - Educação
IDMEducacao <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Educação"))
IDMEducacao$Localidade <- factor(IDMEducacao$Localidade)
IDMEducacao <- merge(x = RegioesGoias,y = IDMEducacao,by=c("Localidade"),all = TRUE)

#IDM - Infraestrutura
IDMInfraestrutura <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Infraestrutura"))
IDMInfraestrutura$Localidade <- factor(IDMInfraestrutura$Localidade)
IDMInfraestrutura <- merge(x = RegioesGoias,y = IDMInfraestrutura,by=c("Localidade"),all = TRUE)

#IDM - Saúde
IDMSaude <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Saúde"))
IDMSaude$Localidade <- factor(IDMSaude$Localidade)
IDMSaude <- merge(x = RegioesGoias,y = IDMSaude,by=c("Localidade"),all = TRUE)

#IDM - Segurança
IDMSeguranca <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Segurança"))
IDMSeguranca$Localidade <- factor(IDMSeguranca$Localidade)
IDMSeguranca <- merge(x = RegioesGoias,y = IDMSeguranca,by=c("Localidade"),all = TRUE)

#IDM - Trabalho
IDMTrabalho <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "IDM Trabalho"))
IDMTrabalho$Localidade <- factor(IDMTrabalho$Localidade)
IDMTrabalho <- merge(x = RegioesGoias,y = IDMTrabalho,by=c("Localidade"),all = TRUE)

#Docentes
Docentes <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Docentes"))
Docentes$Localidade <- factor(Docentes$Localidade)
Docentes <- melt(Docentes,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Quantidade")
Docentes <- merge(x = RegioesGoias,y = Docentes,by=c("Localidade"),all = TRUE)

#Estabelescimentos de Ensino
EstabelecimentosESalas <- data.table(read_excel("Dados/IMB-GYN.xlsx",sheet = "Estabelecimentos de Ensino"))
EstabelecimentosESalas$Localidade <- factor(EstabelecimentosESalas$Localidade)
Estabelecimentos <- select(EstabelecimentosESalas, -starts_with("Salas"))
SalasExistentes <- select(EstabelecimentosESalas, -starts_with("Estabelecimentos") & -contains("Utilizadas"))
SalasUtilizadas <- select(EstabelecimentosESalas, -starts_with("Estabelecimentos") & -contains("Existentes"))
colnames(Estabelecimentos)[-c(1,2)] <- colnames(SalasExistentes)[-c(1,2)] <- colnames(SalasUtilizadas)[-c(1,2)] <- c("Federal","Estadual","Municipal","Particular")
Estabelecimentos <- melt(Estabelecimentos,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Estabelecimentos")
SalasExistentes <- melt(SalasExistentes,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Salas Existentes")
SalasUtilizadas <- melt(SalasUtilizadas,id.vars = c("Localidade","Ano"),variable.name = "Rede",value.name = "Salas Utilizadas")
EstabelecimentosESalas <- Estabelecimentos %>% 
  merge(SalasExistentes,by=c("Localidade","Ano","Rede"),all = TRUE) %>% 
  merge(SalasUtilizadas,by=c("Localidade","Ano","Rede"),all = TRUE) %>% 
  merge(RegioesGoias,by=c("Localidade"),all = TRUE)

#Matrículas
Matrículas <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Matrículas"))
Matrículas$Localidade <- factor(Matrículas$Localidade)
Matrículas <- merge(x = RegioesGoias,y = Matrículas,by=c("Localidade"),all = TRUE)

#Abandono
TaxaAbandono <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Taxa de Abandono"))
TaxaAbandono$Localidade <- factor(TaxaAbandono$Localidade)
TaxaAbandono <- merge(x = RegioesGoias,y = TaxaAbandono,by=c("Localidade"),all = TRUE)

#Reprovação
TaxaReprovação <- data.table(read_excel("Dados/IMB-GYN.xlsx", sheet = "Taxa de Reprovação"))
TaxaReprovação$Localidade <- factor(TaxaReprovação$Localidade)
TaxaReprovação <- merge(x = RegioesGoias,y = TaxaReprovação,by=c("Localidade"),all = TRUE)

rm(LocalAno,Meso,Micro,MesoMicro,SEGPLAN)

save.image(file = "BDE.RData")