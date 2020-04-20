#Script que lê todas as planilhas do arquivo IMB-GYN.xlsx e transforma em um banco de dados RDATA para ser manipulado pelo R
rm(list = ls())
# Mesoregiões
Meso <- data.table(read_excel("IMB-GYN.xlsx", sheet = "MesorregiõesIBGE"))
Meso$MesorregiaoIBGE <- factor(Meso$MesorregiaoIBGE)
Meso$MicrorregiaoIBGE <- factor(Meso$MicrorregiaoIBGE)

# Microrregiões
Micro <- data.table(read_excel("IMB-GYN.xlsx", sheet = "MicrorregiõesIBGE"))
Micro$Localidade <- factor(Micro$Localidade)
Micro$MicrorregiaoIBGE <- factor(Micro$MicrorregiaoIBGE)

# Merge das Micro e Macrorregiões IBGE
MesoMicro <- merge(Meso,Micro,by = "MicrorregiaoIBGE",all = TRUE)

# Regiões de PLanejamento SEPLAN
SEPLAN <- data.table(read_excel("IMB-GYN.xlsx", sheet = "RegiõesSEPLAN"))
SEPLAN$Localidade <- factor(SEPLAN$Localidade)
SEPLAN$RegiaoSEPLAN <- factor(SEPLAN$RegiaoSEPLAN)

# Area Territorial
Area <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Área Territorial"))
Area$Localidade <- factor(Area$Localidade)
Area2016 <- filter(Area,Ano == "2016")

# Merge Regiões IBGE e SEPLAN
RegioesGoias <- merge(MesoMicro,SEPLAN,by = "Localidade",all = TRUE)

# Densidade Demográfica
DensidadeDemografica <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Densidade Demográfica"))
DensidadeDemografica$Localidade <- factor(DensidadeDemografica$Localidade)
DensidadeDemografica <- merge(RegioesGoias,DensidadeDemografica,by = "Localidade",all = TRUE)

# Taxa de Alfabetizaçao
TaxaAlfabetizacao <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Taxa de Alfabetização"))
TaxaAlfabetizacao$Localidade <- factor(TaxaAlfabetizacao$Localidade)
TaxaAlfabetizacao <- merge(RegioesGoias,TaxaAlfabetizacao,by = "Localidade",all = TRUE)

# Emprego CAGED
EmpregoCAGED <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Emprego - CAGED"))
EmpregoCAGED$Localidade <- factor(EmpregoCAGED$Localidade)
Categorias <- select(EmpregoCAGED, -ends_with(c("Admitidos","Desligados")))
Admitidos <- select(EmpregoCAGED, ends_with("Admitidos"))
Emprego.lista <- sub(pattern = " Admitidos",replacement = "",colnames(Admitidos))
colnames(Admitidos) <- Emprego.lista
Admitidos <- cbind(Categorias,Admitidos)
Admitidos$Situacao <- factor("Admitidos")
Desligados <- select(EmpregoCAGED, ends_with("Desligados"))
colnames(Desligados) <- Emprego.lista
Desligados$Situacao <- factor("Desligados")
Desligados <- cbind(Categorias,Desligados)
EmpregoCAGED <- rbind(Admitidos,Desligados)
EmpregoCAGED$Total <- NULL
EmpregoCAGED <- melt(data = EmpregoCAGED,id.vars = c("Localidade","Ano","Situacao"),variable.name = "Setor",value.name = "Total")
EmpregoCAGED <- merge(x = RegioesGoias,y = EmpregoCAGED,by=c("Localidade"), all = TRUE)

# Emprego RAIS
EmpregoRAIS <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Emprego - RAIS"))
EmpregoRAIS$Localidade <- factor(EmpregoRAIS$Localidade)
EmpregoRAIS <- merge(x = RegioesGoias,y = EmpregoRAIS,by=c("Localidade"),all = TRUE)

# Projeçao Projeçao
PopulacaoProjecao <- data.table(read_excel("IMB-GYN.xlsx", sheet = "PopulacaoProjeção"))
PopulacaoProjecao$Localidade <- factor(PopulacaoProjecao$Localidade)
PopulacaoProjecao <- merge(x = RegioesGoias,y = PopulacaoProjecao,by=c("Localidade"),all = TRUE)

# Abastacimento de Agua e Esgoto
AtendimentoAgua <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Abastecimento de Água"))
AtendimentoAgua$Localidade <- factor(AtendimentoAgua$Localidade)
Esgoto <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Esgoto"))
Esgoto$Localidade <- factor(Esgoto$Localidade)
AguaEsgoto <- merge(x = AtendimentoAgua,y = Esgoto,by=c("Localidade","Ano"),all = TRUE)
AguaEsgoto <- merge(x = RegioesGoias,y = AguaEsgoto,by=c("Localidade"),all = TRUE)

#IDEB
IDEB <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDEB"))
IDEB$Localidade <- factor(IDEB$Localidade)
IDEB <- merge(x = RegioesGoias,y = IDEB,by=c("Localidade"),all = TRUE)

#IDM - Economia
IDMEconomia <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDM Economia"))
IDMEconomia$Localidade <- factor(IDMEconomia$Localidade)
IDMEconomia <- merge(x = RegioesGoias,y = IDMEconomia,by=c("Localidade"),all = TRUE)

#IDM - Educação
IDMEducacao <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDM Educação"))
IDMEducacao$Localidade <- factor(IDMEducacao$Localidade)
IDMEducacao <- merge(x = RegioesGoias,y = IDMEducacao,by=c("Localidade"),all = TRUE)

#IDM - Infraestrutura
IDMInfraestrutura <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDM Infraestrutura"))
IDMInfraestrutura$Localidade <- factor(IDMInfraestrutura$Localidade)
IDMInfraestrutura <- merge(x = RegioesGoias,y = IDMInfraestrutura,by=c("Localidade"),all = TRUE)

#IDM - Saúde
IDMSaude <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDM Saúde"))
IDMSaude$Localidade <- factor(IDMSaude$Localidade)
IDMSaude <- merge(x = RegioesGoias,y = IDMSaude,by=c("Localidade"),all = TRUE)

#IDM - Segurança
IDMSeguranca <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDM Segurança"))
IDMSeguranca$Localidade <- factor(IDMSeguranca$Localidade)
IDMSeguranca <- merge(x = RegioesGoias,y = IDMSeguranca,by=c("Localidade"),all = TRUE)

#IDM - Trabalho
IDMTrabalho <- data.table(read_excel("IMB-GYN.xlsx", sheet = "IDM Trabalho"))
IDMTrabalho$Localidade <- factor(IDMTrabalho$Localidade)
IDMTrabalho <- merge(x = RegioesGoias,y = IDMTrabalho,by=c("Localidade"),all = TRUE)

#Docentes
Docentes <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Docentes"))
Docentes$Localidade <- factor(Docentes$Localidade)
Docentes <- merge(x = RegioesGoias,y = Docentes,by=c("Localidade"),all = TRUE)

#Estabelescimentos de Ensino
EstabelecimentosESalas <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Estabelecimentos de Ensino"))
EstabelecimentosESalas$Localidade <- factor(EstabelecimentosESalas$Localidade)
EstabelecimentosESalas <- merge(x = RegioesGoias,y = EstabelecimentosESalas,by=c("Localidade"),all = TRUE)

#Matrículas
Matrículas <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Matrículas"))
Matrículas$Localidade <- factor(Matrículas$Localidade)
Matrículas <- merge(x = RegioesGoias,y = Matrículas,by=c("Localidade"),all = TRUE)

#Abandono
TaxaAbandono <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Taxa de Abandono"))
TaxaAbandono$Localidade <- factor(TaxaAbandono$Localidade)
TaxaAbandono <- merge(x = RegioesGoias,y = TaxaAbandono,by=c("Localidade"),all = TRUE)

#Reprovação
TaxaReprovação <- data.table(read_excel("IMB-GYN.xlsx", sheet = "Taxa de Reprovação"))
TaxaReprovação$Localidade <- factor(TaxaReprovação$Localidade)
TaxaReprovação <- merge(x = RegioesGoias,y = TaxaReprovação,by=c("Localidade"),all = TRUE)

save.image(file = "BDE.RData")