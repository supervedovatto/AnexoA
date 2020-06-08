rm(list = ls())

setwd(dir = "/home/supervedovatto/Documents/IFG/OMT/AnexoA/")

# Mesoregiões
library(readxl)
library(reshape2)
library(sidrar)
library(dbplyr)
library(tidyverse)

Meso <-
  tibble(
    read_delim(
      file = "Datasets/MesorregiõesIBGE.csv",
      delim = ";",
      escape_double = FALSE,
      comment = "#",
      trim_ws = TRUE
    )
  )
colnames(Meso) <- c("Mesorregiao", "Microrregiao")
Meso$Mesorregiao <- factor(Meso$Mesorregiao)
Meso$Microrregiao <- factor(Meso$Microrregiao)

# Microrregiões
Micro <-
  read_delim(
    "Datasets/MicrorregiõesIBGE.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  transmute(Microrregiao = factor(MicrorregiaoIBGE),
            Localidade = factor(Localidade)) %>%
  arrange(Localidade)

#Códigos dos Municípios no IBGE
tabela <- get_sidra(api = "/t/6579/n6/all")
MunicipiosIBGE <- tabela %>%
  filter(Ano == 2019) %>%
  separate(`Município`,
           into = c("Municipio", "UF"),
           sep = " - ") %>%
  mutate(code_muni = `Município (Código)`) %>%
  filter(UF == "GO") %>%
  select(code_muni, Municipio) %>%
  arrange(Municipio)

Micro$code_muni <- MunicipiosIBGE$code_muni

# Merge das Micro e Macrorregiões
MesoMicro <- merge(Meso, Micro, by = "Microrregiao", all = TRUE)

# Regiões de PLanejamento SEGPLAN
SEGPLAN <-
  read_delim(
    file = "Datasets/RegiõesSEGPLAN.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    trim_ws = TRUE
  ) %>%
  tibble()
colnames(SEGPLAN) <- c("RPSEGPLAN", "Localidade")
SEGPLAN$Localidade <- factor(SEGPLAN$Localidade)
SEGPLAN$RPSEGPLAN <- factor(SEGPLAN$RPSEGPLAN)

# Merge Regiões  e SEGPLAN
RegioesGoias <-
  merge(MesoMicro, SEGPLAN, by = "Localidade", all = TRUE)

# Carregando o arquivo com os dados do mapa de Goiás
MapaGoias <- read_municipality(code_muni = "GO", year = 2018) %>%
  merge(RegioesGoias, by = "code_muni", all.x = T)

# Area Territorial
Area <-
  tibble(
    read_delim(
      file = "Datasets/Área Territorial.csv",
      delim = ";",
      escape_double = FALSE,
      comment = "#",
      col_types = cols(Ano = col_date(format = "%Y")),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    )
  )
Area$Localidade <- factor(Area$Localidade)
Area$Ano <- format(Area$Ano, format = "%Y")
Area <- merge(RegioesGoias, Area, by = "Localidade", all = TRUE)

# Densidade Demográfica
DensidadeDemografica <-
  tibble(read_excel("Datasets/IMB-GYN.xlsx", sheet = "Densidade Demográfica"))
DensidadeDemografica$Localidade <-
  factor(DensidadeDemografica$Localidade)
DensidadeDemografica <-
  merge(RegioesGoias,
        DensidadeDemografica,
        by = "Localidade",
        all = TRUE)

# Emprego - CAGED - Admitidos
AdmitidosCAGED <-
  tibble(read_excel("Datasets/IMB-GYN.xlsx", sheet = "Emprego - CAGED - Admitidos"))
AdmitidosCAGED$Localidade <- factor(AdmitidosCAGED$Localidade)
AdmitidosCAGED$Total <- NULL
AdmitidosCAGED <-
  reshape2::melt(
    data = AdmitidosCAGED,
    id.vars = c("Localidade", "Ano"),
    variable.name = "Setor",
    value.name = "Admitidos"
  )

# Emprego - CAGED - Desligados
DesligadosCAGED <-
  tibble(read_excel("Datasets/IMB-GYN.xlsx", sheet = "Emprego - CAGED - Desligados"))
DesligadosCAGED$Localidade <- factor(DesligadosCAGED$Localidade)
DesligadosCAGED$Total <- NULL
DesligadosCAGED <-
  reshape2::melt(
    data = DesligadosCAGED,
    id.vars = c("Localidade", "Ano"),
    variable.name = "Setor",
    value.name = "Desligados"
  )

# Dados CAGED
CAGED <- merge(AdmitidosCAGED, DesligadosCAGED)

# Emprego - RAIS - Setores
EmpregoRAIS <-
  tibble(read_excel("Datasets/IMB-GYN.xlsx", sheet = "Emprego - RAIS - Setores"))
EmpregoRAIS$Total <- NULL
EmpregoRAIS$Localidade <- factor(EmpregoRAIS$Localidade)
EmpregoRAIS <- EmpregoRAIS %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Setor",
    value.name = "Empregos"
  )

# Rendimento - RAIS - Setores
RendimentoRAIS <-
  tibble(read_excel("Datasets/IMB-GYN.xlsx", sheet = "Rendimento - RAIS -  Setores"))
RendimentoRAIS$`Rendimento Médio` <- NULL
RendimentoRAIS$Localidade <- factor(RendimentoRAIS$Localidade)
RendimentoRAIS <- RendimentoRAIS %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Setor",
    value.name = "Rendimento Médio"
  )

# Rendimento + Empregos RAIS - Setores
RAIS <- merge(x = EmpregoRAIS, y = RendimentoRAIS)

Emprego <-
  merge(CAGED,
        RAIS,
        by = c("Localidade", "Ano", "Setor"),
        all = TRUE) %>%
  merge(RegioesGoias)

# Emprego - RAIS - Comércio
ComercioRAIS <-
  tibble(
    read_delim(
      "Datasets/Emprego - RAIS - Comércio.csv",
      delim = ";",
      escape_double = FALSE,
      comment = "#",
      col_types = cols(Ano = col_date(format = "%Y")),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    )
  )
ComercioRAIS$Localidade <- factor(ComercioRAIS$Localidade)
ComercioRAIS <- ComercioRAIS %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Subsetor",
    value.name = "Vagas"
  )

# Emprego - RAIS - Serviços
ServicosRAIS <-
  tibble(
    read_delim(
      "Datasets/Emprego - RAIS - Serviços.csv",
      delim = ";",
      escape_double = FALSE,
      comment = "#",
      col_types = cols(Ano = col_date(format = "%Y")),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    )
  )
ServicosRAIS$Localidade <- factor(ServicosRAIS$Localidade)
ServicosRAIS <- ServicosRAIS %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Subsetor",
    value.name = "Vagas"
  )

# Emprego - RAIS - Indústria de Transformação
TransformacaoRAIS <-
  tibble(
    read_delim(
      "Datasets/Emprego - RAIS - Indústria de Transformação.csv",
      delim = ";",
      escape_double = FALSE,
      comment = "#",
      col_types = cols(Ano = col_date(format = "%Y")),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    )
  )
TransformacaoRAIS$Localidade <- factor(TransformacaoRAIS$Localidade)
TransformacaoRAIS <- TransformacaoRAIS %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Subsetor",
    value.name = "Vagas"
  )

# Projeçao Projeçao
PopulacaoProjecao <-
  read_delim(
    "Datasets/PopulacaoProjeção.csv",
    delim = "\t",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  reshape2::melt(id.vars = c("Localidade", "Ano"),
                 value.name = "Quantidade") %>%
  separate(variable, into = c("Sexo", "Faixa"), sep = "/") %>%
  mutate(
    Sexo = factor(Sexo),
    Faixa = factor(Faixa, ordered = T),
    Localidade = factor(Localidade)
  )

# Abastacimento de Agua e Esgoto
Agua <- read_delim(
  "Datasets/Abastecimento de Água.csv",
  delim = ";",
  escape_double = FALSE,
  comment = "#",
  col_types = cols(Ano = col_date(format = "%Y")),
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE
) %>%
  tibble() %>%
  mutate(Localidade = factor(Localidade))
Esgoto <- read_delim(
  "Datasets/Esgoto.csv",
  delim = ";",
  escape_double = FALSE,
  comment = "#",
  col_types = cols(Ano = col_date(format = "%Y")),
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE
) %>%
  tibble() %>%
  mutate(Localidade = factor(Localidade))
AguaEsgoto <- merge(Agua,
                    Esgoto,
                    by = c("Localidade", "Ano"),
                    all = TRUE)
Ligacoes <-
  select(
    AguaEsgoto,
    -c(
      "PercentualPopulacaoAtendidaAgua",
      "PercentualPopulacaoAtendidaEsgoto"
    )
  )
colnames(Ligacoes) <- c("Localidade", "Ano", "Agua", "Esgoto")
AguaEsgotoLigacoes <-
  reshape2::melt(
    Ligacoes,
    id.vars = c("Localidade", "Ano"),
    value.name = "Ligacoes",
    variable.name = "Servico"
  )
Percentual <-
  select(AguaEsgoto, -c("LigacoesAgua", "LigacoesEsgoto"))
colnames(Percentual) <- c("Localidade", "Ano", "Agua", "Esgoto")
AguaEsgoto <- reshape2::melt(
  Percentual,
  id.vars = c("Localidade", "Ano"),
  value.name = "Percentual",
  variable.name = "Servico"
) %>%
  merge(AguaEsgotoLigacoes, by = c("Localidade", "Ano", "Servico")) %>%
  filter(!is.na(Percentual) | !is.na(Ligacoes))

#IDEB
IDEB <-
  tibble(
    read_delim(
      "Datasets/IDEB.csv",
      delim = "\t",
      escape_double = FALSE,
      comment = "#",
      col_types = cols(Ano = col_date(format = "%Y")),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    )
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Anos"
  ) %>%
  filter(!is.na(Valor)) %>%
  separate(Anos, into = c("Anos", "Rede"), sep = "/") %>%
  mutate(Anos = factor(Anos), Rede = factor(Rede))

#IDM - Economia
IDMEconomia <-
  read_delim(
    file = "Datasets/IDM Economia.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  select(-`IDM Economia`) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Variável"
  ) %>%
  mutate(IDM = "Economia")

#IDM - Educação
IDMEducacao <-
  read_delim(
    file = "Datasets/IDM Educação.csv",
    delim = "\t",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  select(-`IDM Educacao`) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Variável"
  ) %>%
  mutate(
    IDM = "Educação",
    Variável = fct_recode(
      Variável,
      "IDEB Anos Iniciais" = "IDEB 5 ano",
      "IDEB Anos Finais" = "IDEB 9 ano"
    )
  )

#IDM - Infraestrutura
IDMInfraestrutura <-
  read_delim(
    file = "Datasets/IDM Infraestrutura.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  select(-`IDM Infraestrutura`) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Variável"
  ) %>%
  mutate(IDM = "Infraestrutura")

#IDM - Saúde
IDMSaude <-
  read_delim(
    file = "Datasets/IDM Saúde.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  select(-`IDM Saude`) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Variável"
  ) %>%
  mutate(IDM = "Saúde")

#IDM - Segurança
IDMSeguranca <-
  read_delim(
    file = "Datasets/IDM Segurança.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  select(-`IDM Segurança`) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Variável"
  ) %>%
  mutate(IDM = "Segurança")

#IDM - Trabalho
IDMTrabalho <-
  read_delim(
    file = "Datasets/IDM Trabalho.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  select(-`IDM Trabalho`) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Variável"
  ) %>%
  mutate(IDM = "Trabalho")

IDM <-
  rbind(IDMEconomia,
        IDMEducacao,
        IDMInfraestrutura,
        IDMSaude,
        IDMSeguranca,
        IDMTrabalho)
rm(IDMEconomia,
   IDMEducacao,
   IDMInfraestrutura,
   IDMSaude,
   IDMSeguranca,
   IDMTrabalho)

#Docentes
Docentes <-
  read_delim(
    file = "Datasets/Docentes.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Rede",
    value.name = "Quantidade"
  )

#Estabelescimentos de Ensino
Estabelescimentos <-
  read_delim(
    file = "Datasets/Estabelecimentos de Ensino.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Rede",
    value.name = "Total"
  ) %>%
  mutate(Rede = factor(
    Rede,
    ordered = T,
    levels = c("Federal", "Estadual", "Municipal", "Particular")
  )) %>%
  filter(!is.na(Total) & Total > 0)

#Salas de Aula Usadas e Disponíveis
SalasAula <-
  read_delim(
    file = "Datasets/Salas de Aula.csv",
    delim = "\t",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    variable.name = "Rede",
    value.name = "Total"
  ) %>%
  separate(col = Rede,
           into = c("Situacao", "Rede"),
           sep = "/") %>%
  mutate(Rede = factor(
    Rede,
    ordered = T,
    levels = c("Federal", "Estadual", "Municipal", "Particular")
  ),
  Situacao = factor(Situacao)) %>%
  filter(!is.na(Total) & Total > 0)

#Matrículas
Matriculas <-
  read_delim(
    file = "Datasets/Matrículas.csv",
    delim = "\t",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  tibble() %>%
  mutate(Localidade = factor(Localidade)) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Total",
    variable.name = "Modalidade"
  ) %>%
  filter(!is.na(Total)) %>%
  separate(Modalidade,
           into = c("Modalidade", "Rede"),
           sep = "/") %>%
  mutate(Modalidade = factor(Modalidade), Rede = factor(Rede))

#Abandono
TaxaAbandono <-
  read_delim(
    file = "Datasets/Taxa de Abandono.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  rename(Abandono = `Taxa de Abandono`)

#Reprovação
TaxaReprovacao <-
  read_delim(
    file = "Datasets/Taxa de Reprovação.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  rename(Reprovação = `Taxa de Reprovação`)

# Taxa de Alfabetizaçao
TaxaAlfabetizacao <-
  read_delim(
    file = "Datasets/Taxa de Alfabetização.csv",
    delim = ";",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>%
  rename(Alfabetização = Alfabetizacao)

Taxas <-
  merge(TaxaAbandono,
        TaxaReprovacao,
        by = c("Localidade", "Ano"),
        all = T) %>%
  merge(TaxaAlfabetizacao,
        by = c("Localidade", "Ano"),
        all = T) %>%
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "Taxa"
  ) %>%
  filter(!is.na(Valor))

Gini <-
  read_delim(
    file = "Datasets/Gini.csv",
    delim = "\t",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade))

IDH <-
  read_delim(
    file = "Datasets/IDHM.csv",
    delim = "\t",
    escape_double = FALSE,
    comment = "#",
    col_types = cols(Ano = col_date(format = "%Y")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  ) %>%
  mutate(Localidade = factor(Localidade)) %>% 
  reshape2::melt(
    id.vars = c("Localidade", "Ano"),
    value.name = "Valor",
    variable.name = "IDHM"
  )

# Produto Interno Bruto a preços correntes
PIBpc <- get_sidra(x = 5938, geo = "City", variable = 37) %>%
  separate(`Município`,
           into = c("Localidade", "UF"),
           sep = " - ") %>%
  mutate(code_muni = `Município (Código)`) %>%
  filter(UF == "GO") %>%
  select(code_muni, Localidade, Ano, Valor) %>%
  arrange(Localidade) %>% 
  mutate(Ano = as.Date(Ano,format = "%Y"),Localidade = factor(Localidade))

rm(
  Meso,
  Micro,
  MesoMicro,
  SEGPLAN,
  AguaEsgotoLigacoes,
  AguaEsgotoPercentual,
  Percentual,
  Agua,
  TaxaAlfabetizacao,
  Estabelecimentos,
  TaxaAbandono,
  TaxaReprovacao,
  AdmitidosCAGED,
  DesligadosCAGED,
  Ligacoes
)

save.image(file = "Datasets/BDE.RData")