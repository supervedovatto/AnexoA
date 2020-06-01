source(file = "Maps/Microrregiões.R")
source(file = "Barplots/PopulaçãoMicrorregião.R")

ArranjoMicrorregiao <- ggarrange(
  MapsMicrorregioes,
  BarplotsPopulacaoMicrorregiao,
  common.legend = TRUE,
  align = "v",
  legend = "bottom"
)