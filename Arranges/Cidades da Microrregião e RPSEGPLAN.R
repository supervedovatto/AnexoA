source(file = "Maps/RPSEGPLANCidades.R")
source(file = "Maps/MicrorregiãoCidades.R")

ArrangeCidadesMicroRPSEGPLAN <- ggarrange(MicrorregiaoCidades, RPSEGPLANcidades, align = "h")