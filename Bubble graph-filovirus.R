### Bubble graph
library(readxl)
Filovirus <- read_excel("C:/Users/TaeheeCH/Desktop/Filoviruses/Filovirus bubble graph.xlsx")

library(tidyverse)
Filovirus_1 <- Filovirus %>% 
  filter(Cases < 3000)

install.packages("ggrepel")
library(ggrepel)

library(ggplot2)
ggplot(Filovirus_1 , aes(x=Date, y = Cases)) +
  geom_point(aes(size=Cases, fill=Species, color = Species), shape=21) + 
  theme_bw() + ylim(0, 600) +
  geom_text_repel(aes(label = Outbreak), size = 3) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  )





