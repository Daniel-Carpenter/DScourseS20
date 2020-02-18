library(tidyverse)
library(ggthemes)

ggplot(data = mpg, aes(x = cty, y = hwy, 
        color = year)) + 
        geom_point() +
        theme_gdocs() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        labs(y = "Highway Fuel Economy", x = "City Fuel Economy")
