---
title: "R_tools"
author: "F. Waldner"
date: "July 12, 2017"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r global_options, include = FALSE}
library(knitr)
options(width = 120)
opts_chunk$set(fig.width = 12, fig.height = 8, fig.path = 'Figs/',
               include = TRUE, warning = FALSE, message = FALSE)
```

## Viz

## locateGlobe

```{r locateGlobe, echo=FALSE}
source(C:/Users/waldner/Documents/git/R_tools/viz/plot_world_sphere.R)

```

```{r locateGlobePlot}
df.point <- data.frame(x=30,y=30)
p.lg <- locateGlobe(df.point, 40, 30, c =0, size=10, alpha=0.9)
plot(p.lg)
```
