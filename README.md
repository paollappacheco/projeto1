# Mudança no consumo de bebidas alcoólicas durante a pandemia de COVID-19

Apresentação em slides (Quarto + R) com a associação entre a mudança no consumo de bebidas alcoólicas e variáveis qualitativas e quantitativas.

## Estrutura

```
projeto_bebidas_alcoolicas/
├── apresentacao.qmd   # slides (introdução, metodologia, resultados, conclusão)
├── styles.css         # estilo e capa
├── R/funcoes.R        # preparo dos dados e funções das tabelas
├── data/nutricao.xlsx # base de dados (não versionada, ver .gitignore)
└── README.md
```

## Como renderizar

1. Instale o [R](https://cran.r-project.org/), o [RStudio](https://posit.co/download/rstudio-desktop/) e o [Quarto](https://quarto.org/docs/get-started/) (já vem no RStudio recente).
2. Instale os pacotes:
   ```r
   install.packages(c("tidyverse", "readxl", "gtsummary", "gt", "rstatix", "DescTools"))
   ```
3. Coloque `nutricao.xlsx` na pasta `data/`.
4. Abra `apresentacao.qmd` no RStudio e clique em **Render**, ou no terminal:
   ```
   quarto render apresentacao.qmd
   ```
5. O resultado é `apresentacao.html` (arquivo único, abre no navegador; tecla **F** = tela cheia).

## Publicar no GitHub

```
git init
git add .
git commit -m "Projeto: consumo de bebidas alcoólicas na pandemia"
git branch -M main
git remote add origin https://github.com/SEU-USUARIO/SEU-REPOSITORIO.git
git push -u origin main
```
