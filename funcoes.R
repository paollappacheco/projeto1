# ============================================================
# Preparo dos dados + funções das tabelas (qualitativa e quantitativa)
# Este arquivo é carregado pelo apresentacao.qmd com source("R/funcoes.R")
# ============================================================
suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(gtsummary)
  library(gt); library(rstatix); library(DescTools)
})

alfa      <- 0.05
limite    <- 1.96
var_by    <- "Bebidas_Alcoolicas"
cor_baixo <- "#D00000"   # vermelho: abaixo do esperado
cor_alto  <- "#0057B8"   # azul: acima do esperado

fmt_p <- function(p) ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p)))
sub_verde <- function(txt) {
  paste0("<span style='text-decoration: underline; text-decoration-color: #28A745;",
         " text-decoration-thickness: 2px; text-underline-offset: 3px;'>", txt, "</span>")
}

# ---------------- Dados ----------------
base <- read_xlsx("C:/Users/paoll/OneDrive/Desktop/Projeto1_v2/projeto1_v2/nutricao.xlsx") |>
  select(-c(Tempo_Preparo_Refeicao, Refeicoes_Diarias, Delivery,
            Planejamento_Cardapio, Estoque_Alimentos, Paes_Torradas_Biscoitos,
            Batata_Macarrao_Arroz, Frutas, Legumes_Verduras, Ovos_Carnes,
            Leites_Derivados, Doces, Refrigerante, Produtos_Industrializados,
            Embutidos, Consumo_água, Tempo_Preparo_Pandemia,
            atividade_fisica_pandemia)) |>
  mutate(
    Raca_cor = case_when(
      Raca_cor %in% c("Amarela", "Indígena", "Prefiro não declarar", "Outro", "Outros",
                      "Outros/Não Informado", "Outras/Não Informado") ~ "Outras/Não Informado",
      TRUE ~ Raca_cor),
    Escolaridade = case_when(
      Escolaridade %in% c("Analfabeto", "Ensino Fundamental Completo",
                          "Não desejo informar") ~ "Fundamental ou menos/NI",
      TRUE ~ Escolaridade),
    Genero = case_when(
      Genero %in% c("Outro", "Prefiro não responder") ~ "Outro/Não Informado",
      TRUE ~ Genero)
  )

vars_quanti  <- c("Idade", "Altura", "Peso", "IMC")
vars_sim_nao <- c("Isolamento", "Trabalha", "Covid", "Consulta_Nutricionista",
                  "Dificuldade_Financeira", "cigarro", "Ansiedade",
                  "Depressao", "Insonia", "Enxaqueca")

dados_quali <- base |>
  select(-all_of(vars_quanti)) |>
  mutate(
    Raca_cor = factor(Raca_cor, levels = c("Branca", "Parda", "Preta", "Outras/Não Informado")),
    Escolaridade = factor(Escolaridade, levels = c("Fundamental ou menos/NI", "Ensino Médio Completo",
                                                   "Ensino Superior Completo", "Pós-graduação")),
    across(all_of(vars_sim_nao), ~ factor(.x, levels = c("Sim", "Não")))
  )

base_1 <- base |> select(all_of(c(vars_quanti, var_by)))
niveis <- sort(unique(base_1[[var_by]]))

# ---------------- Tabela qualitativa ----------------
rotulos <- list(
  Genero = "Gênero<sup>F</sup>", Raca_cor = "Raça/cor<sup>Q</sup>",
  Regiao = "Região de residência<sup>Q</sup>", Isolamento = "Isolamento social<sup>Q</sup>",
  Trabalha = "Situação de trabalho<sup>Q</sup>", Renda_familiar = "Faixa de renda familiar<sup>Q</sup>",
  Escolaridade = "Nível de escolaridade<sup>F</sup>", Covid = "Diagnóstico de COVID-19<sup>Q</sup>",
  Consulta_Nutricionista = "Consulta com nutricionista<sup>Q</sup>",
  Dificuldade_Financeira = "Dificuldade financeira<sup>Q</sup>",
  cigarro = "Hábito de fumar cigarro<sup>Q</sup>",
  Ansiedade = "Ansiedade na pandemia<sup>Q</sup>", Depressao = "Depressão na pandemia<sup>Q</sup>",
  Insonia = "Insônia na pandemia<sup>Q</sup>", Enxaqueca = "Enxaqueca na pandemia<sup>Q</sup>"
)

cramer_fun <- function(data, variable, by, ...) {
  tibble::tibble(`**Cramér**` = round(cramer_v(table(data[[variable]], data[[by]])), 3))
}

# Cor/seta só quando p < alfa e |resíduo padronizado| > limite
aplicar_residuos <- function(tbl, dados, by = var_by) {
  nv  <- if (is.factor(dados[[by]])) levels(dados[[by]]) else sort(unique(na.omit(dados[[by]])))
  vars <- unique(tbl$table_body$variable)
  pv <- tbl$table_body |> filter(row_type == "label") |> select(variable, p.value)
  pv <- setNames(pv$p.value, pv$variable)
  res <- lapply(setNames(vars, vars), function(v)
    suppressWarnings(chisq.test(table(dados[[v]], dados[[by]]))$stdres))
  tbl |> modify_table_body(function(x) {
    for (j in seq_along(nv)) {
      col <- paste0("stat_", j)
      x[[col]] <- pmap_chr(list(x$variable, x$label, x$row_type, x[[col]]),
        function(v, l, tipo, txt) {
          if (tipo != "level" || is.na(txt) || !(l %in% rownames(res[[v]]))) return(txt)
          if (is.na(pv[[v]]) || pv[[v]] >= alfa) return(txt)
          z <- res[[v]][l, j]
          if (is.na(z)) return(txt)
          if (z < -limite) paste0("<span style='color:", cor_baixo, ";font-weight:bold'>", txt, " &#9660;</span>")
          else if (z > limite) paste0("<span style='color:", cor_alto, ";font-weight:bold'>", txt, " &#9650;</span>")
          else txt
        })
    }
    x
  })
}

montar_tabela_quali <- function(vars, tamanho = "13px") {
  fv <- intersect(vars, c("Genero", "Escolaridade"))
  testes <- c(list(all_categorical() ~ "chisq.test"),
              lapply(fv, function(v) as.formula(paste0(v, ' ~ "fisher.test"'))))
  args <- if (length(fv) > 0) lapply(fv, function(v)
    as.formula(paste0(v, " ~ list(simulate.p.value = TRUE, B = 10000)"))) else NULL
  set.seed(123)

  tbl_summary(dados_quali, include = all_of(vars), by = all_of(var_by),
              percent = "row", label = rotulos[vars]) |>
    add_p(test = testes, test.args = args, pvalue_fun = label_style_pvalue(digits = 3)) |>
    bold_p(t = alfa) |>
    add_stat(fns = everything() ~ cramer_fun) |>
    modify_spanning_header(all_stat_cols() ~ "**Mudança no consumo de bebidas alcoólicas**") |>
    modify_header(label ~ "**Variáveis**") |>
    bold_labels() |>
    modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") |>
    aplicar_residuos(dados_quali) |>
    modify_footnote(everything() ~ NA) |>
    as_gt() |>
    fmt_markdown(columns = c(label, starts_with("stat_"))) |>
    text_transform(locations = cells_body(columns = p.value), fn = function(x) {
      p <- suppressWarnings(as.numeric(gsub("&lt;|[<>]", "", gsub(",", ".", x))))
      ifelse(!is.na(p) & p < alfa, sub_verde(x), x)
    }) |>
    tab_source_note(md(paste0(
      "F: Fisher; Q: qui-quadrado. Linhas somam 100%. ",
      "<span style='color:", cor_alto, ";font-weight:bold'>&#9650;</span> acima do esperado (resíduo > 1,96); ",
      "<span style='color:", cor_baixo, ";font-weight:bold'>&#9660;</span> abaixo do esperado (resíduo < -1,96). ",
      "Resíduos analisados apenas quando p < 0,05."))) |>
    tab_options(table.font.size = tamanho, column_labels.font.size = tamanho,
                data_row.padding = px(2), table.width = pct(100))
}

# ---------------- Tabela quantitativa ----------------
montar_tabelas_quanti <- function(tamanho = "14px") {

  pressupostos <- map_dfr(vars_quanti, function(v) {
    sh  <- tapply(base_1[[v]], base_1[[var_by]], function(x) shapiro.test(x)$p.value)
    lev <- LeveneTest(as.formula(paste(v, "~", var_by)), data = base_1)$`Pr(>F)`[1]
    bind_cols(tibble(Variável = v),
              as_tibble_row(setNames(fmt_p(sh), paste0("Shapiro: ", names(sh)))),
              tibble(`Levene (p)` = fmt_p(lev),
                     Variâncias = ifelse(lev >= alfa, "Homogêneas", "Heterogêneas")))
  }) |>
    gt() |>
    tab_options(table.font.size = tamanho, column_labels.font.size = tamanho,
                data_row.padding = px(3))

  analisar <- function(v) {
    f <- as.formula(paste(v, "~", var_by))
    lev <- LeveneTest(f, data = base_1)$`Pr(>F)`[1]
    homog <- lev >= alfa
    omni <- oneway.test(f, data = base_1, var.equal = homog)$p.value
    if (homog) {
      ph <- TukeyHSD(aov(f, data = base_1))[[var_by]][, "p adj"]
    } else {
      gh <- games_howell_test(base_1, f)
      ph <- setNames(gh$p.adj, paste(gh$group1, gh$group2, sep = "-"))
    }
    medias <- tapply(base_1[[v]], base_1[[var_by]], mean)
    sig <- sort(ph[ph < alfa])
    texto <- if (omni >= alfa || length(sig) == 0) "Sem diferenças significativas" else
      paste(sapply(seq_along(sig), function(i) {
        g <- strsplit(names(sig)[i], "-", fixed = TRUE)[[1]]
        maior <- g[which.max(medias[g])]; menor <- setdiff(g, maior)
        p_txt <- if (sig[i] < 0.001) paste0("p ", sub_verde("&lt; 0,001")) else
          paste0("p = ", sub_verde(sub("\\.", ",", sprintf("%.3f", sig[i]))))
        paste0(maior, " > ", menor, " (", p_txt, ")")
      }), collapse = "<br>")
    list(levene = fmt_p(lev), teste = ifelse(homog, "ANOVA clássica", "ANOVA de Welch"),
         homog = homog, comp = texto)
  }
  resultados <- setNames(lapply(vars_quanti, analisar), vars_quanti)

  testes_q <- lapply(vars_quanti, function(v) as.formula(paste0(v, ' ~ "oneway.test"')))
  args_q <- lapply(vars_quanti, function(v)
    as.formula(paste0(v, " ~ list(var.equal = ", resultados[[v]]$homog, ")")))
  col_levene <- function(data, variable, by, ...) tibble::tibble(`**Levene**` = resultados[[variable]]$levene)
  col_teste  <- function(data, variable, by, ...) tibble::tibble(`**Teste**` = resultados[[variable]]$teste)
  col_comp   <- function(data, variable, by, ...) tibble::tibble(`**Comparações Múltiplas**` = resultados[[variable]]$comp)
  ultima_stat <- paste0("stat_", length(niveis))

  final <- tbl_summary(base_1, by = all_of(var_by), statistic = all_continuous() ~ "{mean} ({sd})",
                       label = list(Idade ~ "Idade (anos)", Altura ~ "Altura (m)",
                                    Peso ~ "Peso (kg)", IMC ~ "IMC (kg/m²)")) |>
    add_p(test = testes_q, test.args = args_q, pvalue_fun = label_style_pvalue(digits = 3)) |>
    bold_p(t = alfa) |>
    modify_spanning_header(all_stat_cols() ~ "**Consumo de Bebidas Alcoólicas**") |>
    modify_header(label ~ "**Variáveis**") |>
    bold_labels() |>
    modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") |>
    modify_footnote(everything() ~ NA) |>
    add_stat(fns = everything() ~ col_levene, location = everything() ~ "label") |>
    add_stat(fns = everything() ~ col_teste,  location = everything() ~ "label") |>
    add_stat(fns = everything() ~ col_comp,   location = everything() ~ "label") |>
    as_gt() |>
    cols_move(columns = c(`**Levene**`, `**Teste**`), after = all_of(ultima_stat)) |>
    cols_move(columns = `**Comparações Múltiplas**`, after = p.value) |>
    fmt_markdown(columns = c(label, `**Comparações Múltiplas**`)) |>
    text_transform(locations = cells_body(columns = p.value), fn = function(x) {
      p <- suppressWarnings(as.numeric(gsub("&lt;|[<>]", "", gsub(",", ".", x))))
      ifelse(!is.na(p) & p < alfa, sub_verde(x), x)
    }) |>
    tab_source_note(md(paste0(
      "Média (desvio padrão). Levene: p-valor da homogeneidade de variâncias. ",
      "Tukey (ANOVA clássica) ou Games-Howell (Welch). \"A > B\": média de A maior que a de B."))) |>
    tab_options(table.font.size = tamanho, column_labels.font.size = tamanho,
                data_row.padding = px(4), table.width = pct(100))

  list(pressupostos = pressupostos, final = final)
}
