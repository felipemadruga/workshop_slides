

20 + log(2)

20/5

3*3

# 3*3

filmes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv")

# install.packages("readr")

library(readr)


filmes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv")

filmes

view(filmes)

# install.packages("dplyr")

library(dplyr)

filmes |> 
  select(movie, genre)

filmes |> 
  select(-movie, -genre)

filmes |> 
  select(-1)

filmes |> 
  rename(data_de_lancamento = release_date)

filmes |> 
  select(filme = movie, arrecadacao_mundial = 6)

filmes |> 
  arrange(worldwide_gross)

filmes |> 
  arrange(desc(worldwide_gross))

filmes |> 
  slice(1:10)

filmes |> 
  slice_max(
    # indicando a variável que a função deve classificar
    order_by = worldwide_gross, 
    # o numero de linhas que ela deve retornar
    n = 5)


filmes |> 
  filter(worldwide_gross > 500000000)

filmes |> 
  filter(mpaa_rating == "PG" & genre == "Comedy") |> 
  select(movie, mpaa_rating, production_budget)

filmes |> 
  filter(production_budget >= 5000000 & production_budget <= 10000000)

filmes |> 
  filter(
    between(production_budget, 5000000, 10000000)
  )

filmes |> 
  filter(distributor == "MGM" | distributor == "Lionsgate" | distributor == "Universal")

filmes |> 
  filter(distributor %in% c("MGM", "Lionsgate", "Universal") )

filmes |> 
  count(genre)

filmes |> 
  count(mpaa_rating, sort = T)

filmes |> 
  filter(is.na(mpaa_rating))

filmes |> 
  filter(!is.na(mpaa_rating))

filmes |> 
  drop_na(mpaa_rating)

filmes |> 
  drop_na()

filmes |> 
  distinct(genre)

filmes |> 
  summarise(
    arrecadacao = sum(worldwide_gross)
  )

filmes |> 
  summarise(
    r_total_domestica = sum(domestic_gross, na.rm = TRUE)
  )


filmes |> 
  group_by(genre) |> 
  summarise(
    receita = sum(worldwide_gross)
  )

maiores_bilheterias <- 
  filmes |> 
  slice_max(domestic_gross, n = 10) |> 
  select(movie, domestic_gross, worldwide_gross, production_budget)


maiores_bilheterias |> 
  mutate(
    # construindo nossa nova variável receita_total
    receita_total = domestic_gross + worldwide_gross
  )


maiores_bilheterias |> 
  mutate(
    # construindo nossa nova variável receita_total
    receita_total = domestic_gross + worldwide_gross, 
    # lucro de cada um dos filmes
    lucro = receita_total - production_budget, 
    # transformando os valores
    lucro_1 = lucro*(1/1000000)
  ) |> 
  select(filme = movie, lucro, lucro_1) |> 
  arrange(desc(lucro))


filmes |> 
  select(movie, genre) |> 
  mutate(
    novo_genero = if_else(
      # condição lógica a ser analisada
      genre == "Comedy",
      # valor se verdadeiro
      "Comédia", 
      # valor se falso
      "Demais generos"
    )
  )


filmes |> 
  count(distributor, sort = T) |> 
  mutate(
    classificacao = case_when(
      n >= 200 ~ "Top 200+", 
      n >= 100 ~ "Top 100+", 
      n >= 50 ~ "Top 50+", 
      .default = "Demais distribuidoras"
    )
  ) 


#dados <- read_csv("C:\\Users\\Felipe\\Documents\\meus_dados\\movie_profit.csv")

#dados_1 <- read_csv("C:/Users/Felipe/Documents/meus_dados/movie_profit.csv")


library(readxl) # carregando nosso pacote
# Segunda forma
dados_1 <- read_xlsx("C:/Users/Felipe/Documents/meus_dados/movie_profit.xlsx")


meus_dados <- tibble(
  nome = c("João", "Maria", "Carlos"), 
  nota = c(0.5, 4, 8)
)
