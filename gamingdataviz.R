
games3 <- read_csv('vgchartz-7_7_2020.csv')


data_clean <- games3 %>%
  mutate(total_sales = ifelse(is.na(total_sales), total_shipped, total_sales)) %>%
  filter(!is.na(total_sales))
  

data_summarized <- data_clean %>%
  group_by(title) %>%
  summarize(total_sales = sum(total_sales, na.rm = TRUE)) %>%
  arrange(desc(total_sales)) %>%
  slice(1:20)

data_summarized$title <- factor(data_summarized$title, levels = rev(data_summarized$title))

ggplot(data_summarized, aes(x = total_sales, y = title)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(expand = c(0.01,0.02), breaks = seq(0, 400, by = 25)) +
  labs(x = "Global Satış Adedi (Milyon)", y = "Oyun Serileri", title = "Tarihin En Çok Satan 20 Video Oyunu Serisi") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.4),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20, hjust = 0.4),
  )







games2 <- read_csv('game_info.csv')

top20games <- games2 %>%
  filter(!is.na(metacritic)) %>%
  filter(slug != "soulcalibur-1998", slug != "half-life-2-update") %>%
  arrange(desc(metacritic)) %>%
  slice(1:20)


ggplot(top20games, aes(x=reorder(top20games$name, top20games$metacritic), y=top20games$metacritic)) +
  geom_segment( aes(x=top20games$name, xend=top20games$name, y=94, yend=top20games$metacritic), color="orange") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  labs(title = "Metacritic Puanına Göre Tarihin En İyi 20 Oyunu",
       x = "Oyunların İsimleri",
       y = "Oyunların Puanları") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.2),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20, hjust = 0.35)
    )





install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
library(tidytuesdayR)
install.packages("glue")
library(glue)


tuesdata <- tidytuesdayR::tt_load('2021-03-16')
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

games <- tuesdata$games

games <- games %>%
  mutate(month_num = match(month, month.name),
         end_month = ceiling_date(ymd(glue("{year}-{month_num}-18")), "month") - days(1))


games_df <- games %>%
  group_by(gamename) %>%
  arrange(end_month) %>%
  mutate(timeline = row_number()) %>%
  ungroup()


palette <- c("PLAYERUNKNOWN'S BATTLEGROUNDS" = "#56B4E9", "Team Fortress 2" = "#009E73", "Counter-Strike: Global Offensive" = "#F0E442", "Dota 2" = "#0072B2", "Grand Theft Auto V" = "#D55E00")

select_games <- games_df %>%
  filter(gamename %in% names(palette))


ggplot() +
  geom_line(data = select_games, aes(x = timeline, y = avg, group = gamename, color = gamename), size = 0.8) +
  scale_y_continuous(limits = c(-5000, 1700000), breaks = seq(0, 1750000, by = 200000)) +
  scale_x_continuous(expand = c(0.01,0.02), breaks = seq(0, 100, by = 10)) +
  labs(x = "Çıkışının Üzerinden Geçen Ay Sayısı", y = "Ortalama Eşzamanlı Aktif Oyuncu Sayısı", title = "Seçili Oyunların Ortalama Oyuncu Değişimleri") +
  scale_color_manual(values = palette) +
  guides(color = guide_legend(title = "Oyunlar")) +
  theme_minimal() +
  theme(
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.6),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20, hjust = 0.6),
        axis.title.y = element_text(angle = 90),
        legend.position = "right",
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 12))





game_info <- read_csv("game_info.csv")

top_publishers <- video_games %>%
  count(publishers, sort = TRUE) %>%
  filter(!is.na(publishers)) %>%
  filter(publishers != "Big Fish Games") %>%
  top_n(5, n) %>%
  pull(publishers)


filtered_data <- game_info %>%
  filter(publishers %in% top_publishers)
  
publisher_genre_counts <- filtered_data %>%
  filter(!is.na(genres)) %>%
  count(publishers, genres, sort = TRUE)

top_genres_by_publisher <- publisher_genre_counts %>%
  group_by(publishers) %>%
  top_n(5, n)

plot_data <- top_genres_by_publisher %>%
  ungroup() %>%
  mutate(genres = fct_reorder(genres, n))

cbPalette2 <- c("Electronic Arts" = "#56B4E9", "Konami" = "#009E73", "Nintendo" = "#F0E442", "SEGA" = "#0072B2", "Ubisoft Entertainment" = "#D55E00")

ggplot(plot_data, aes(x = genres, y = n, fill = publishers)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(-10, 200), breaks = seq(0, 200, by = 20)) +
  labs(title = "En Fazla Oyun Yayınlayan 5 Yayıncının En Çok Yayınladığı 5 Oyun Türü",
       x = "Oyun Türü",
       y = "Oyun Sayısı") +
  scale_fill_manual(values = cbPalette2) +
  guides(fill = guide_legend(title = "Yayıncılar")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.4),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 12))
