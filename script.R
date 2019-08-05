library(tidyverse)
library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = "## Your client ID ##")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "## Your client secret ##")

access_token <- spotifyr::get_spotify_access_token()

mydata <- spotifyr::get_artist_audio_features("Death Grips", album_types = c("album", "single"), return_closest_artist = TRUE, parallelize = TRUE) %>%
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DXbYM3nMM0oPk")) %>% # pop
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZEVXbMDoHDwVN2tF")) %>% # top 50
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX0XUsuxWHRQd")) %>% # hip hop
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX186v583rmzp")) %>% # 90's hip hop
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX4dyzvuaRJ0n")) %>% # electronic
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DXdbXrPNafg9d")) %>% # indie
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DWVmps5U8gHNv")) %>% # essential folk
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DXbITWG1ZJKYt")) %>% # jazz classics
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX1MUPbVKMgJE")) %>% # disco
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DXbSbnqxMTGx9")) %>% # reggae
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX14fiWYoe7Oh")) %>% # k-pop
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DWTwzVdyRpXm1")) %>% # desi
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DXcIme26eJxid")) %>% # afropop
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DWWEJlAGA9gs0")) %>% # classical
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX1lVhptIYRda")) %>% # country
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DWTcqUzwhNmKv")) %>% # metal
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX2LTcinqsO68")) %>% # old school metal
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DXasneILDRM7B")) %>% # pop punk
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DX3LDIBRoaCDQ")) %>% # classic punk
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DWXRqgorJj26U")) %>% # classic rock
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "37i9dQZF1DZ06evO4eBuao")) %>% # swans
  bind_rows(spotifyr::get_playlist_audio_features("spotify", "2Zcx8ObWlAPiZLZffNB4os")) %>% # jpegmafia
  drop_na(danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  distinct(artist_name, track_name, .keep_all = TRUE) %>%
  filter(!c(artist_name == "Death Grips" & album_name %in% c("Black Paint",
                                                             "Flies", 
                                                             "Inanimate Sensation", 
                                                             "Shitshow", 
                                                             "Streaky", 
                                                             "Hahaha")))


death_grips <- mydata %>%
  select(artist_name, track_name, album_name, playlist_name, danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  mutate_if(is.numeric, ~as.vector(scale(.))) %>%
  filter(artist_name == "Death Grips")

pop_data <- mydata %>%
  select(artist_name, track_name, album_name, playlist_name, danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  mutate_if(is.numeric, ~as.vector(scale(.))) %>%
  filter(playlist_name == "Global Top 50")

rap <- mydata %>%
  select(artist_name, track_name, album_name, playlist_name, danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  mutate_if(is.numeric, ~as.vector(scale(.))) %>%
  filter(playlist_name == "RapCaviar")

metal <- mydata %>%
  select(artist_name, track_name, album_name, playlist_name, danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  mutate_if(is.numeric, ~as.vector(scale(.))) %>%
  filter(playlist_name == "Kickass Metal")

electronic <- mydata %>%
  select(artist_name, track_name, album_name, playlist_name, danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  mutate_if(is.numeric, ~as.vector(scale(.))) %>%
  filter(playlist_name == "mint")

rock <- mydata %>%
  select(artist_name, track_name, album_name, playlist_name, danceability, energy, valence, tempo, loudness, instrumentalness, speechiness, acousticness) %>%
  mutate_if(is.numeric, ~as.vector(scale(.))) %>%
  filter(playlist_name == "Rock Classics")

accessibility_data <- death_grips %>%

  mutate(dist_grips = (danceability - mean(death_grips$danceability))^2 +
           (energy - mean(death_grips$energy))^2 +
           (valence - mean(death_grips$valence))^2 +
           (tempo - mean(death_grips$tempo))^2 +
           (loudness - mean(death_grips$loudness))^2 +
           (instrumentalness - mean(death_grips$instrumentalness))^2 +
           (speechiness - mean(death_grips$speechiness))^2 +
           (acousticness - mean(death_grips$acousticness))^2) %>%
           
  mutate(dist_grips = sqrt(dist_grips)) %>%
  
  mutate(dist_pop = (danceability - mean(pop_data$danceability))^2 +
           (energy - mean(pop_data$energy))^2 +
           (valence - mean(pop_data$valence))^2 +
           (tempo - mean(pop_data$tempo))^2 +
           (loudness - mean(pop_data$loudness))^2 +
           (instrumentalness - mean(pop_data$instrumentalness))^2 +
           (speechiness - mean(pop_data$speechiness))^2 +
           (acousticness - mean(pop_data$acousticness))^2) %>%
           
  mutate(dist_pop = sqrt(dist_pop)) %>%

  mutate(dist_rap = (danceability - mean(rap$danceability))^2 +
           (energy - mean(rap$energy))^2 +
           (valence - mean(rap$valence))^2 +
           (tempo - mean(rap$tempo))^2 +
           (loudness - mean(rap$loudness))^2 +
           (instrumentalness - mean(rap$instrumentalness))^2 +
           (speechiness - mean(rap$speechiness))^2 +
           (acousticness - mean(rap$acousticness))^2) %>%
           
  mutate(dist_rap = sqrt(dist_rap)) %>%
  
    mutate(dist_metal = (danceability - mean(metal$danceability))^2 +
           (energy - mean(metal$energy))^2 +
           (valence - mean(metal$valence))^2 +
           (tempo - mean(metal$tempo))^2 +
           (loudness - mean(metal$loudness))^2 +
           (instrumentalness - mean(metal$instrumentalness))^2 +
           (speechiness - mean(metal$speechiness))^2 +
           (acousticness - mean(metal$acousticness))^2) %>%
           
  mutate(dist_metal = sqrt(dist_metal)) %>%
  
    mutate(dist_electronic = (danceability - mean(electronic$danceability))^2 +
           (energy - mean(electronic$energy))^2 +
           (valence - mean(electronic$valence))^2 +
           (tempo - mean(electronic$tempo))^2 +
           (loudness - mean(electronic$loudness))^2 +
           (instrumentalness - mean(electronic$instrumentalness))^2 +
           (speechiness - mean(electronic$speechiness))^2 +
           (acousticness - mean(electronic$acousticness))^2) %>%
           
  mutate(dist_electronic = sqrt(dist_electronic)) %>%
  
  mutate(dist_rock = (danceability - mean(rock$danceability))^2 +
           (energy - mean(rock$energy))^2 +
           (valence - mean(rock$valence))^2 +
           (tempo - mean(rock$tempo))^2 +
           (loudness - mean(rock$loudness))^2 +
           (instrumentalness - mean(rock$instrumentalness))^2 +
           (speechiness - mean(rock$speechiness))^2 +
           (acousticness - mean(rock$acousticness))^2) %>%
           
  mutate(dist_rock = sqrt(dist_rock)) %>%
    
  mutate(pop_accessibility = (dist_grips + dist_pop)/2) %>%
  mutate(dist_pop = scales::rescale(dist_pop, to = c(100, 0))) %>%
  mutate(pop_accessibility = scales::rescale(pop_accessibility, to = c(100, 0))) %>%

  mutate(rap_accessibility = (dist_grips + dist_rap)/2) %>%
  mutate(dist_rap = scales::rescale(dist_rap, to = c(100, 0))) %>%
  mutate(rap_accessibility = scales::rescale(rap_accessibility, to = c(100, 0))) %>%
  
  mutate(metal_accessibility = (dist_grips + dist_metal)/2) %>%
  mutate(dist_metal = scales::rescale(dist_metal, to = c(100, 0))) %>%
  mutate(metal_accessibility = scales::rescale(metal_accessibility, to = c(100, 0))) %>%

  mutate(electronic_accessibility = (dist_grips + dist_electronic)/2) %>%
  mutate(dist_electronic = scales::rescale(dist_electronic, to = c(100, 0))) %>%
  mutate(electronic_accessibility = (dist_grips + dist_electronic)/2) %>%

  mutate(rock_accessibility = (dist_grips + dist_rock)/2) %>%
  mutate(dist_rock = scales::rescale(dist_rock, to = c(100, 0))) %>%
  mutate(rock_accessibility = scales::rescale(rock_accessibility, to = c(100, 0))) %>%

  mutate(dist_grips = scales::rescale(dist_grips, to = c(100, 0)))
  
  

theme_awesome <- function(font) {
  
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0, face = "bold", family = font, size = 55, margin = margin(1, 0, 20, 0)),
          axis.title = element_text(face = "bold", family = font, size = 30),
          axis.text = element_text(family = font, face = "bold", size = 28),
          axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
          plot.margin = margin(25, 15, 25, 15),
          legend.title = element_text(family = font, face = "bold", size = 18),
          legend.title.align = 0,
          legend.text = element_text(family = font, face = "bold", size = 15, margin = margin(1, 1, 1, 100)),
          legend.key.size = unit(1, "cm"),
          legend.margin = margin(0, 30, 30, 20),
          #axis.line = element_blank(),
          axis.ticks.length = unit(0.7, "cm"),
          panel.grid.major.y = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.text = element_text(family = font, face = "bold", size = 18, margin = margin(0, 0, 10, 0)),
          strip.background = element_rect(fill = "white", color = "black"),
          plot.caption = element_text(size = 20, face = "bold", family = font, hjust = 0.95),
          plot.subtitle = element_text(size = 30, face = "bold", family = font, margin = margin(0, 0, 30, 0)))
  
}


accessibility_data %>% 
  mutate(track_name = str_trunc(track_name, 20)) %>%
  arrange(desc(pop_accessibility)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(track_name, pop_accessibility), y = pop_accessibility)) + 
  geom_col(show.legend = FALSE, fill = "#900C3F") +
  coord_flip() + 
  theme_awesome("Tw Cen MT Condensed") + 
  labs(title = "Which Death Grips Songs are the Most Accessible?", 
       subtitle = 'Based on the "Global Top 50" (Mainstream Pop) Spotify Playlist', 
       y = "Accessibility", 
       x = "", 
       caption = "@OppenheimerEvan")
       
ggsave("accessibility_pop.png", dpi = 450, height = (2.72 * 4), width = (4.17 * 5))

accessibility_data %>% 
  mutate(track_name = str_trunc(track_name, 20)) %>%
  arrange(desc(rap_accessibility)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(track_name, rap_accessibility), y = rap_accessibility)) + 
  geom_col(show.legend = FALSE, fill = "#900C3F") +
  coord_flip() + 
  theme_awesome("Tw Cen MT Condensed") + 
  labs(title = "Which Death Grips Songs are the Most Accessible?", 
       subtitle = 'Based on the "RapCaviar" (Mainstream Hip Hop) Spotify Playlist', 
       y = "Accessibility", 
       x = "", 
       caption = "@OppenheimerEvan")
  
ggsave("accessibility_rap.png", dpi = 450, height = (2.72 * 4), width = (4.17 * 5))

accessibility_data %>% 
  mutate(track_name = str_trunc(track_name, 20)) %>%
  arrange(desc(metal_accessibility)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(track_name, metal_accessibility), y = metal_accessibility)) + 
  geom_col(show.legend = FALSE, fill = "#900C3F") +
  coord_flip() + 
  theme_awesome("Tw Cen MT Condensed") + 
  labs(title = "Which Death Grips Songs are the Most Accessible?", 
       subtitle = 'Based on the "Kickass Metal" (Contemporary Metal) Spotify Playlist', 
       y = "Accessibility", 
       x = "", 
       caption = "@OppenheimerEvan")
       
ggsave("accessibility_metal.png", dpi = 450, height = (2.72 * 4), width = (4.17 * 5))


accessibility_data %>% 
  mutate(track_name = str_trunc(track_name, 20)) %>%
  arrange(desc(electronic_accessibility)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(track_name, electronic_accessibility), y = electronic_accessibility)) + 
  geom_col(show.legend = FALSE, fill = "#900C3F") +
  coord_flip() + 
  theme_awesome("Tw Cen MT Condensed") + 
  labs(title = "Which Death Grips Songs are the Most Accessible?", 
       subtitle = 'Based on the "mint" (Mainstream Electronic) Spotify Playlist', 
       y = "Accessibility", 
       x = "", 
       caption = "@OppenheimerEvan")

ggsave("accessibility_electronic.png", dpi = 450, height = (2.72 * 4), width = (4.17 * 5))


accessibility_data %>% 
  mutate(track_name = str_trunc(track_name, 20)) %>%
  arrange(desc(rock_accessibility)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(track_name, rock_accessibility), y = rock_accessibility)) + 
  geom_col(show.legend = FALSE, fill = "#900C3F") +
  coord_flip() + 
  theme_awesome("Tw Cen MT Condensed") + 
  labs(title = "Which Death Grips Songs are the Most Accessible?", 
       subtitle = 'Based on the "Rock Classics" (Classic Rock) Spotify Playlist', 
       y = "Accessibility", 
       x = "", 
       caption = "@OppenheimerEvan")
       
ggsave("accessibility_rock.png", dpi = 450, height = (2.72 * 4), width = (4.17 * 5))

death_grips_albums <- c("The Money Store", 
                        "No Love Deep Web",
                        "Government Plates", 
                        "The Powers That B",
                        "Bottomless Pit",
                        "Year Of The Snitch")

accessibility_data %>%
  filter(album_name %in% death_grips_albums) %>%
  ggplot(aes(y = pop_accessibility, x = ordered(album_name, levels = death_grips_albums))) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 4) + 
  guides(color = FALSE) +
  labs(title = 'Which Death Grips Albums Are the Most Accessible?', 
       subtitle = 'Based on the "Global Top 50" (Mainstream Pop) Spotify Playlist',
       y = "Accessibility",
       x = "",
       caption = "@OppenheimerEvan") +
  theme_minimal(base_family = "Tw Cen MT Condensed") +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 50, margin = margin(1, 0, 20, 0)),
        axis.title = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "bold", size = 27),
        axis.text.x = element_text(face = "bold", size = 23),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 15, 25, 15),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(size = 19, face = "bold", hjust = 0.95),
        plot.subtitle = element_text(size = 32, face = "bold", margin = margin(0, 0, 30, 0)))

ggsave("accessibility_albums_pop.png", dpi = 320, height = (2.72 * 4), width = (4.17 * 4))

accessibility_data %>%
  filter(album_name %in% death_grips_albums) %>%
  ggplot(aes(y = rap_accessibility, x = ordered(album_name, levels = death_grips_albums))) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 4) + 
  guides(color = FALSE) +
  labs(title = 'Which Death Grips Albums Are the Most Accessible?', 
       subtitle = 'Based on the "RapCaviar" (Mainstream Hip Hop) Spotify Playlist',
       y = "Accessibility",
       x = "",
       caption = "@OppenheimerEvan") +
  theme_minimal(base_family = "Tw Cen MT Condensed") +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 50, margin = margin(1, 0, 20, 0)),
        axis.title = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "bold", size = 27),
        axis.text.x = element_text(face = "bold", size = 23),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 15, 25, 15),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(size = 19, face = "bold", hjust = 0.95),
        plot.subtitle = element_text(size = 32, face = "bold", margin = margin(0, 0, 30, 0)))

ggsave("accessibility_albums_rap.png", dpi = 320, height = (2.72 * 4), width = (4.17 * 4))

accessibility_data %>%
  filter(album_name %in% death_grips_albums) %>%
  ggplot(aes(y = metal_accessibility, x = ordered(album_name, levels = death_grips_albums))) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 4) + 
  guides(color = FALSE) +
  labs(title = 'Which Death Grips Albums Are the Most Accessible?', 
       subtitle = 'Based on the "Kickass Metal" (Contemporary Metal) Spotify Playlist',
       y = "Accessibility",
       x = "",
       caption = "@OppenheimerEvan") +
  theme_minimal(base_family = "Tw Cen MT Condensed") +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 50, margin = margin(1, 0, 20, 0)),
        axis.title = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "bold", size = 27),
        axis.text.x = element_text(face = "bold", size = 23),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 15, 25, 15),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(size = 19, face = "bold", hjust = 0.95),
        plot.subtitle = element_text(size = 32, face = "bold", margin = margin(0, 0, 30, 0)))

ggsave("accessibility_albums_metal.png", dpi = 320, height = (2.72 * 4), width = (4.17 * 4))

accessibility_data %>%
  filter(album_name %in% death_grips_albums) %>%
  ggplot(aes(y = electronic_accessibility, x = ordered(album_name, levels = death_grips_albums))) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 4) + 
  guides(color = FALSE) +
  labs(title = 'Which Death Grips Albums Are the Most Accessible?', 
       subtitle = 'Based on the "mint" (Mainstream Electronic) Spotify Playlist',
       y = "Accessibility",
       x = "",
       caption = "@OppenheimerEvan") +
  theme_minimal(base_family = "Tw Cen MT Condensed") +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 50, margin = margin(1, 0, 20, 0)),
        axis.title = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "bold", size = 27),
        axis.text.x = element_text(face = "bold", size = 23),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 15, 25, 15),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(size = 19, face = "bold", hjust = 0.95),
        plot.subtitle = element_text(size = 32, face = "bold", margin = margin(0, 0, 30, 0)))

ggsave("accessibility_albums_electronic.png", dpi = 320, height = (2.72 * 4), width = (4.17 * 4))

accessibility_data %>%
  filter(album_name %in% death_grips_albums) %>%
  ggplot(aes(y = rock_accessibility, x = ordered(album_name, levels = death_grips_albums))) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 4) + 
  guides(color = FALSE) +
  labs(title = 'Which Death Grips Albums Are the Most Accessible?', 
       subtitle = 'Based on the "Rock Classics" (Classic Rock) Spotify Playlist',
       y = "Accessibility",
       x = "",
       caption = "@OppenheimerEvan") +
  theme_minimal(base_family = "Tw Cen MT Condensed") +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 50, margin = margin(1, 0, 20, 0)),
        axis.title = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "bold", size = 27),
        axis.text.x = element_text(face = "bold", size = 23),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 15, 25, 15),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(size = 19, face = "bold", hjust = 0.95),
        plot.subtitle = element_text(size = 32, face = "bold", margin = margin(0, 0, 30, 0)))


ggsave("accessibility_albums_rock.png", dpi = 320, height = (2.72 * 4), width = (4.17 * 4))
