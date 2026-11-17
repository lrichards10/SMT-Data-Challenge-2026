
#Installs the necessary packages and set seed
library(tidyverse)
library(rsample)
library(gifski)
library(gt)
library(gtExtras)
set.seed(10)

#Remember to set working directory to current file location

# ------Read in Starter Data--------------------

source("SMT_Data_Starter.R")

#Displays numbers as regular numbers
#Not scientific notation
options(scipen = 999)

#------Import active bindings to data frames and clean the data-----------------
##-------------Ball Events------------------------------------------------------
ball_events_raw <- ball_events %>% collect()

ball_events_df <- ball_events_raw %>%
  #Remove data with missing game string, play number, and event timestamp
  filter(!is.na(game_string) & !is.na(play_per_game) & !is.na(timestamp)) %>%
  #Remove duplicated ball events during a play
  group_by(game_string, play_per_game) %>%
    #Removes plays that don't have a play ending
    filter(any(player_id == 0)) %>%
    #Ensures all ball events are in order in case of duplicates with different timestamps
    arrange(timestamp, .by_group = TRUE) %>%
    mutate(
     #Adds a label in case a ppg has 2 plays occuring in it
     ppg_section = if_else(row_number() <= first(which(player_id == 0)),"a","b"),
     game_string_play = paste(game_string, play_per_game, ppg_section, sep = "_")
    ) %>%
  ungroup() %>%
  #Regroup based on newly created game string
  group_by(game_string_play)%>%
    #Remove single line plays
    filter(n() > 1) %>%
  ungroup() %>%
  relocate(ppg_section, .after = play_per_game)
  
#These two dataframes show proof that duplicated plays or plays containing
#Two plays in them are now all listed as one play
ball_events_2_plays_in_1_raw <- ball_events_raw %>%
  group_by(game_string, play_per_game) %>%
    filter(player_id == 0 & ball_eventcode == 5) %>%
    summarise(num_plays_in_ppg = n()) %>%
  ungroup() %>%
  filter(num_plays_in_ppg > 1)

ball_events_2_plays_in_1_df <- ball_events_df %>%
  group_by(game_string_play) %>%
    filter(player_id == 0 & ball_eventcode == 5) %>%
    summarise(num_plays_in_ppg = n()) %>%
  ungroup() %>%
  filter(num_plays_in_ppg > 1)

##------------------------Ball Positions----------------------------------------
ball_positions_raw <- ball_positions %>% collect()

ball_positions_df <- ball_positions_raw %>%
  #Filters out rows that list the header row from multiple datasets in the 
  #dataframe
  filter(game_string != "game_string") %>%
  #Creates unique play identifier
  #stores variables as proper type
  mutate(
    game_string_play = paste(game_string, play_per_game, sep = "_"),
    play_per_game = as.integer(play_per_game),
    timestamp = as.integer(timestamp),
    ball_position_x = as.numeric(ball_position_x),
    ball_position_y = as.numeric(ball_position_y),
    ball_position_z = as.numeric(ball_position_z)
  )


##--------------------------Player Positions------------------------------------
player_positions_raw <- player_positions %>% collect()

player_positions_df <- player_positions %>%
  #Filters out rows that list the header row from multiple datasets in the 
  #dataframe
  filter(game_string != "game_string") %>%
  #Creates unique play identifier
  #stores variables as proper type
  mutate(
    game_string_play = paste(game_string, play_per_game, sep = "_"),
    play_per_game = as.integer(play_per_game),
    player_id = as.integer(player_id),
    timestamp = as.integer(timestamp),
    field_x = as.numeric(field_x),
    field_y = as.numeric(field_y)
  ) %>%
  collect()

##------------------------Lineups-----------------------------------------------
lineups_df <- lineups %>%
  #Removes duplicate plays from the dataset
  distinct() %>%  
  group_by(game_string, play_per_game) %>%
    mutate(
      #Adds a label in case a ppg has 2 plays occuring in it
      ppg_section = if_else(row_number() == 1, "a","b"),
      #Creates unique play identifier
      game_string_play = paste(game_string, play_per_game, ppg_section, sep = "_")
    ) %>%
  ungroup() %>%
  relocate(ppg_section, .after = play_per_game)
  






#-----------------------Play Types-------------------------------------------

#Creates a dataset that lists what type of play occurs for each play in the 
#available data
play_description <- ball_events_df %>%
  #Adds the inning, half inning, batter, and runners to each play
  #These variables will assist in defining what play occurred
  left_join(.,
            lineups_df %>% 
              select(game_string_play, inning, half_inning, batter, on_1b, on_2b, on_3b),
            by = join_by(game_string_play)
  ) %>%
  #Goes play by play to list which play occured in each
  group_by(game_string_play) %>%
    mutate(
      row_id = row_number(),
      #Defines exact row by row combinations that define a certain play by ball
      #event codes
      play_type = case_when(
##----------------------Pitched Balls, not hit----------------------------------
        #Pitch
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode %in% c(2,7)) &
          lead(row_id == 3, n=2) & lead(player_id == 0, n=2) & lead(ball_eventcode == 5, n=2)
          ~ "Pitch",
        #Pitch, but end of play is listed before catcher's ball acquisition
        #Timestamp is same, but different listing order
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 0) & lead(ball_eventcode == 5) &
          lead(row_id == 3, n=2) & lead(player_id == 2, n=2) & lead(ball_eventcode %in% c(2,7), n=2)
          ~ "Pitch-Out of Order",
        #Pitch with Throw Down
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode %in% c(2,7)) &
          lead(row_id == 3, n=2) & lead(player_id == 2, n=2) & lead(ball_eventcode %in% c(3,8), n=2)
          ~ "Pitch-Throw Down",
        #Pitch deflected off catcher but clean, with a throw down after
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode == 9) &
          lead(row_id == 3, n=2) & lead(player_id == 2, n=2) & lead(ball_eventcode %in% c(2,7), n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 2, n=3) & lead(ball_eventcode %in% c(3,8), n=3)
          ~ "Pitch-Deflected-Throw Down",
        #Pitch Deflected, off catcher but, but clean otherwise
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode == 9) &
          #Does not contain bounces
          !any(player_id == 255) &
          lead(row_id == max(row_number()) - 1, n=max(row_number()) - 2) & lead(player_id == 2, n=max(row_number()) - 2) & lead(ball_eventcode %in% c(2,7), n=max(row_number()) - 2) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Deflected",
        #Pitch Deflected, play ends without catchers fielding the ball
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode == 9) &
          #Does not contain bounces
          !any(player_id == 255) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Deflected-End",
        #Pitch Deflected and dropped with a throw down after
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode == 9) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) &
          any(player_id == 2 & ball_eventcode %in% c(2,7) &
                lead(player_id == 2) & lead(ball_eventcode %in% c(3,8)))
          ~ "Pitch-Deflected-Bounced-Throw Down",
        #Pitch Deflected and Bounces, with any number of bounces
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode == 9) &
          #Ballcode not specified to allow more possibilities
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) &
          lead(row_id == max(row_number()) - 1, n=max(row_number()) - 2) & lead(player_id == 2, n=max(row_number()) - 2) & lead(ball_eventcode %in% c(2,7), n=max(row_number()) - 2) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Deflected-Bounced",
        #Pitch deflected and bounces and play ends without catcher fielding it
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 2) & lead(ball_eventcode == 9) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 0, n=3) & lead(ball_eventcode == 5, n=3)
          ~ "Pitch-Deflected-Bounced-End",
          
        #Pitch Bounced but other events occur and then a throw down
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) &
          #Does not contain a catcher deflection
          !any(player_id == 2 & ball_eventcode == 9) &
          #Has a throw down at some point in the play
          any(player_id == 2 & ball_eventcode %in% c(2,7) &
                lead(player_id == 2) & lead(ball_eventcode %in% c(3,8)))
          ~ "Pitch-Bounced-Throw Down",
        #Pitch Bounced and blocked by the catcher. Nothing else happens after
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) &
          #Does not contain a catcher deflection
          !any(player_id == 2 & ball_eventcode == 9) &
          lead(row_id == max(row_number()) - 1, n=max(row_number()) - 2) & lead(player_id == 2, n=max(row_number()) - 2) & lead(ball_eventcode %in% c(2,7), n=max(row_number()) - 2) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Bounced",
        #Pitch Bounced and not fielded by the catcher
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) &
          #Does not contain a catcher deflection
          !any(player_id == 2 & ball_eventcode == 9) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Bounced-End",
        #Pitch bounced and deflected, with a throw down after
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) &
          lead(row_id == 3, n=2) & lead(player_id == 2, n=2) & lead(ball_eventcode == 9, n=2) &
          any(player_id == 2 & ball_eventcode %in% c(2,7) &
                lead(player_id == 2) & lead(ball_eventcode %in% c(3,8)))
          ~ "Pitch-Bounced-Deflected-Throw Down",
        #Pitch Bounced, then blocked by the catcher, but not cleanly
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) &
          lead(row_id == 3, n=2) & lead(player_id == 2, n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == max(row_number()) - 1, n=max(row_number()) - 2) & lead(player_id == 2, n=max(row_number()) - 2) & lead(ball_eventcode %in% c(2,7), n=max(row_number()) - 2) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Bounced-Deflected",
        #Pitch Bounced and deflected but not fielded
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) &
          lead(row_id == 3, n=2) & lead(player_id == 2, n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == max(row_number()), n=max(row_number()) - 1) & lead(player_id == 0, n=max(row_number()) - 1) & lead(ball_eventcode == 5, n=max(row_number()) - 1)
          ~ "Pitch-Bounced-Deflected-End",
        
        #Hit By Pitch
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 9)
          ~ "Hit By Pitch",
        #Hit By Pitch, but ball bounced before it hit the batter
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 255) & lead(ball_eventcode == 16) &
          lead(row_id == 3, n=2) & lead(player_id == 10, n=2) & lead(ball_eventcode == 9, n=2)
          ~ "Hit By Pitch-Bounced",
        
##---------------Balls Hit----------------------------------------------------

        #Foul Ball
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 0, n=2) & lead(ball_eventcode == 5, n=2)
          ~ "Foul Ball",
        #Foul Ball off wall
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) & lead(ball_eventcode == 10, n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 0, n=3) & lead(ball_eventcode == 5, n=3)
          ~ "Foul Ball-Off Wall",
        #Home Run
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) & lead(ball_eventcode == 11, n=2)
          ~ "Home Run",
        #Outfield Fly Out
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(7,8,9), n=2) & lead(ball_eventcode %in% c(2,7), n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 0, n=3) & lead(ball_eventcode == 5, n=3)
          ~ "Outfield Fly Out",
        #Outfield Fly Out Complex
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(7,8,9), n=2) & lead(ball_eventcode %in% c(2,7), n=2) &
          lead(row_id == 4, n=3) & lead(player_id != 0, n=3)
        ~ "Outfield Fly Out-Complex",
        #Outfield Fly Out Bobbled
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:9), n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == 4, n=3) & lead(player_id %in% c(7,8,9), n=3) &lead(ball_eventcode %in% c(2,7), n=3) &
          lead(row_id == 5, n=4) & lead(player_id == 0, n=4) & lead(ball_eventcode == 5, n=4)
          ~ "Outfield Fly Out-Bobbled",
        #Outfield Fly Out Bobbled, Complex
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:9), n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == 4, n=3) & lead(player_id %in% c(7,8,9), n=3) &lead(ball_eventcode %in% c(2,7), n=3) &
          lead(row_id == 5, n=4) & lead(player_id != 0, n=4)
          ~ "Outfield Fly Out-Bobbled-Complex",
        #Infield Fly Out
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:6), n=2) & lead(ball_eventcode %in% c(2,7), n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 0, n=3) & lead(ball_eventcode == 5, n=3)
          ~ "Infield Fly Out",
        #Infield Fly Out Complex
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:6), n=2) & lead(ball_eventcode %in% c(2,7), n=2) &
          lead(row_id == 4, n=3) & lead(player_id != 0, n=3)
          ~ "Infield Fly Out-Complex",
        #Infield Fly out Bobbled
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:9), n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == 4, n=3) & lead(player_id %in% c(1:6), n=3) &lead(ball_eventcode %in% c(2,7), n=3) &
          lead(row_id == 5, n=4) & lead(player_id == 0, n=4) & lead(ball_eventcode == 5, n=4)
          ~ "Infield Fly Out-Bobbled",
        #Infield Fly-out Bobbled Complex
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:9), n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == 4, n=3) & lead(player_id %in% c(1:6), n=3) &lead(ball_eventcode %in% c(2,7), n=3) &
          lead(row_id == 5, n=4) & lead(player_id != 0, n=4)
          ~ "Outfield Fly Out-Bobbled-Complex",
        #Live Ball
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) & lead(ball_eventcode == 16, n=2)
          ~ "Live Ball",
        #Live Ball deflected
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id %in% c(1:9), n=2) & lead(ball_eventcode == 9, n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 255, n=3) & lead(ball_eventcode %in% c(10,16), n=3)
          ~ "Live Ball-Deflected",
        #Live Ball off Wall
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) & lead(ball_eventcode == 10, n=2) &
          lead(row_id == 4, n=3) & lead(player_id == 255, n=3) & lead(ball_eventcode == 16, n=3)
          ~ "Live Ball-Off Wall",
        #Live Ball Off Wall and Bobbled
         row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) & lead(ball_eventcode == 10, n=2) &
          lead(row_id == 4, n=3) & lead(player_id %in% c(1:9), n=3) & lead(ball_eventcode == 9, n=3)
          ~ "Live Ball-Off Wall-Bobbled",
        #Live Ball off wall, fielded with no bounce
          row_id == 1 & player_id == 1 & ball_eventcode == 1 &
          lead(row_id == 2) & lead(player_id == 10) & lead(ball_eventcode == 4) &
          lead(row_id == 3, n=2) & lead(player_id == 255, n=2) & lead(ball_eventcode == 10, n=2) &
          lead(row_id == 4, n=3) & lead(player_id %in% c(1:9), n=3) & lead(ball_eventcode %in% c(2,7), n=3)
          ~ "Live Ball-Off Wall-No Bounce"
    ) 
  )

##-----------------Pickoffs and Inning change-----------------------------------
play_description_enhanced <- lineups_df %>%
  #Adds the play type to the dataframe that contain info on each single play
  left_join(.,
            play_description %>% 
              select(game_string_play, play_type) %>%
              #Each grouped play from play_description only has the play_type
              #listed in the first row. This condenses that information to have
              #One play with only one row
              filter(!is.na(play_type)),
            by = join_by(game_string_play)
  ) %>%
  mutate(
    #Defines the pickoff play
    play_type = if_else(is_pickoff == TRUE, "Pickoff", play_type),
    #Reset outs to 0 at start of game and new half inning
    outs_before = case_when(
      play_per_game == 1 ~ 0,
      lag(half_inning) != half_inning | lag(inning) != inning ~ 0
    )
  ) %>%
  relocate(outs_before, .before = play_type)

#Lists how many of each play occurred
table(play_description_enhanced$play_type)

#These dataframes display show which plays still aren't defined, with
#ball events included. These won't be defined, but wanted to show what they were
# and what happened to them
play_description_enhanced__na_list <- play_description_enhanced %>%
  filter(is.na(play_type))%>%
  pull(game_string_play)

play_description_enhanced_na_df <- ball_events_df %>%
  filter(game_string_play %in% play_description_enhanced__na_list)

#-------------Calculate Outs----------------------------------------------------

#Dataset will hold the calculation for outs
play_description_outs <- play_description_enhanced %>%
  mutate(
    #Replace NA values with empty strings for comparison
    on_1b = replace_na(on_1b, ""),
    on_2b = replace_na(on_2b, ""),
    on_3b = replace_na(on_3b, ""),
    #Calculate outs
    outs_during_p1 = case_when(
      #Pitch, not strikeout or walk
      play_type == "Pitch" & batter == lead(batter) ~ 0,
      #Walk
      play_type == "Pitch" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Bounced" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Bounced" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Bounced" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Bounced" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,

      #Runners stayed, same batter
      play_type == "Pitch-Bounced-Throw Down" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Bounced-Throw Down" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Bounced-Throw Down" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Bounced-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runnners stayed, same batter
      play_type == "Pitch-Bounced-End" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Bounced-End" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Bounced-End" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Bounced-End" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Bounced-Deflected" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Bounced-Deflected" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Bounced-Deflected" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Bounced-Deflected" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Bounced-Deflected-Throw Down" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Bounced-Deflected-Throw Down" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Bounced-Deflected-Throw Down" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Bounced-Deflected-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Bounced-Deflected-End" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Bounced-Deflected-End" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Bounced-Deflected-End" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Bounced-Deflected-End" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Deflected" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Deflected" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Deflected" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeput
      play_type == "Pitch-Deflected" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Deflected-Throw Down" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Deflected-Throw Down" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Deflected-Throw Down" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Deflected-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Deflected-End" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Deflected-End" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Deflected-End" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Deflected-End" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Deflected-Bounced" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Deflected-Bounced" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Deflected-Bounced" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Deflected-Bounced" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Deflected-Bounced-Throw Down" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Deflected-Bounced-Throw Down" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Deflected-Bounced-Throw Down" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Deflected-Bounced-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Runners stayed, same batter
      play_type == "Pitch-Deflected-Bounced-End" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Runners advanced, same batter
      play_type == "Pitch-Deflected-Bounced-End" & batter == lead(batter) & (on_1b != lead(on_1b) | on_2b != lead(on_2b) | on_3b != lead(on_3b)) ~ 0,
      #Walk
      play_type == "Pitch-Deflected-Bounced-End" & batter != lead(batter) & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Strikeout
      play_type == "Pitch-Deflected-Bounced-End" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Not Strikeout/Walk, No attempted advance
      play_type == "Pitch-Throw Down" & batter == lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Not Strikeout/Walk, Caught trying to steal 3rd
      play_type == "Pitch-Throw Down" & batter == lead(batter) & on_2b != lead(on_3b) ~ 1,
      #Not Strikeout/Walk, Caught trying to Steal 2nd, possibly 3rd on bad throw
      play_type == "Pitch-Throw Down" & batter == lead(batter) & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
      #Not Strikeout/Walk, Not caught trying to steal 3rd
      play_type == "Pitch-Throw Down" & batter == lead(batter) & on_2b == lead(on_3b) ~ 0,
      #Not Strikeout/Walk, Not caught trying to steal 2nd, possibly 3rd on bad throw
      play_type == "Pitch-Throw Down" & batter == lead(batter) & (on_1b == lead(on_2b) | on_1b == lead(on_3b)) ~ 0,
      #Strikeout, no attempted advance
      play_type == "Pitch-Throw Down" & batter != lead(batter) & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 1,
      #Strikeout, Not Caught Trying to Steal 3rd
      play_type == "Pitch-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) & on_2b == lead(on_3b) ~ 1,
      #Strikeout, Not Caught Trying to Steal 2nd, possibly 3rd on bad throw
      play_type == "Pitch-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) & (on_1b == lead(on_2b) | on_1b == lead(on_3b)) ~ 1,
      #Strikeout, Caught trying to steal 3rd
      play_type == "Pitch-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) & on_2b != lead(on_3b) ~ 2,
      #Strikeout, Caught trying to steal 2nd, possibly 3rd on bad throw
      play_type == "Pitch-Throw Down" & batter != lead(batter) & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 2,
      #Walk, Not caught trying to steal 3rd
      play_type == "Pitch-Throw Down" & batter == lead(on_1b) & on_2b == lead(on_3b) ~ 1,
      #Walk, Caught trying to steal 3rd
      play_type == "Pitch-Throw Down" & batter == lead(on_1b) & on_2b != lead(on_3b) ~ 1,
      
      #Unsuccessful pickoff, runners stayed
      play_type == "Pickoff" & on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b) ~ 0,
      #Successful pickoff at 3rd
      play_type == "Pickoff" & on_3b != lead(on_3b) ~ 1,
      #Successful pickoff at 2nd
      play_type == "Pickoff" & on_2b != lead(on_2b) & on_2b != lead(on_3b) ~ 1,
      #Successful pickoff at 1st
      play_type == "Pickoff" & on_1b != lead(on_1b) & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
      
      #Hit by pitch cant have an out
      play_type == "Hit By Pitch" ~ 0,
      play_type == "Hit By Pitch-Bounced" ~ 0,
      
      #Foul ball cant have an out
      play_type == "Foul Ball" ~ 0,
      play_type == "Foul Ball-Off Wall" ~ 0,
      
      #Home run cant have an out
      play_type == "Home Run" ~ 0,
      
      #Ball in play, batter reaches base
      play_type == "Live Ball" & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Ball in play, batter doesn't reach base
      play_type == "Live Ball" & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Ball in play, batter reaches base
      play_type == "Live Ball-Deflected" & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Ball in play, batter doesn't reach base
      play_type == "Live Ball-Deflected" & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Ball in play, batter reaches base
      play_type == "Live Ball-Off Wall" & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Ball in play, batter doesn't reach base
      play_type == "Live Ball-Off Wall" & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Ball in play, batter reaches base
      play_type == "Live Ball-Off Wall-No Bounce" & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Ball in play, batter doesn't reach base
      play_type == "Live Ball-Off Wall-No Bounce" & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Ball in play, batter reaches base
      play_type == "Live Ball-Off Wall-Bobbled" & (batter == lead(on_1b) | batter == lead(on_2b) | batter == lead(on_3b)) ~ 0,
      #Ball in play, batter doesn't reach base
      play_type == "Live Ball-Off Wall-Bobbled" & batter != lead(on_1b) & batter != lead(on_2b) & batter != lead(on_3b) ~ 1,
      
      #Ball hit in air and caught by an infielder, and nothing happens after
      play_type == "Infield Fly Out" ~ 1,
      play_type == "Infield Fly Out-Bobbled" ~ 1,
      
      #Ball hit in air to outfielder and caught, nothing happens after
      play_type == "Outfield Fly Out" ~ 1,
      
      #Other, more complex plays are accounted for in later steps
    )
  ) %>%
  group_by(game_string, inning, half_inning) %>%
    #Sums up current accounted for outs
    mutate(outs_total_p1 = cumsum(outs_during_p1)) %>%
  ungroup() %>%
  #Calculate outs for complex fly outs
  mutate(
    outs_during_p2 = case_when(
        #Complex, but the inning ends on the catch so everything after is irrelevant
        play_type == "Infield Fly Out-Complex" &
        lag(outs_total_p1) == 2
        ~ 1,
        #Runners are all in same place after play. No movement means no one was thrown out
        play_type == "Infield Fly Out-Complex" &
        on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b)
        ~ 1,
        #Runner on third moved. Unclear if they were out. Will need to be checked later
        play_type == "Infield Fly Out-Complex" &
        on_3b != "" &
        on_3b != lead(on_3b)
        ~ 2,
        #Runner on second tagged up
        play_type == "Infield Fly Out-Complex" &
        on_2b != "" &
        on_2b == lead(on_3b)
        ~ 1,
        #Runner on second was thrown out
        play_type == "Infield Fly Out-Complex" &
        on_2b != "" &
        on_2b != lead(on_2b) & on_2b != lead(on_3b)
        ~ 2,
        #Runner on first tagged up
        play_type == "Infield Fly Out-Complex" &
        on_1b != "" &
        (on_1b == lead(on_2b) | on_1b == lead(on_3b))
        ~ 1,
        #Runner on first was thrown out
        play_type == "Infield Fly Out-Complex" &
        on_1b != "" &
        on_1b != lead(on_1b) & on_1b != lead(on_2b) & on_1b != lead(on_3b)
        ~ 2,
        
        
        #Complex, but the inning ends on the catch so everything after is irrelevant
        play_type == "Outfield Fly Out-Complex" &
        lag(outs_total_p1) == 2
        ~ 1,
        #Runners are all in same place after play. No movement means no one was thrown out
        play_type == "Outfield Fly Out-Complex" &
        on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b)
        ~ 1,
        #Runner on third moved. Unclear if they were out. Will need to be checked later
        play_type == "Outfield Fly Out-Complex" &
        on_3b != "" &
        on_3b != lead(on_3b)
        ~ 2,
        #Runner on second tagged up
        play_type == "Outfield Fly Out-Complex" &
        on_2b != "" &
        on_2b == lead(on_3b)
        ~ 1,
        #Runner on second was thrown out
        play_type == "Outfield Fly Out-Complex" &
        on_2b != "" &
        on_2b != lead(on_2b) & on_2b != lead(on_3b)
        ~ 2,
        #Runner on first tagged up
        play_type == "Outfield Fly Out-Complex" &
        on_1b != "" &
        (on_1b == lead(on_2b) | on_1b == lead(on_3b))
        ~ 1,
        #Runner on first was thrown out
        play_type == "Outfield Fly Out-Complex" &
        on_1b != "" &
        on_1b != lead(on_1b) & on_1b != lead(on_2b) & on_1b != lead(on_3b)
        ~ 2, 
        
        
        #Complex, but the inning ends on the catch so everything after is irrelevant
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        lag(outs_total_p1) == 2
        ~ 1,
        #Runners are all in same place after play. No movement means no one was thrown out
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        on_1b == lead(on_1b) & on_2b == lead(on_2b) & on_3b == lead(on_3b)
        ~ 1,
        #Runner on third moved. Unclear if they were out. Will need to be checked later
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        on_3b != "" &
        on_3b != lead(on_3b)
        ~ 2,
        #Runner on second tagged up
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        on_2b != "" &
        on_2b == lead(on_3b)
        ~ 1,
        #Runner on second was thrown out
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        on_2b != "" &
        on_2b != lead(on_2b) & on_2b != lead(on_3b)
        ~ 2,
        #Runner on first tagged up
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        on_1b != "" &
        (on_1b == lead(on_2b) | on_1b == lead(on_3b))
        ~ 1,
        #Runner on first was thrown out
        play_type == "Outfield Fly Out-Bobbled-Complex" &
        on_1b != "" &
        on_1b != lead(on_1b) & on_1b != lead(on_2b) & on_1b != lead(on_3b)
        ~ 2
      )
    ) %>%
  #Updates the current calculation of outs
  rowwise() %>%
    mutate(outs_net_p2 = sum(outs_during_p1, outs_during_p2, na.rm = TRUE)) %>%
  group_by(game_string, inning, half_inning) %>%
    mutate(outs_total_p2 = cumsum(outs_net_p2)) %>%
  ungroup() %>%
  mutate(
    #This column will add on an extra out to live ball plays that did not record outs on the runners on base
    outs_during_p3 = case_when(
      #Other runners on base are out
      play_type == "Live Ball" & outs_total_p2 < 2 & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
      play_type == "Live Ball-Deflected" & outs_total_p2 < 2 & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
      play_type == "Live Ball-Off Wall" & outs_total_p2 < 2 & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
      play_type == "Live Ball-Off Wall-No Bounce" & outs_total_p2 < 2 & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
      play_type == "Live Ball-Off Wall-Bounce" & outs_total_p2 < 2 & on_1b != lead(on_2b) & on_1b != lead(on_3b) ~ 1,
    )
  ) %>%
  #Updates the current calculation of outs
  rowwise() %>%
    mutate(outs_net_p3 = sum(outs_during_p1, outs_during_p2, outs_during_p3, na.rm = TRUE)) %>%
  group_by(game_string, inning, half_inning) %>%
    mutate(outs_total_p3 = cumsum(outs_net_p3)) %>%
  ungroup() %>%
  mutate(
    outs_during_p4 = case_when(
      #Tag from 3rd recalculation
      play_type == "Outfield Fly Out-Complex" & on_3b != "" & any(outs_total_p2 == 4) ~ -1
    )
  ) %>%
  #Updates the current calculation of outs
  rowwise() %>%
    mutate(outs_net_p4 = sum(outs_during_p1, outs_during_p2, outs_during_p3, outs_during_p4, na.rm = TRUE)) %>%
  group_by(game_string, inning, half_inning) %>%
    mutate(outs_total_p4 = cumsum(outs_net_p4)) %>%
  ungroup()
  
      
 
#Lists out plays that still didn't have outs accounted
#Just to keep of what was missed
play_description_outs_na_df <- play_description_outs %>%
  filter(is.na(outs_during_p1) & is.na(outs_during_p2)) %>%
  inner_join(ball_events_df, ., by = join_by(game_string_play))

#Shows the last play of each half inning
#Shows how many of each inning ended with or w/o 3 outs
end_of_inning_out_count <- play_description_outs %>%
  filter(lead(outs_before) == 0)

#Shows the full dataframe with each half inning labeled with correctly having 
#3 outs at the end.
#Also shows if at any point batter was n/a is half inning because caused 
#problems with evaluating the outs
end_of_inning_out_count_full <- play_description_outs %>%
  group_by(game_string, inning, half_inning) %>%
    mutate(three_outs = if_else(last(outs_total_p4) == 3, TRUE, FALSE),
           batter_na = if_else(any(is.na(batter)), TRUE, FALSE)) %>%
  ungroup() %>%
  filter(three_outs == FALSE, batter_na == FALSE) %>%
  inner_join(ball_events_df, ., by = join_by(game_string_play))

#---------------Relevant Plays--------------------------------------------------

#Pull out the half innings that have an outfield fly out of any type,
#Ended correctly with 3 outs, and didnt have any missing batters in the inning
relevant_plays <- play_description_outs %>%
  #Replaces N/a with blanks for easy comparison
  mutate(play_type = replace_na(play_type, "")) %>%
  group_by(game_string, inning, half_inning) %>%
    mutate(half_inning_has_outfield_fly = if_else(any(play_type == "Outfield Fly Out" | play_type == "Outfield Fly Out-Complex" | play_type == "Outfield Fly Out-Bobbled-Complex"),TRUE,FALSE),
           three_outs = if_else(last(outs_total_p4) == 3, TRUE, FALSE),
           batter_na = if_else(any(is.na(batter)), TRUE, FALSE)) %>%
    arrange(game_string, inning, desc(half_inning)) %>%
  ungroup() %>%
  filter(half_inning_has_outfield_fly == TRUE, three_outs == TRUE, batter_na == FALSE) %>%
  #Removes all the intermediary outs columns, shows only the final ones
  select(game_string:play_type, outs_net_p4, outs_total_p4)

#---------------------Model Variables-------------------------

#Adding variables for model to relevant plays  
relevant_plays_timestamp <- relevant_plays %>%
    mutate(
      #Determine the lead advance base and its location on the field
      next_base = case_when(
        on_3b != "" ~ 4,
        on_2b != "" ~ 3,
        on_1b != ""~ 2,
        on_1b == "" & on_2b == "" & on_3b == "" ~ 1
      ),
      next_base_x = case_when(
        next_base == 4 ~ 0,
        next_base == 3 ~ -45*sqrt(2),
        next_base == 2 ~ 0,
        next_base == 1 ~ 45*sqrt(2)
      ), 
      next_base_y = case_when(
        next_base == 4 ~ 0,
        next_base == 3 ~ 45*sqrt(2),
        next_base == 2 ~ 90*sqrt(2),
        next_base == 1 ~ 45*sqrt(2)
      ),
      #Check if there were two outs before the play occurred
      two_outs_before_play = ifelse(
        outs_net_p4 == 1 & outs_total_p4 == 3,
        TRUE, 
        FALSE
      ), 
      #Check if runners started on base when play occurred
      runners_on = ifelse(
        next_base != 1,
        TRUE,
        FALSE
      ),
      #Define potential tag up situation
      tagging_situation = ifelse(
        runners_on == TRUE & two_outs_before_play == FALSE,
        TRUE, 
        FALSE
      ),
      #Check if lead runner tagged up successfully
      lead_runner_advanced = ifelse(
        tagging_situation == TRUE & 
          (next_base == 4 & on_3b != lead(on_3b) |
            next_base == 3 & on_2b == lead(on_3b) |
             next_base == 2 & on_1b == lead(on_2b)
          ),
        TRUE,
        FALSE
      ),
      #Check if outfielder successfully prevented the leader from tagging up
      successful_outcome = ifelse(
        (tagging_situation == TRUE & (lead_runner_advanced == FALSE | outs_net_p4 == 2)) |
        tagging_situation == FALSE,
        TRUE,
        FALSE
      )
    ) %>%
    #Contact timestamp
    left_join(.,
              ball_events_df %>%
                select(game_string_play, ball_eventcode, timestamp) %>%
                group_by(game_string_play) %>%
                filter(row_number() == first(which(ball_eventcode == 4))) %>%
                ungroup(),
              by = join_by(game_string_play)
    ) %>%
    rename(contact_time = timestamp) %>%
    select(-ball_eventcode) %>%
    #Catch timestamp and player who made the catch
    left_join(.,
              ball_events_df %>%
                select(game_string_play, player_id, ball_eventcode, timestamp) %>%
                group_by(game_string_play) %>%
                    filter(row_number() == first(which(ball_eventcode == 2))) %>%
                ungroup(),
              by = join_by(game_string_play)
              ) %>%
    rename(catch_time = timestamp) %>%
    select(-ball_eventcode) %>%
    #Postion of player who made the catch at contact
    left_join(.,
              player_positions_df %>%
                select(game_string, play_per_game, player_id, timestamp, field_x, field_y),
              by = join_by(game_string, play_per_game, player_id, contact_time == timestamp),
              na_matches = "never"
              ) %>%
    rename(contact_x = field_x, contact_y = field_y) %>%
    #Position of player who made the catch when they caught it
    left_join(.,
              player_positions_df %>%
                select(game_string, play_per_game, player_id, timestamp, field_x, field_y),
              by = join_by(game_string, play_per_game, player_id, catch_time == timestamp),
              na_matches = "never"
    ) %>%
    rename(catch_x = field_x, catch_y = field_y) %>%
    mutate(
      #Time ball was in air when hit
      airtime = (catch_time - contact_time) / 1000,
      #Which direction the player went to catch
      #Cartesian quadrants
      catch_quadrant = case_when(
        catch_x > contact_x & catch_y > contact_y ~ 1,
        catch_x < contact_x & catch_y > contact_y ~ 2,
        catch_x < contact_x & catch_y < contact_y ~ 3,
        catch_x > contact_x & catch_y < contact_y ~ 4
      ),
      #How far on optimal line player would need to go to make catch
      catch_distance = sqrt((catch_y - contact_y)^2 + (catch_x - contact_x)^2),
      #Build out the line on the field of the optimal path to catch ball
      catch_line_slope = (catch_y - contact_y) / (catch_x - contact_x),
      catch_line_intercept = catch_y - (((catch_y - contact_y) / (catch_x - contact_x)) * catch_x),
      catch_line_equation = paste("y = ", catch_line_slope, "x + ", catch_line_intercept, sep = ""),
      #Convert line to angle
      catch_line_angle = case_when(
        catch_quadrant == 1 ~ atan(catch_line_slope) * (180 / pi),
        catch_quadrant == 2 ~ atan(catch_line_slope) * (180 / pi) + 180,
        catch_quadrant == 3 ~ atan(catch_line_slope) * (180 / pi) + 180,
        catch_quadrant == 4 ~ atan(catch_line_slope) * (180 / pi) + 360
      ),
      #Which direction player was moving when making catch
      #Cartesian quadrants
      throw_quadrant = case_when(
        next_base_x > catch_x & next_base_y > catch_y ~ 1,
        next_base_x < catch_x & next_base_y > catch_y ~ 2,
        next_base_x < catch_x & next_base_y < catch_y ~ 3,
        next_base_x > catch_x & next_base_y < catch_y ~ 4
        
      ),
      #Optimal line for throw to be to next base of the lead runner
      throw_line_slope = (catch_y-next_base_y) / (catch_x - next_base_x),
      throw_line_intercept = catch_y - (((catch_y - next_base_y) / (catch_x - next_base_x)) * catch_x),
      throw_line_equation = paste("y = ", throw_line_slope, "x + ", throw_line_intercept, sep = ""),
      #Convert line to angle
      throw_line_angle = case_when(
        throw_quadrant == 1 ~ atan(throw_line_slope) * (180 / pi),
        throw_quadrant == 2 ~ atan(throw_line_slope) * (180 / pi) + 180,
        throw_quadrant == 3 ~ atan(throw_line_slope) * (180 / pi) + 180,
        throw_quadrant == 4 ~ atan(throw_line_slope) * (180 / pi) + 360
      ),
    ) %>%
    #Catch Line Details
    left_join(.,
              player_positions_df %>%
                select(game_string:field_y) %>%
                filter(player_id %in% c(7,8,9)) %>%
                group_by(game_string, play_per_game, player_id) %>%
                  #Only need first 4 rows of each group to calculate jerk
                  filter(row_number() <= 4) %>%
                  mutate(
                    #All this goes to calculating jerk magnitude and direction
                    displacement = sqrt((field_y - lag(field_y))^2 + (field_x - lag(field_x))^2),
                    #displacement over time
                    velocity = displacement / ((timestamp - lag(timestamp)) / 1000),
                    #Change in velocity over time
                    acceleration = (velocity - lag(velocity)) / ((timestamp - lag(timestamp)) / 1000),
                    #Change in acceleration over time
                    #Jerk magnitude at contact
                    catch_jerk = (acceleration - lag(acceleration)) / ((timestamp - lag(timestamp)) / 1000),
                    #Jerk direction at contact
                    catch_jerk_quadrant = case_when(
                      field_x > lag(field_x, n=3) & field_y > lag(field_y, n=3) ~ 1,
                      field_x < lag(field_x, n=3) & field_y > lag(field_y, n=3) ~ 2,
                      field_x < lag(field_x, n=3) & field_y < lag(field_y, n=3) ~ 3,
                      field_x < lag(field_x, n=3) & field_y < lag(field_y, n=3) ~ 4
                    ) , 
                    catch_jerk_angle = case_when(
                      catch_jerk_quadrant == 1 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi),
                      catch_jerk_quadrant == 2 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi) + 180,
                      catch_jerk_quadrant == 3 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi) + 180,
                      catch_jerk_quadrant == 4 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi) + 360
                    )
                  ) %>%
                rowwise() %>%
                #Only last row of group has jerk, so remove the other rows
                filter(!is.na(catch_jerk)) %>%
                select(game_string, play_per_game, player_id, catch_jerk, catch_jerk_angle),
              by = join_by(game_string, play_per_game, player_id)
    ) %>%
    mutate(
      #Calculates how close to the optimal angle the first step after contact was
      catch_first_step_accuracy = case_when(
        abs(catch_jerk_angle - catch_line_angle) > 180 ~ (180 - abs(180 - abs(catch_jerk_angle - catch_line_angle))) / 180 *100,
        abs(catch_jerk_angle - catch_line_angle) <= 180 ~ abs(catch_jerk_angle - catch_line_angle) / 180 *100
      )
    ) %>%
    #Throw Line Details
    #Same process as catch line details, but for at time of catch
    left_join(.,
              player_positions_df %>%
                select(game_string:field_y) %>%
                filter(player_id %in% c(7,8,9)) %>%      
                left_join(.,
                          relevant_plays_timestamp %>%
                            select(game_string, play_per_game, catch_time),
                          by = join_by(game_string, play_per_game)
                ) %>%
                filter(!is.na(catch_time) & timestamp >= catch_time) %>%
                group_by(game_string, play_per_game, player_id) %>%
                  filter(row_number() <= 4) %>%
                  mutate(
                    displacement = sqrt((field_y - lag(field_y))^2 + (field_x - lag(field_x))^2),
                    velocity = displacement / ((timestamp - lag(timestamp)) / 1000),
                    acceleration = (velocity - lag(velocity)) / ((timestamp - lag(timestamp)) / 1000),
                    throw_jerk = (acceleration - lag(acceleration)) / ((timestamp - lag(timestamp)) / 1000),
                    throw_jerk_quadrant = case_when(
                      field_x > lag(field_x, n=3) & field_y > lag(field_y, n=3) ~ 1,
                      field_x < lag(field_x, n=3) & field_y > lag(field_y, n=3) ~ 2,
                      field_x < lag(field_x, n=3) & field_y < lag(field_y, n=3) ~ 3,
                      field_x < lag(field_x, n=3) & field_y < lag(field_y, n=3) ~ 4
                    ) , 
                    throw_jerk_angle = case_when(
                      throw_jerk_quadrant == 1 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi),
                      throw_jerk_quadrant == 2 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi) + 180,
                      throw_jerk_quadrant == 3 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi) + 180,
                      throw_jerk_quadrant == 4 ~ atan((field_y - lag(field_y, n=3)) / (field_x - lag(field_x, n=3))) * (180/pi) + 360
                    )
                  ) %>%
                rowwise() %>%
                filter(!is.na(throw_jerk)) %>%
                select(game_string, play_per_game, player_id, throw_jerk, throw_jerk_angle),
              by = join_by(game_string, play_per_game, player_id)
    ) %>%
    mutate(
      throw_first_step_accuracy = case_when(
        abs(throw_jerk_angle - throw_line_angle) > 180 ~ (180 - abs(180 - abs(throw_jerk_angle - throw_line_angle))) / 180 * 100,
        abs(throw_jerk_angle - throw_line_angle) <= 180 ~ abs(throw_jerk_angle - throw_line_angle) / 180 * 100
      )
    ) %>%
    #Cuts down to just the fly outs with all important variables included
    #Doesn't include potential tag ups to home because those outs can't be tracked
    filter(play_type %in% c("Outfield Fly Out", "Outfield Fly Out-Bobbled", "Outfield Fly Out-Bobbled-Complex","Outfield Fly Out-Complex") &
             next_base != 4)
  
              
              
#Calculates the error of the path the player ran to catch
#Normalizes the error based on distance of the optimal route
#Value produced is error squared per ft needed to cover
catch_line_accuracy_df <- player_positions_df %>%
  left_join(
    .,
    relevant_plays_timestamp %>% 
      select(game_string, play_per_game, player_id, airtime, catch_distance, catch_line_slope, catch_line_intercept, catch_line_equation),
    by = join_by(game_string, play_per_game, player_id)
  ) %>%
  filter(!is.na(catch_line_equation)) %>%
  mutate(field_y_pred = (catch_line_slope * field_x) + catch_line_intercept) %>%
  group_by(game_string, play_per_game, catch_distance) %>%
    summarise(SSE = sum((field_y - field_y_pred)^2)) %>%
  ungroup() %>%
  mutate(catch_line_accuracy = SSE / catch_distance)

#Adds the accuracy of the route the play took to catch to dataframe
final_df <- relevant_plays_timestamp %>%
  left_join(
    .,
    catch_line_accuracy_df %>% 
      select(game_string, play_per_game, catch_line_accuracy), 
    by = join_by(game_string, play_per_game)
  )

#-------------------Model-------------------------

#splits the data into training and testing
data_split <- initial_split(final_df , prop = .8)

train_data <- training(data_split)

test_data <- testing(data_split)

#Generates model
tag_prevention_model <- glm(successful_outcome ~ airtime + catch_distance + 
                              catch_line_accuracy + catch_first_step_accuracy + 
                              #Throw jerk and throw first step acccuracy only matter if its a tagging situation
                              catch_jerk + (throw_first_step_accuracy + throw_jerk) * tagging_situation,
                            family = binomial(link = "logit"),
                            data = train_data)
summary(tag_prevention_model)

#Calculates the rating of each play by inverting the probability of a successful outcome
#Basically, plays with lowest likelihood of success that did succeed will be rated highest
final_df$rating <- 1 -predict(tag_prevention_model, newdata = final_df, type = "response")

#Leaderboard only shows when a player successfully made the play
leaderboard_plays <- final_df %>%
  filter(successful_outcome == TRUE) %>%
  arrange(desc(rating)) %>%
  mutate(
    player_involved_name = case_when(
      player_id == 7 ~ left_fielder,
      player_id == 8 ~ center_fielder,
      player_id == 9 ~ right_fielder
    ),
    rank = row_number(), .before = 1
  ) %>%
  select(rank, player_involved_name, rating, airtime, catch_distance, 
         catch_line_accuracy, catch_first_step_accuracy, catch_jerk, 
         throw_first_step_accuracy, throw_jerk)

#--------------Visualization-----------------
leaderboard_plays %>%
  #Columns to include in table
  select(rank, player_involved_name, catch_distance, airtime,
         catch_first_step_accuracy, throw_first_step_accuracy) %>%
  #Rank the top 5 plays
  head(5) %>%
  gt() %>%
  #Table title
  tab_header(title = "Top Outfield Catches") %>%
  #Table footnotes
  tab_footnote("Not including plays at the plate") %>%
  tab_footnote("Not all factors shown") %>%
  #Not including the jerk stats because it would be difficult for fans to
  #Undertsand and it takes up space from more significant valyes
  cols_label(player_involved_name = "Player",
             rank = "",
             airtime = "Airtime(s)",
             catch_distance = "Distance(ft)",
             catch_first_step_accuracy = "First Step Quality",
             throw_first_step_accuracy = "Throw Momentum Quality") %>%
  #Format numbers with one decimal
  fmt_number(
    columns = c(airtime, catch_distance),
    decimals = 1
  ) %>% 
  #Format percent values with 0 decimals
  fmt_percent(
    columns = c(catch_first_step_accuracy, throw_first_step_accuracy),
    decimals = 0,
    scale_values = FALSE
  ) %>%
  #Colors found on listed website
  #https://htmlcolorcodes.com/color-picker/
  #Style guides learned from gt package documentation
  
  #Color column labels
  tab_style(
    style = list(
      cell_fill(color = "#1C2841"), 
      cell_text(color = "#FFFFFF", weight = "bold", align = "center")
    ),
    locations = cells_column_labels()
  ) %>% 
  #Makes rows striped
  opt_row_striping() %>% 
  tab_options(
    row.striping.background_color = "#F0F4F8" # Very soft, opaque blue-gray
  ) %>% 
  #Highlight Ranking column
  tab_style(
    style = cell_fill(color = "#FAF9F6"), 
    locations = cells_body(columns = rank)
  ) %>% 
  #Highlight top play
  tab_style(
    style = list(
      cell_fill(color = "#FFD6D6"), # Faded, highly readable red
      cell_text(color = "#8B0000", weight = "bold") # Dark red text for legibility
    ),
    locations = cells_body(rows = rank == 1)
  ) %>% 
  tab_options(
    #Change fonts
    table.font.names = c("Kanit", "Chivo", "Arial", "sans-serif"),
    table.font.size = px(14),
    
    #Color the borders and change widths
    table.border.top.color = "#1C2841",
    table.border.top.width = px(3),
    table.border.bottom.color = "#1C2841",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.color = "#1C2841",
    column_labels.border.bottom.width = px(2)
  )



  

#--------Animation tests----------

animate_play("y1_d108_SPL_PHD", 58)

create_play_by_play("y1_d217_VAS_ARN", 316)





