# Generate looking duration to each fam video clip from csvs exported from Datavyu, Exp 1
# Shari Liu

if(!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if(!require("dplyr")) {install.packages("dplyr"); require("dplyr")}

# init blank data frame
looks.on.group <- NA

# get all data files from the dir
datafiles <- list.files(".", pattern=".csv", recursive=FALSE, ignore.case = TRUE)

# read out durations per videoclip per trial per subject
for (i in 1:length(datafiles)) {
    curfile <- datafiles[i]
    subjID <- str_remove(curfile, ".csv")

    data <- read.csv(curfile, header = TRUE) 
    names(data) <- tolower(names(data))

    # there might be ways to condense this, but here's what I've got that works!
    
    # make 3 new tibbles: one for looks away, one for videoclips, and one for trials
    off <- data %>%
      select(off.onset, 
             off.offset, 
             off.lessthan2_2ormore) %>%
      rename(onset= off.onset, 
             offset= off.offset, 
             crit= off.lessthan2_2ormore) %>%
      na.omit() %>%
      mutate(event = 'off', videoclip = NA) 
    
    
    trials <- data %>%
      select(trial.onset, 
             trial.offset, 
             trial.famx) %>%
      rename(onset = trial.onset, 
             offset = trial.offset, 
             trial = trial.famx) %>%
      mutate(event = 'trial') %>%
      na.omit()
    
    videoclip <- data %>%
      select(event.onset, 
             event.offset, 
             event.depth_yesno) %>%
      rename(onset = event.onset, 
             offset = event.offset, 
             videoclip = event.depth_yesno) %>%
      na.omit() %>%
      mutate(event = 'videoclip', trial=NA) 
    
    # calculate looking times within each video clip
    
    # first, figure out which off-looks go with which movies
    for (i in 1:nrow(off)) {
      for (j in 1:nrow(videoclip)) {
        if (videoclip$onset[j] <= off$onset[i] & videoclip$offset[j] >= off$offset[i]) {
          off$videoclip[i] <- videoclip$videoclip[j]
        }
      }
    }
    
    # then, figure out which movies go with which trials
    for (i in 1:nrow(videoclip)) {
      for (j in 1:nrow(trials)) {
        if (trials$onset[j] <= videoclip$onset[i] & trials$offset[j] >= videoclip$offset[i]) {
          videoclip$trial[i] <- trials$trial[j]
        }
      }
    }
    
    # and finally, figure out which offs go with which trials
    for (i in 1:nrow(off)) {
      for (j in 1:nrow(trials)) {
        if (trials$onset[j] <= off$onset[i] & trials$offset[j] >= off$offset[i]) {
          off$trial[i] <- trials$trial[j]
        }
      }
    }
    
    looks.on <- merge(videoclip, off, all=TRUE) %>%
      mutate(
             subjID = subjID,
             duration = (offset-onset)/1000) %>%
      group_by(subjID,trial,videoclip) %>%
      mutate(duration.on = sum(duration[event=="videoclip"]-sum(duration[event=="off"]))) %>%
      distinct(duration.on) %>%
      as.data.frame()
    
    # add to group data
    looks.on.group <- rbind(looks.on.group, looks.on)
}

looks.on.group %>% 
  mutate(trial = str_replace_all(trial, "[[:punct:]]", "")) %>% # remove trial annotation that is irrelevant for this analysis
  mutate(videoclip = str_replace_all(videoclip, " ", "_")) %>%
  na.omit() %>%
  filter(trial != "intro") %>% # remove looks to trench fam
  filter(!str_detect(videoclip, "\\*")) %>% # remove all videos where infant didn't see the agent accept or refuse
  write.csv("exp1_fam_looks.csv") # write to csv

