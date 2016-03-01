library(Lahman)
library(dplyr)
library(retrosheet)

YearData <- Batting %>%
  group_by(yearID) %>%
  summarise(AB = sum(AB, na.rm=TRUE), H=sum(H, na.rm=TRUE), HR95p=quantile(HR, .95, na.rm=TRUE),
            BB=sum(BB, na.rm=TRUE), RBI=sum(RBI, na.rm=TRUE), SO=sum(SO, na.rm=TRUE)) %>%
  mutate(Avg=H/AB, KRate=SO/AB) %>% filter(yearID >= 1920)

PlayerSeason <- Batting %>%
  left_join(select(Master, playerID, nameLast)) %>%
  left_join(select(YearData, yearID, HR95p)) %>%
  arrange(desc(HR)) %>% select(nameLast, yearID, teamID, HR, HR95p) %>%
  mutate(label=paste0(nameLast, "-", yearID, "-", teamID))

GameData2015t <- getRetrosheet(type="game", year=2015)

GameData2015 <- bind_rows(
  GameData2015t %>%
    transmute(team=HmTm, opp=VisTm, Runs=HmRuns, RunsAllowed=VisRuns, League=HmTmLg, win=ifelse(HmRuns>VisRuns, 1, 0), AB=HmAB, Hits=HmH, Interleague=ifelse(HmTmLg==VisTmLg, FALSE, TRUE)),
  GameData2015t %>%
    transmute(team=VisTm, opp=HmTm, Runs=VisRuns, RunsAllowed=HmRuns, League=VisTmLg, win=ifelse(VisRuns>HmRuns, 1, 0), AB=VisAB, Hits=VisH, Interleague=ifelse(HmTmLg==VisTmLg, FALSE, TRUE))
  )

HeadToHeadResults <- GameData2015 %>% filter(!Interleague) %>%
  group_by(team, opp) %>%
  summarise(AB=sum(AB, na.rm=TRUE), Hits=sum(Hits, na.rm=TRUE)) %>%
  mutate(Avg=Hits/AB)

TeamData2015 <- GameData2015 %>%
  group_by(team) %>%
  summarise(Runs=sum(Runs), RunsAllowed=sum(RunsAllowed), Wins=sum(win)) %>%
  mutate(RunDiff=Runs-RunsAllowed)

devtools::use_data(YearData, GameData2015, TeamData2015, HeadToHeadResults, PlayerSeason,
                   overwrite = TRUE, internal = TRUE)
