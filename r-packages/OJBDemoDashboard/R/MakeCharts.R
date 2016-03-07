#' Internal function for controlling the theme in one place
#' 
#' @import ggthemes
getTheme <- function() {
  ggthemes::theme_gdocs()
}

#' Create the chart for r1, c1
#'
#' @import ggplot2
#' @import svglite
#' @return the plot
#' @export
chart_r1c1 <- function() {
  svglite("chart_r1c1.svg")
  plot <- ggplot(data=YearData, aes(x=yearID)) +
    geom_line(aes(y=Avg, color="Average")) +
    geom_line(aes(y=KRate, color="Strikeout Rate")) +
    xlab("Year") + getTheme() +
    theme(axis.title.y = element_blank(), legend.title=element_blank())
  print(plot)
  dev.off()
  invisible()
}
#' Create the chart for r1, c2
#'
#' @import ggplot2
#' @import dplyr
#' @import svglite
#' @return the plot
#' @export
chart_r1c2 <- function(league) {
  al <- c("DET","HOU","KCA","NYA","OAK","SEA","TBA","ANA","BAL","CHA","CLE","TEX","BOS","MIN","TOR")
  nl <- c("CHN","ARI","CIN","LAN","MIA","MIL","PHI","WAS","SDN","ATL","COL","NYN","PIT","SFN","SLN")
  leagueChoice <- get(league)
  df <- HeadToHeadResults %>%
    filter(team %in% leagueChoice) %>%
    filter(opp %in% leagueChoice)
  svglite("chart_r1c2.svg")
  print(ggplot(data=df, aes(x=team, y=opp)) + geom_tile(aes(fill=Avg)) +
    scale_fill_gradient(low = "red", high = "green") + xlab("Team") + ylab("Opponent") + getTheme())
  dev.off()
  invisible()
}
#' Create the chart for r1, c3
#'
#' @import ggplot2
#' @return the plot
#' @import svglite
#' @import ggrepel
#' @export
chart_r1c3 <- function() {
  svglite("chart_r1c3.svg")
  print(ggplot(data=TeamData2015, aes(x=Wins, y=RunDiff)) + geom_point() + geom_text_repel(aes(label=team), nudge_x=1) +
          xlab("Season Wins") + ylab("Total Run Differential") + getTheme())
  dev.off()
  invisible()
}
#' Create the chart for r2, c1
#'
#' @import ggplot2
#' @return the plot
#' @import svglite
#' @export
chart_r2c1 <- function() {
  wins <- GameData2015 %>% filter(win==1) %>% mutate(TotalRuns=Runs+RunsAllowed)
  svglite("chart_r2c1.svg")
  print(ggplot(data=wins, aes(x=TotalRuns)) + geom_bar() + xlab("Total Runs Scored") + ylab("# of games") + getTheme())
  dev.off()
  invisible()
}
#' Create the chart for r2, c2
#'
#' @import ggplot2
#' @return the plot
#' @import svglite
#' @import tidyr
#' @import dplyr
#' @export
chart_r2c2 <- function() {
  ps2 <- PlayerSeason %>%
    select(label, HR, HR95p) %>%
    head(25) %>% mutate(HR=HR-HR95p) %>%
    gather(key=var, value=value, HR, HR95p) %>%
    arrange(desc(var))
  svglite("chart_r2c2.svg")
  print(ggplot(data=ps2, aes(x=reorder(label, -value), y=value, fill=var)) + geom_bar(stat="identity") +
          xlab("Player/Season") + ylab("# of Home Runs") + scale_fill_discrete(labels=c("Player HR", "95th %ile HR")) + getTheme() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title=element_blank()))
  dev.off()
  invisible()
}
