SalaryAnalyzer<- function(year) {
  packageLoaded <- function(pkg.name) {
    return(0 != length(grep(paste("^package:", 
                                  pkg.name, "$", sep=""), search())))
  }
  
  if (!packageLoaded("Lahman")) {
    library(Lahman)
  }
  data(Salaries)
  
  sa = list(year=year,
            data=subset(Salaries, yearID==year))
  class(sa) <- "SalaryAnalyzer"
  return(sa)
}

plot.SalaryAnalyzer <- function(sa, team="all") {
  # plot median salaries of each team, or salaries of a players in each team.
  par(family="mono")
  opar <- par(no.readonly=T)
  par(opar)
  if (team == "all") {
    tab <- table(sa$data$teamID)
    teamID.dropnon <- factor(sa$data$teamID)
    levels(teamID.dropnon) <- names(tab)[tab > 0]
    
    med.sal = tapply(sa$data$salary, teamID.dropnon, median)
    barplot(med.sal/10000, las=1, cex.names = 0.7, horiz=T,
            ylab="Teams", xlab="Salaries (10,000 USD)", main=c("median salaries of teams", sa$year))
  } else {
    sal.tex.df <- subset(sa$data, teamID==team)
    sal.tex <- sal.tex.df$salary
    names(sal.tex) <- sal.tex.df$playerID
    
    par(mar = c(6.5, 6.5, 3, 3), mgp = c(5, 1, 0))
    barplot(sal.tex/10000, horiz=T, las=1, cex.names=0.7,
            xlab="Salaries (10,000 USD)", ylab="Players", 
            main=c("Salaries of players", team))
  }
}

get.salary.SalaryAnalyzer <- function(sa, query, of.team=T, of.player=!of.team) {
  # returns median salary of team, or salary of a player.
  if (of.team) {
    byteam <- subset(sa$data, teamID==query)
    salary <- median(byteam$salary)
    names(salary) <- paste(query, "_", sa$year, sep="")
  } 
  if (of.player) {
    byplayer <- subset(sa$data, playerID==query)
    salary <- byplayer$salary
    names(salary) <- paste(query, "_", sa$year, sep="")
  }
  
  return(salary)
}

plot <- function(SalaryAnalyzer, team="all") {
  UseMethod("plot", SalaryAnalyzer)
}

get.salary <- function(SalaryAnalyzer, query, of.team=T, of.player=!of.team) {
  UseMethod("get.salary", SalaryAnalyzer)
}

