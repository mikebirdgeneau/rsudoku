library(sudoku)
library(ggplot2)

ggSudoku <- function(seed,blanks=36){
  require(ggplot2)
  require(sudoku)
  
  set.seed(seed = seed)
  s <- generateSudoku(Nblank=blanks,print.it = T)
  solve_test <- solveSudoku(s,verbose=T)
  
  
  sm <<- data.frame(value = as.character(ifelse(s>0,s,"")),
                   x=rep(seq(1,9,by=1),times=9),
                   y = unlist(lapply(seq(1,9,by=1),function(x){as.integer(rep(x,times=9))})))
  
  
  p <- ggplot(sm,aes(x=y,y=-x))+
    geom_tile(fill="white",colour="black")+
    geom_text(aes(label=value),size=8)+
    geom_hline(yintercept=-0.5)+
    geom_hline(yintercept=-3.5)+
    geom_hline(yintercept=-6.5)+
    geom_hline(yintercept=-9.5)+
    geom_vline(xintercept=0.5)+
    geom_vline(xintercept=3.5)+
    geom_vline(xintercept=6.5)+
    geom_vline(xintercept=9.5)+
    theme_bw()+coord_fixed(xlim = c(0.5,9.5),ylim = c(-0.5,-9.5),expand = F)+
    theme(axis.title = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())+
    ggtitle(paste0("Sudoku #", seed),subtitle = paste0("Difficulty=",round(blanks/81,2)))
  
  dir.create("output",showWarnings = F)
  ggsave(filename = paste0("output/soduku_",seed,"_diff_",round(blanks/81,2),".pdf"),plot = p,width = 8.5,height=11)
  
  return(p)
}

ggSudoku(1)

lapply(seq(1,10),ggSudoku)

