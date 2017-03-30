#Function that makes a nice square pichart for displaying proportions
# 1 square = 1 percent. Adopted from the Flowing Data tutorial

squarePie <- function(values, col.grid="#e0e0e0", col.border="black", main="") {

  counts <- table(values) %>%
    as_data_frame() %>%
    filter(n != 0) %>% #factors can sit around and make zeros
    mutate(pcts = 100*n/sum(n)) %>%  #find percentage of whole
    mutate(pcts_round = round(pcts))

  num_unique <- length(counts$pcts)
  #gotta make them add to 100, so we fudge a bit in case.
  counts$pcts_round[num_unique] <- 100 - sum(counts$pcts_round[-num_unique])

  col <- brewer.pal(n = 8, name = "Dark2")[1:num_unique]

  #Making a 10x10 grid so each element is a per-cent
  x_row <- 1:10
  y_col <- 1:10

  # Put together full coordinate vectors
  x <- rep(x_row, 10)
  y <- rep(y_col, each=10)

  # Colors
  fill_col <- c()
  for (i in 1:length(counts$pcts)) {
    fill_col <- c(fill_col, rep(col[i], counts$pcts_round[i]))
  }

  # Plot
  par(mar=c(0, 0, 1, 1))
  # par(xpd=TRUE) # this is usually the default

  plot(0, 0, type="n", xlab="", ylab="", main=main, xlim=c(0,11), ylim=c(0,10.5), asp=1, bty="n", axes=FALSE)
  symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE, bg=fill_col, fg=col.grid, lwd=0.5)
  rect(.5, .5, 10.5, 10.5, lwd=2, border=col.border)
  legend(x = 10.3, y = 10,
         inset=.05,
         cex = 1,
         box.lty=0,
         rev(counts$values),
         fill=rev(col),
         bg="transparent")
}
