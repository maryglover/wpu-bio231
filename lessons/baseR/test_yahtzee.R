# Testing yahtzee exercise

yahtzee <- data.frame(vinny=c(4, 2, 12, 8, 20, 24, 22, 0, 25, 30, 40, 0, 18, 0), mary = c(3, 8, 9, 12, 10, 24, 13, 10, 25, 30, 40, 50, 21, 0))

row.names(yahtzee) <- c('ones', 'twos', 'threes', 'fours', 'fives', 'sixes', 'threeKind', 'fourKind', 'full_house', 'small_straight', 'large_straight', 'yahtzee', 'chance', 'yahtzee_bonus')


top <- apply(yahtzee[1:6,], 2, sum)
top_bonus <- top + 35
bottom <- apply(yahtzee[7:14,],2,  sum)

max(top + bottom)

total <- top + bottom
which.max(total)

