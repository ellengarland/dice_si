## Here, we used it as a measure of phrase sharing:
## SI =  2A / (B + C)
## where SI is the similarity in song phrases between population pairs, A  is the number of shared phrases, B  is the
## total number of phrases present in population 1, and C is the total number of phrases present in population 2.

# Example invocation: dice("~/Downloads/GAMA_2Jul_PA_Dice.txt", "MA.02.1", "GA.02.1")
dice <- function(filename, left, right)
{
    data <- read.table(filename, header=TRUE)
    ## A is the count of columns for which data[left] = 1 and data[right] = 1
    A <- sum(data[left,] & data[right,])
    ## B is the sum of data[left]
    B <- sum(data[left,])
    ## C is the sum of data[right]
    C <- sum(data[right,])
    return(2*A / (B+C))
}
