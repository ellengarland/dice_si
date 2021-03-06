## Here, we used it as a measure of phrase sharing:
## SI =  2A / (B + C)
## where SI is the similarity in song phrases between population pairs, A  is the number of shared phrases, B  is the
## total number of phrases present in population 1, and C is the total number of phrases present in population 2.

# Example invocation: dice("~/Downloads/GAMA_2Jul_PA_Dice.txt")
dice <- function(filename, format="matrix")
{
    if (format == "lsi_strings")
    {
        # Maximum of 100 columns supported. read.csv will wrap columns if the first 5 lines are not representative
        raw <- read.csv(filename, header=FALSE, sep=",", strip.white=TRUE, col.names=paste0("V",seq_len(100)))
        # Determine the unique themese
        themes <- na.exclude(unique(c(as.matrix(raw[-1][-1][-1][-1]))))
        # Omit empty strings
        themes <- themes[themes != ""]
        # This is the source data, having removed the first, second and fourth column
        source_data <- as.matrix(raw[-1][-1][-2])
        row_count <- dim(source_data)[1]
        data <- matrix(0, ncol=length(themes), nrow=row_count)
        colnames(data) <- themes
        rownames(data) <- source_data[,1]
        for (i in 1:row_count)
        {
            for (j in 1:length(themes))
            {
                if (themes[j] %in% source_data[i,])
                {
                    data[i,j] = 1;
                }        
            }
        }
        data <- data.frame(data)
    }
    else
    {
        data <- read.table(filename, header=TRUE)
    }
    song_count = nrow(data)
    output <- matrix(data=NA, nrow=song_count, ncol=song_count)
    rownames(output) <- rownames(data)
    colnames(output) <- rownames(data)
    for (i in 1:song_count)
    {
        for (j in 1:song_count)
        {
            ## A is the count of columns for which data[left] = 1 and data[right] = 1
            A <- sum(data[i,] & data[j,])
            ## B is the sum of data[i]
            B <- sum(data[i,])
            ## C is the sum of data[j,]
            C <- sum(data[j,])
            output[i,j] = 2*A / (B + C)
        }
    }
    return(output)
}


cluster <- function(dice_matrix, method)
{
    plot(as.dendrogram(hclust(as.dist(1-dice_matrix), method=method)))

}

bootstrap <- function(dice_matrix, method)
{
    s<-pvclust(dice_matrix, method.dist="euclidean", method.hclust=method, nboot=1000)
    plot(s)
    pvrect(s, alpha=0.95)
    dev.new()
    seplot(s)
    return(s)
}
