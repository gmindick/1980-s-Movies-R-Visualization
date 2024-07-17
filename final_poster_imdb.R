#Gregory Indick 
#Final Poster

#Loads in my libraries and fonts
library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(treemap)
library(extrafont)
library(ggrepel)


#Stores all of my data sets into vectors
movie_titles <- read_tsv('title.basics.tsv')
movie_crew <- read_tsv('title.crew.tsv')
movie_ratings <- read_tsv('title.ratings.tsv')
movie_crew_names <- read_tsv('name.basics (1).tsv')

#Views my datasets
View(movie_titles)
View(movie_crew)
View(movie_crew_names)
View(movie_ratings)

#Shows structure of datasets
str(movie_titles) 

#Grabs movies that premiered between 1980 and 1990
movie_titles_new <- filter(movie_titles, startYear >=1980 & startYear<=1989)
#Gets rid of tv shows and only keeps movies
movie_titles_new <- filter(movie_titles_new, titleType=="movie" | titleType=="tvMovie")
#Removed Adult movies from the data set
movie_titles_new <- filter(movie_titles_new, isAdult=="0")
#Removed columns that weren't needed
movie_titles_new <- select(movie_titles_new, -c(endYear,isAdult))

#Joined the movie title columns with the director name columns and removed some more columns
movie_titles_joined <- movie_titles_new %>% inner_join(movie_ratings)
movie_crew_joined <- movie_crew %>% inner_join(movie_crew_names,by=c("directors"="nconst"))
movies <- movie_crew_joined %>% inner_join(movie_titles_joined)
movies <- select(movies,-c(writers,knownForTitles))
movies <- movies %>% 
  rename("releaseYear" = "startYear")
movies <- arrange(movies,releaseYear)

#This separates both profession and genre into new columns so that they are 
#more easily filtered.
movies <- movies %>% separate(primaryProfession, c('Profession1', 'Profession2','Profession3'))
movies <- movies %>% separate(genres, c('Genre1', 'Genre2'))


View(movies)

#Average movie ratings across the 1980's
ggplot(movies)+geom_boxplot(aes(averageRating, fill=releaseYear))+
  coord_flip()+scale_fill_brewer(palette="GnBu")+
  labs(title = "Average Movie Ratings Across the 80's") +
  xlab("Average Rating") + 
  ylab("Year Released") + theme_classic()

ggsave("poster_ratingboxplot_gmindick.png")

#Treemap of genres by average rating
treemap(movies,
        index=c("Genre1","releaseYear"), #This time adds a subgroup for grouping
        vSize="averageRating",
        type="index",
        fontsize.labels=c(15,8),  # Changes the size of labels
        fontcolor.labels=c("white","black"), # Changes color of labels
        fontface.labels=c(2,3),  # Changes font of labels
        bg.labels=c("transparent"), # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")), # Decides where to put numbers in rectangles
        overlap.labels=0.5, # number between 0 and 1 that determines the tolerance of the overlap between labels. 
        inflate.labels=F,
        title="Treemap of Average Rating of Genres with Subgroup of Release Year",                     
        fontsize.title=12,
        palette="GnBu"
)

#Stores the palette I want to use
palette <- brewer.pal(10,"Spectral")

#Creates scatterplot of highest rated movies
movies %>%
  filter(averageRating>5,numVotes>500000) %>%
  ggplot()+
  geom_point(aes(numVotes,averageRating,color=releaseYear))+
  geom_label_repel(aes(x=numVotes,y=averageRating,label=originalTitle),size=2.2)+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()+
  scale_color_manual(values=rep(palette,10))+
  labs(title = "Highest Rated Movies of the 1980's")+
  xlab("Number of Votes for Rating") + 
  ylab("Movie Rating")

ggsave("poster_highest_rated_movies_gmindick.png")


#Creates a scatterplot of lowest rated movies
movies %>%
  filter(averageRating<5,numVotes>30000) %>%
  ggplot()+
  geom_point(aes(numVotes,averageRating,color=releaseYear))+
  geom_label_repel(aes(x=numVotes,y=averageRating,label=originalTitle),size=2.2)+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()+
  scale_color_manual(values=rep(palette,10))+
  labs(title = "Lowest Rated Movies of the 1980's")+
  xlab("Number of Votes for Rating") + 
  ylab("Movie Rating")

ggsave("poster_lowest_rated_movies_gmindick.png")

#Shows a count of movies released by year
movies %>%
  count(releaseYear,sort=TRUE)

#Creates a dataframe with movie counts every two years
movie_count <- data.frame (Year  = c("1980-1981", "1982-1983","1984-1985","1986-1987","1988-1989"),
                                 Count = c(4620,4926,4988,5344,5652))
View(movie_count)

#Creates a bar plot based on movie counts
movie_count %>%
  ggplot( aes(x=Year, y=Count)) +
  geom_bar(stat="identity", fill="#74BACF", alpha=.6, width=.4) +
  coord_flip() +
  labs(title = "Amount of Movies Released Between 1980 and 1989")+
  theme_classic()       
       
ggsave("poster_barplot_gmindick.png")

       