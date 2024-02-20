## SECTION 1. Introduction

# Figure 1
top("ham$", c("GB", "IE"))


## SECTION 2. The GeoNames data

# Figure 2 and info on GeoNames
library(RColorBrewer)
# The following requires that the working directory is set to the folder containing
# the file allCountries.txt downloaded 
# from https://download.geonames.org/export/dump/ and unzipped
x <- read.table(file="allCountries.txt", header=FALSE, sep="\t", quote="", 
                na.strings="", comment.char="")
colnames(x) <- c("geonameid", "name", "asciiname", "alternatenames", 
                 "latitude", "longitude", "feature_class", "feature_code", 
                 "country_code", "cc2", "admin1_code", "admin2_code", 
                 "admin3_code", "admin4_code", "population", "elevation", "dem", 
                 "timezone", "modification_date")
# check the number of entries
nrow(x)
# check how many countries are represented
unique(x$country_code)  # 253 in all, not counting one which is NA
# check how many feature classes are represented
unique(x$feature_class)  # 9 in all, not counting one which is NA
fc <- x$feature_class
nass <- which(is.na(fc))
fc <- fc[-nass]
tfc <- table(fc)
tfc <- sort(tfc, decreasing=TRUE)
tfc <- tfc/1000000
mycol <- brewer.pal(9, "Set1") 
# Figure 2
barplot(tfc, col=mycol, ylim=c(0,5), ylab="Number of entries (million)", 
        xlab="Categories")
# check how many NAs there are in the different columns
count_nas <- function(z) length(which(is.na(z)))
nas <- apply(x, 2, count_nas)
# get rounded percentages of non-NA entries in different categories
round(100 - 100*nas/nrow(x),2)
# the dem column does not have NA, but instead a value -9999
# get the percentage of dem entries not equal to -9999
100 - round(100 * length(x$dem[x$dem < -5000])/nrow(x), 2)
# get the percentage of population entries not equal to 0
100 - round(100 * (nrow(x) - length(x$population[x$population == 0]))
      / nrow(x), 2)
# get the percentage of Russian entries which have NA cells in the 
# alternatenames column
round(100 * length(which(x$country_code=="RU" & is.na(x$alternatenames)))
      / length(which(x$country_code=="RU")),1)
# get the percentage of Chinese entries which have NA cells in the 
# alternatenames column
round(100 * length(which(x$country_code=="CN" & is.na(x$alternatenames)))
      / length(which(x$country_code=="CN")),1)
# correlate the dem and elevation data
x_redux <- x[x$dem > -5000,]  # exclude NA values encoded as dem = -9999
cor.test(x_redux$dem, x_redux$elevation)


## SECTION 3. A practical introduction to the R package toponym

# Installation
install.packages("devtools")  # installs package “devtools”
library ("devtools")  # loads package “devtools”
install_github("Lennart05/toponym")  # installs package “toponym”
library("toponym")  # loads package “toponym”

# Create a simple map
country("DE")
country("country table")
help(regex)  # to get an introduction to regular expressions
top("ham$", c("GB", "IE"))  # creates Figure 1
# verify that the name Seaham but not Walthamstow are among the hits
sort(data_ham$name)
top("ham", c("GB", "IE"))  # remove the dollar sign
# verify that both of the names Seaham and Walthamstow are among the hits
grep("Seaham", data_ham$name)
grep("Walthamstow", data_ham$name)
top("ham$", "IE")  # trailing string -ham in Ireland only

# More on the top() function
top(countries=c("GB", "IE"), strings="ham$")  # other order of parameters
help(top)  # get info on the top() function
top("ham$", c("GB", "IE"), color="blue", regions=1, csv=TRUE)
top(strings="^Влад", countries="RU")
top(strings="^Vlad", countries="RU", column="alternatenames")

# Finding frequent toponym strings
topFreq(countries="US", len=5, limit=24, type="$", feat.class="H")
topFreq(countries="BE", len=5, type="^", limit=8, polygon=flanders_polygon)

# Defining a polygon
p <- createPolygon("ID")  # this will require defining a polygon through clicks
country("MX", regions = 1)
Chi <- createPolygon(countries="MX", region_name="Chihuahua", retrieve=TRUE)

# Strings specific to a region
topComp(countries="GB", len=2, rat=.7, limit=100, polygon=danelaw_polygon)
topCompOut(countries="GB", len=2, rat=.7, limit=100, polygon=danelaw_polygon)
# exloring cases of -thorpe
length(data_pe$name)
length(grep("thorpe", data_pe$name))
thorpe <- data_pe[grep("thorpe", data_pe$name),]
mapper(thorpe)
topComp(countries="GB", limit=100, len=5, rat=.5, polygon=danelaw_polygon)
topZtest(strings="stead$", countries="GB",polygon=danelaw_polygon)

# Other functions
getData(countries=c("DE", "GB"))  # example of updating downloaded data
# examples of lists of symbols and their frequencies, here for Thailand
ortho(countries="TH", column="name")  # 
ortho(countries="TH", column="alternatenames")  # 

## SECTION 4. Case study 1: tracking the former extension of Xincan languages
# Mapping names mentioned in Thompson (1970: 98-99)
thompson=c("Toquegua", "Motagua", "Managua", "Tigua", "Chanmagua", "Chaumagua", 
           "Jagua", "Pasasagua", "Anchagua", "Cocuyagua", "Jacagua", "Jalagua", 
           "Chilistagua", "Comayagua", "Sasagua", "Chasnigua", "Ulua", 
           "Silisgualagua", "Manzaragua", "Masahua", "Mulacagua", "Tircagua", 
           "Chumbagua", "Chapulistagua", "Xagua", "Xelegua", "Eraxagua", 
           "Moncagua", "Teconalistagua", "Laxigua", "Talgua", "Colomoncagua", 
           "Tiscagua", "Apacilagua", "Conchagua", "Masaguara", "Sicaguara", 
           "Yaguacire", "Yamaguare", "Comasahua", "Atepammasagua", "Quixnagua", 
           "Masahua", "Moncagua", "Aguasarca", "Guahtajigua", "Nicaragua", 
           "Managua", "Veragua")
top(strings=thompson, countries=c("GT", "HN", "SV", "NI"), 
    feat.class=c("P", "H"), name="agua")
# It was verified that including all feature classes will not produce more 
# matches:
top(strings=thompson, countries=c("GT", "HN", "SV", "NI"), 
    feat.class=c("P", "S", "H", "T", "A", "L", "R", "V", "U"), name="agua_all_feats")
length(unique(agua_all_feats$group))  # 29
length(unique(agua$group))  # 29

# Figure 3
mapper(mapdata=agua, title="Names in -(a)gua/-ahua from Thompson")

# Figure 4
top(strings=c("agua$", "gua$", "ua$", "ahua$", "guara$",
              "guacire$", "guare$"), countries=c("GT", "HN", "SV", "NI"),
    feat.class=c("P", "H"), name="generic_agua")
nrow(generic_agua)  # 477
# inspect the results
generic_agua$name
# filtering manually
# the following are out for containing agua
# 1:4,7:11,15:34,37,49:51,53:62,64:72,74:79,82:89,92:96,98,100:102,111:120,134,163,
# 166:173,177,179:203,285:298,343:352,355,258:359,361,364,372,374,381:382,389:392,
# 394:407,419,429:432,434:436,438,440:444,450,454,470:472,475
# the following contain one of the search strings in other Spanish words
# 90,97,104,125,159,211,240,301,465
# the following contain Jagua
# 41,91,242:256,265:276,284,363,415:416,460
out <- c(1:4,7:11,15:34,37,49:51,53:62,64:72,74:79,82:89,92:96,98,100:102,111:120,
         134,163,166:173,177,179:203,285:298,343:352,355,258:359,361,364,372,374,
         381:382,389:392,394:407,419,429:432,434:436,438,440:444,450,454,470:472,
         475,90,97,104,125,159,211,240,301,465,41,91,242:256,265:276,284,363,415:416,460)
generic_agua_filtered <- generic_agua[-out,]
nrow(generic_agua_filtered)
mapper(mapdata=generic_agua_filtered, title="Names in -gua, -agua, etc.")

# Figure 5
campbell_names <- c("Ayampuc", "Ayarza", "Ipala", "Sanarate", "Sanjaje", 
                    "Sansare", "Sansur", "Sansirisay", "Sanguayaba", "Sansuque", 
                    "Sanyoyo", "Sansupo", "Sanjomo", "Sansurutate", 
                    "Sampaquisoy", "Sancash", "Sashico", "Alzatate", 
                    "Tatasirire", "Maraxcó", "Arloroma", "Shusho Arriba", 
                    "Shusho Abajo", "Güishoro")
top(strings=campbell_names, countries=c("GT", "HN", "SV", "NI"), 
    feat.class=c("P", "H", "T", "A", "L", "R", "V", "U"), 
    name="campbell_names")
# check what the dot is to the west
western_dot <- which(campbell_names$longitude < -91)
campbell_names$name[western_dot]
# exclude it
cambpell_names_redux <- campbell_names[-western_dot,]
mapper(mapdata=cambpell_names_redux, title="Names from Campbell")

# Figure 6
campbell_agua <- c("Pasasagua", "Jagua", "Sasagua", "Xagua", "Eraxagua", 
                   "Conchagua", "Comasahua", "Manzaragua", "Anshagua", 
                   "Anshigua", "Xororagua")
top(strings=campbell_agua, countries=c("GT", "HN", "SV", "NI"), 
    feat.class=c("P", "H", "T", "A", "L", "R", "V", "U"), 
    name="campbell_agua")
mapper(mapdata=campbell_agua, title="Names in -(a)gua/-ahua from Campbell")


## SECTION 5. Case study 2: Slavic toponyms in Germany
# Figure 7
top("itz$", "DE", regions=1)

# Figure 8
top("ow$", "DE", colors="blue", regions=1)

# Figure 9
# finding all names in -in
top(strings="in$", countries="DE", name="insuffix")
# inspecting them
insuffix$name
# identifying some to be excluded
ein <- grep("ein$", insuffix$name)
ain <- grep("ain$", insuffix$name)
sankt <- grep("Sankt", insuffix$name)
krin <- grep("Krin", insuffix$name)
del <- unique(c(ein, ain, sankt, krin))
insuffix <- insuffix[-del,]
# getting data for the heading, which will not be produced automatically
# since the filtering requires us to use mapper(), not top()
nrow(insuffix)  # 585
mapper(insuffix, regions=1, colors="green", title="in$ 585")

# Figure 10
p <- createPolygon(countries="DE", 
                   region_name=c("Mecklenburg-Vorpommern", "Niedersachsen", 
                                 "Sachsen-Anhalt", "Brandenburg", "Sachsen"), 
                   retrieve=TRUE)
top(strings=c("Deutsch", "Wendisch"), countries="DE", polygon=p, regions=1, 
    color=c("purple", "orange"))

# Figure 11
# the data frames that hols the most frequent suffixes are 
# data_itz, data_ow, insuffix;
# to this we add data_Wendisch
top("Wendisch", "DE")
# now all are combined
combined <- rbind(data_itz, data_ow, insuffix, data_Wendisch)
mapper(combined, color=c("red", "blue", "green", "purple"), regions=1)

# Figure 12
top(strings=c("[Pp]fuhl$", "[Ff]ließ$", "[Ll]uch$"), countries="DE", regions=1, 
    feat.class="H")
