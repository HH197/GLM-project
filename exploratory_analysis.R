
library(readr)

car_dat <- read_csv("C:/Users/Hamid/Desktop/price_data/CarPrice_Assignment.csv")
test_string<-toString(car_dat$CarName[1])
strsplit(test_string,"-")
test_string2<-toString(car_dat$CarName[4])
test_vec<-c(strsplit(test_string2," "))
test_car_comp<-c(rep(NA,nrow(car_dat)))
test_car_list<-vector(mode = "list", length = nrow(car_dat))
for(i in 1:nrow(car_dat)){
  test_string<-toString(car_dat$CarName[i])
  test_car_list[[i]]<-c(strsplit(test_string," "))
  test_car_pre<-unlist(test_car_list[[i]][1])
  test_car_comp[i]<-test_car_pre[1]
}
#change the typo
car_comp_final<-c(rep(NA,nrow(car_dat)))
for(i in 1:nrow(car_dat)){
  if(test_car_comp[i]=="maxda"){
    car_comp_final[i]<-"mazda"
  }
  else if(test_car_comp[i]=="Nissan"){
    car_comp_final[i]<-"nissan"
  }
  else if(test_car_comp[i]=="porcshce"){
    car_comp_final[i]<-"toyota"
  }
  else if(test_car_comp[i]=="vokswagen"){
    car_comp_final[i]<-"volkswagen"
  }
  else if(test_car_comp[i]=="vw"){
    car_comp_final[i]<-"volkswagen"
  }
  else if(test_car_comp[i]=="toyouta"){
    car_comp_final[i]<-"toyota"
  }
  else{
    car_comp_final[i]<-test_car_comp[i]
  }
}
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
car_comp_final<-firstup(car_comp_final)
car_dat$car_company<-car_comp_final
hist(car_dat$price)
#can use exponential regression or log linear model
#remove the car id and car name because they are not useful in analysis
#car name overlaps with car company variables
car_dat2<-car_dat[,-c(1,3)]
#extract character variables
character_var<-car_dat2[, sapply(car_dat2, class) == 'character']
character_var[sapply(character_var, is.character)] <- lapply(character_var[sapply(character_var, is.character)],
                                                             as.factor)
#extract non-character variables
no_chara<-car_dat2[, sapply(car_dat2, class) != 'character']
#summary(no_chara)
car_dat3<-cbind(character_var,no_chara)
#exponential model
expo_model_price<-glm(price~., data= car_dat3[, -c(6,7,9)],family= Gamma(link = "log"))
(sum_mod_expo<-summary(expo_model_price,dispersion=1))


library(ggplot2)


## distribution plots
p1 <- ggplot(car_dat3) + aes(x =price) + 
  geom_histogram(aes(y=..density..), size=1,color="blue", fill="white", binwidth = 2000)+ 
  geom_density(alpha=.05, fill="red", size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(line = element_line(size = 0.5)) 

p2 <- ggplot(car_dat3) + aes(x = log(price)) + 
  geom_histogram(aes(y=..density..), size=1,color="blue", fill="white")+ 
  geom_density(alpha=.05, fill="red", size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(line = element_line(size = 0.5)) 

p <- 
  ggpubr::ggarrange(p1, p2, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)
p
ggsave('C:/Users/Hamid/Desktop/car_price_dist.jpeg', p, width = 7, height = 8)

## Box plots
ggplot(car_dat3, aes(x=car_company, y=price, fill=fueltype)) +
  geom_boxplot() + 
  xlab('Company') + ylab('Price')+
  scale_fill_manual(values=c("#cc0099", "#3399ff"), 
                    name = "Fuel Type", 
                    labels = c("Diesel", "Gas"))+
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))


library("dplyr")
ordered_names <- car_dat3 %>% group_by(car_company) %>% summarize(count=n()) %>% arrange(-count)

ordered_names <- ordered_names$car_company

ggplot(car_dat3) + aes(x =car_company) + 
  geom_bar(size=0.5,color="black", fill="#4d79ff")+ 
  xlab('Company') + ylab('Number of samples')+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits=ordered_names)+
  theme_classic() +
  theme(line = element_line(size = 0.5),
        axis.text.x = element_text(angle = 45,vjust = 0.7))


car_dat3$cylindernumber <- factor(car_dat3$cylindernumber, levels=c('two', 'three', 'four', 'five',  'six', 'eight', 'twelve'),
                                  labels=c('Two', 'Three', 'Four', 'Five',  'Six', 'Eight', 'Twelve'))

ggplot(car_dat3) +
  aes(x = enginesize, y = price, color = cylindernumber) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", alpha = 0.2) + 
  xlab('Engine size') + ylab('Price')+
  scale_colour_brewer(palette = "Set1", name = "Cylinder Number")+
  theme_classic()

res <- car_dat3 %>% mutate(category=cut(wheelbase, breaks=4, labels=c("1","2","3", "4")))

p <- 
  ggplot(res) +
  aes(x = category, y = price) +
  geom_boxplot(color = "#3399ff") + stat_summary(fun=median, geom="line", aes(group=1), color = "#cc0099")  + 
  stat_summary(fun=median, geom="point", color = "#cc0099")+ ggtitle('The square effect of wheel base on price')+
  xlab('Wheel base groups') + ylab('Price')+
  theme_classic()
p
ggsave('C:/Users/Hamid/Desktop/wheel base groups.jpeg', p, width = 4, height = 5)
##### Heart disease ###### 
heart_disease <- read_csv("C:/Users/Hamid/Desktop/heart.csv")

heart_disease$HeartDisease <- factor(heart_disease$HeartDisease, levels = c(0, 1), labels = c('No', 'Yes'))
heart_disease$Sex <- factor(heart_disease$Sex, levels = c('F', 'M'), labels = c('Female', 'Male'))




df <- heart_disease %>% group_by(HeartDisease, ChestPainType, Sex) %>% count()
p_chest_pain <- 
  ggplot(df, aes(y = n, x=ChestPainType, fill=HeartDisease)) + facet_grid(.~Sex)+
  geom_bar(stat = 'identity') +
  xlab('Chest pain type') + ylab('Count')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Heart Disease")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))


p<- 
  ggplot(heart_disease, aes(x=Age, fill = HeartDisease))+ facet_grid(.~Sex)+
  geom_bar() +
  xlab('Age') + ylab('Count')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Heart Disease")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

p
ggsave('C:/Users/Hamid/Desktop/age_heart_sex.jpeg', p, width = 8, height = 5)

##Box plot cholestrol
p <- ggplot(heart_disease, aes(y = Cholesterol, x=HeartDisease))+
  geom_boxplot(colour = "#cc0099") +
  xlab('Heart Disease') + ylab('Cholesterol')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Heart Disease")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

p
ggsave('C:/Users/Hamid/Desktop/cholestrol.jpeg', p, width = 4, height = 5)
### exercise
df <- heart_disease %>% group_by(HeartDisease, ExerciseAngina, Sex) %>% count()
ex_ang_plot <- 
  ggplot(df, aes(y = n, x=ExerciseAngina, fill=HeartDisease)) + facet_grid(.~Sex)+
  geom_bar(stat = 'identity') +
  xlab('Exercise Angina') + ylab('Count')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Heart Disease")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))


p <-
  ggpubr::ggarrange(p_chest_pain, ex_ang_plot, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)



p
ggsave('C:/Users/Hamid/Desktop/chest_pain_exercise_angina.jpeg', p, width = 8, height = 8)
## 
p <- 
  ggplot(heart_disease, aes(y = Oldpeak, x=HeartDisease))+
  geom_boxplot(colour = "#cc0099") +
  xlab('Heart Disease') + ylab('Old peak')+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

p
ggsave('C:/Users/Hamid/Desktop/old_peak.jpeg', p, width = 5, height = 5)
