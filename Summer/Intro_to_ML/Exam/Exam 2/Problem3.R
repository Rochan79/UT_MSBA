g = ggplot(green_buildings, aes(x=age))
g + geom_density(aes(fill=factor(green_rating)))+
  labs(x="Age", y='Density', title = 'Distribution of age',
       fill='Green building')
ggplot(green_buildings, aes(class_a, ..count..)) + geom_bar(aes(fill = green_rating), position = "dodge")+
  labs(x="Class a", y='Number of buildings', title = 'Class A vs Green Buildings',
       fill='Green building')
g = ggplot(green_buildings, aes(x=size))
g + geom_density(aes(fill=factor(green_rating)))+
  labs(x="Size", y='Density', title = 'Size distribution',
       fill='Green building')
medians_class_a_gb <- aggregate(Rent ~  class_a, green_buildings, median)
ggplot(data=green_buildings, aes(x=factor(class_a), y=Rent, fill=class_a)) + geom_boxplot()+
  stat_summary(fun=median, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = medians_class_a_gb, aes(label = Rent, y = Rent)) +
  labs(x="Class A", y='Rent', title = 'Rent vs Class a',
       fill='Class A')