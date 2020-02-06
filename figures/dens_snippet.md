```
# wrong_dens
ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) + 
  geom_density(position = "stack") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Accent")

# right_dens
ggplot(mtcars, aes(x = mpg, y = stat(density * n), fill = factor(cyl))) + 
  geom_density(position = "stack") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Accent")

```
