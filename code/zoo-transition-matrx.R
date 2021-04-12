

p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()

states = c("A", "B", "C", "D", "E", "F")
num_states = length(states)
df <- data.table(
  t0 = rep(states, num_states)
  t1 = rep(states, each = num_states)
  probability = c(
    0, 0.7, 
    
  )
)





ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z), colour = "grey50")


pimage <- axis_canvas(p, axis = 'y') + 
  draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/295px-Iris_virginica.jpg", y = 2.5, scale = 0.5) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/320px-Iris_versicolor_3.jpg", y = 1.5, scale = 0.5) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/450px-Kosaciec_szczecinkowaty_Iris_setosa.jpg", y = 0.5, scale = 0.5)


# insert the image strip into the plot
ggdraw(insert_yaxis_grob(p, pimage, position = "left"))
