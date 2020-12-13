library(contoureR)
library(grid)
library(tidyverse)
library(Rcpp)

cppFunction('DataFrame createTrajectory(int n, double x0, double y0,
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = cos(a*y[i-1])*cos(a*y[i-1])+c*sin(a*x[i-1])*sin(a*x[i-1]);
            y[i] = cos(b*x[i-1])*cos(b*x[i-1])+d*sin(b*y[i-1])*sin(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a  = -2
b = 2
n  = 150
x  = runif(n*n,a,b)
y  = runif(n*n,a,b)

df = data.frame(x,y)

df$z = with(df,-x*y*exp(-x^2-y^2))

df <- createTrajectory(500000, 0, 0, 0.1, 0.1, 0.1, 0.1) %>% 
  mutate(z = -x*y*exp(-x^2-y^2))

df.sub = subset(df,x^2 + y^2 < 2)

df.cnt = getContourLines(df,nlevels=100)
contour_plot <- 
ggplot(data=df.cnt,aes(x,y,group=Group,colour=z)) + 
  scale_color_gradientn(colors = hcl.colors(10, palette = "TealGrn")) +
  geom_path(alpha = 0.8) + 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey20"))

ggsave("contour_plot.png", plot = contour_plot, dpi = "retina")

generateRandomContour <- function() {
  df <- createTrajectory(500000, 0, 0, -0.8, 1.1, 0.5, 0.1) %>% 
    mutate(z = -x*y*exp(-x^2-y^2))
  
}
