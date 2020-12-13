library(tidyverse)
library(ambient)
library(particles)
library(tidygraph)
library(ggforce)
library(data.table)

# # https://github.com/aschinchon/general-2D-map/blob/master/general_2d_map.R
# # https://www.williamrchase.com/post/2019/02/28/strange-attractors-12-months-of-art-february/
# # https://github.com/marcusvolz/mathart
# 
# grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
#   mutate(noise = gen_simplex(x, y))
# 
# curl <- curl_noise(gen_perlin, x = grid$x, y = grid$y)
# 
# grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)
# 
# field <- as.matrix(grid, x, value = angle)
# 
# sim <- create_empty(1000) %>%
#   simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>%
#   wield(reset_force, xvel = 0, yvel = 0) %>%
#   wield(field_force, angle = field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>%
#   evolve(100, record)
# 
# traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
# names(traces) <- c('x', 'y')
# traces$particle <- rep(1:1000, 100)
# 
# #triangles
# ggplot(traces[1:100, ]) +
#   geom_regon(aes(x0 = x, y0 = y, r = 1, angle = runif(100) * 180, sides = 3), size = 0.1, alpha = 0.1) +
#   theme_void() +
#   theme(legend.position = 'none') 
# 
# # sandy effect
# seed <- sample(1:2000, 1)
# grid <-
#   long_grid(x = seq(0, 10, length.out = 1000),
#             y = seq(0, 10, length.out = 1000)) %>%
#   mutate(
#     x1 = x + gen_perlin(x = x, y = y, frequency = 2, seed = seed),  # tweak noise
#     y1 = y + gen_perlin(x = x, y = y, frequency = 0.5, seed = seed)
#   )
# 
# curl <- curl_noise(gen_cubic, seed = seed, x = grid$x1, y = grid$y1)  # change the gen_
# 
# grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)  # tweak
# 
# field <- as.matrix(grid, x, value = angle)
# 
# sim <- create_ring(10000) %>%
#   simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>%  # tweak velocity
#   wield(reset_force, xvel = 0, yvel = 0) %>%
#   wield(field_force, angle = field, vel = 0.15, xlim = c(-50, 40), ylim = c(-50, 40)) %>%
#   evolve(100, record)
# 
# traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
# names(traces) <- c('x', 'y')
# traces$particle <- rep(1:10000, 100)
# 
# bl_yl <- c('#C29F5B', '#C26F5B')
# bl_yl_bg <- '#EEEEEE'
# 
# traces2 <-
#   traces %>%
#   group_by(particle) %>%
#   mutate(color = sample(bl_yl, 1, replace = TRUE))
# 
# # flow trace 1 ------------------------------------------------------------
# flow_trace1 <- 
# ggplot(traces2[1:20000, ]) +
#   geom_path(aes(x, y, group = particle, color = color), size = 0.035, alpha = 0.6) +
#   scale_color_identity(guide = "none") +
#   theme_void() +
#   theme(legend.position = 'none', panel.background = element_rect(fill = "#3A6E78")) 
# ggsave("flow_trace1.png", flow_trace, width = 14, height = 12, dpi = "retina")


# flow trace 2 ------------------------------------------------------------

# seed <- sample(1:2000, 1)
# grid <-
#   long_grid(x = seq(0, 10, length.out = 1000),
#             y = seq(0, 10, length.out = 1000)) %>%
#   mutate(
#     x1 = x + gen_perlin(x = x, y = y, frequency = 2, seed = seed),  # tweak noise
#     y1 = y + gen_perlin(x = x, y = y, frequency = 0.5, seed = seed)
#   )
# 
# curl <- curl_noise(gen_cubic, seed = seed, x = grid$x1, y = grid$y1)  # change the gen_
# 
# grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)  # tweak
# 
# field <- as.matrix(grid, x, value = angle)
# 
# sim <- create_ring(10000) %>%
#   simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>%  # tweak velocity
#   wield(reset_force, xvel = 0, yvel = 0) %>%
#   wield(field_force, angle = field, vel = 0.15, xlim = c(-50, 40), ylim = c(-50, 40)) %>%
#   evolve(100, record)
# 
# traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
# names(traces) <- c('x', 'y')
# traces$particle <- 1:100
# 
# bl_yl <- c('#C29F5B', '#C26F5B')
# bl_yl_bg <- '#EEEEEE'
# 
# traces2 <-
#   traces %>%
#   group_by(particle) %>%
#   mutate(color = sample(bl_yl, 1, replace = TRUE))
# 
# flow_trace1 <- 
#   ggplot(traces2) +
#   geom_path(aes(x, y, group = particle, color = color), size = 0.035, lineend = "round", alpha = .1) +
#   scale_color_identity(guide = "none") +
#   theme_void() +
#   theme(legend.position = 'none', panel.background = element_rect(fill = alpha("#3A6E78", .4))) 
# ggsave("flow_trace2.png", width = 14, height = 12, dpi = "retina")


# flow trace 3 ------------------------------------------------------------
# seed <- sample(1:2000, 1)
# grid <-
#   long_grid(x = seq(0, 10, length.out = 1000),
#             y = seq(0, 10, length.out = 1000)) %>%
#   mutate(
#     x1 = x + gen_perlin(x = -x, y = y, frequency = 2),  # tweak noise
#     y1 = y + gen_perlin(x = x, y = y, frequency = 0.5)
#   )
# 
# curl <- curl_noise(gen_worley, seed = 1, x = grid$x1, y = grid$y1)  # change the gen_
# 
# grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)  # tweak
# 
# field <- as.matrix(grid, x, value = angle)
# 
# sim <- create_ring(10000) %>%
#   simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>%  
#   wield(reset_force, xvel = 0, yvel = 0) %>%
#   wield(field_force, angle = field, vel = 0.15, xlim = c(-50, 40), ylim = c(-50, 40)) %>%  # tweak velocity
#   evolve(100, record)
# 
# traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
# names(traces) <- c('x', 'y')
# traces$particle <- 1:100
# 
# bl_yl <- hcl.colors(4, "Berlin")
# bl_yl_bg <- '#EEEEEE'
# 
# traces2 <-
#   traces %>%
#   group_by(particle) %>%
#   mutate(color = sample(bl_yl, 1, replace = TRUE))
# 
# flow_trace1 <- 
#   ggplot(traces2) +
#   geom_path(aes(x, y, group = particle, color = color), size = 0.035, lineend = "round", alpha = .5) +
#   scale_color_identity(guide = "none") +
#   theme_void() +
#   theme(legend.position = 'none', panel.background = element_rect(fill = alpha("#3A6E78", .4))) 
# ggsave("flow_trace3.png", width = 14, height = 12, dpi = "retina")


# flow trace 4 ------------------------------------------------------------

seed <- sample(1:2000, 1)
grid <-
  long_grid(x = seq(0, 10, length.out = 1000),
            y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    x1 = x + gen_perlin(x = -x, y = y, frequency = 2),  # tweak noise
    y1 = y + gen_perlin(x = x, y = y, frequency = 0.5)
  )

curl <- curl_noise(gen_perlin, seed = 1, x = grid$x1, y = grid$y1)  # change the gen_

grid$angle <- atan2(curl$y, curl$x)* - 1 - atan2(grid$y, grid$x)  # tweak

field <- as.matrix(grid, x, value = angle)

sim <- create_ring(10000) %>%
  simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>%  
  wield(reset_force, xvel = 0, yvel = 0) %>%
  wield(field_force, angle = field, vel = 0.15, xlim = c(-50, 40), ylim = c(-50, 40)) %>%  # tweak velocity
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- 1:1000

bl_yl <- hcl.colors(4, "Berlin")
bl_yl_bg <- '#EEEEEE'

traces2 <-
  traces %>%
  group_by(particle) %>%
  mutate(color = sample(bl_yl, 1, replace = TRUE))

flow_trace1 <- 
  ggplot(traces2) +
  geom_path(aes(x, y, group = particle, color = color), size = 0.035, lineend = "round", alpha = .5) +
  scale_color_identity(guide = "none") +
  theme_void() +
  theme(legend.position = 'none', panel.background = element_rect(fill = alpha("#3A6E78", .4))) 
ggsave("flow_trace4.png", width = 14, height = 12, dpi = "retina")
