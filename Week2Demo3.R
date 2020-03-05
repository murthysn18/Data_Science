vec1 <- c("hockey", "football", "baseball", "curling", 
          "Rugby", "hurling", "basketball", "tennis", 
          "cricket", "lacrosse")
vec2 <- c("hockey", "lacrosse", "hockey", "water polo", "hockey", "lacrosse")
vec3 <- c(vec1, vec2)

vec4 <- vec3[c(1, 3, 6)]

vec4_factor <- as.factor(vec4)
class(vec4)
class(vec4_factor)