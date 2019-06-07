library(igraph)
library(ggplot2)
# 2.

# a
# create an undirected network with 1000 nodes, where each new node attaches to 1 old nodes
g <- barabasi.game(1000, m = 1, directed = F)
if (is.connected(g)) {
  print("This graph (network) is connected.")
}
plot(main = "Undirected network with 1000 nodes", g, edge.arrow.size = .5, vertex.color = "gold", vertex.size = 6, vertex.frame.color = "gray", vertex.label = "") 

# b
# Result of network community detection 
# modularity measurement
# plot community structure
community <- cluster_fast_greedy(g)
modularity(community)
print(community)
plot(main = "Undirected network with 1000 nodes", g, vertex.color = "gold", mark.groups = groups(community), vertex.size = 4, 
     vertex.frame.color = "gray", vertex.label.cex = 0.8, vertex.label = "")

# c
g_c <- barabasi.game(10000, m = 1, directed = F)
plot(main = "Undirected network with 10000 nodes", g_c, vertex.color = "gold", vertex.size = 4, vertex.frame.color = "gray", vertex.label = "") 

community_c <- cluster_fast_greedy(g_c)
modularity(community_c)
print(community_c)
plot(main = "Undirected network with 10000 nodes", g_c, vertex.color = "gold", mark.groups = groups(community_c), vertex.size = 4, 
     vertex.frame.color = "gray", vertex.label.cex = 0.8, vertex.label = "")

# d
degree_distribution = function(graph, n) {
    log_degree = log(seq(1:length(degree.distribution(graph))))
    log_distribution = log(degree.distribution(graph))
        
    index = which(!is.infinite(log_distribution), arr.ind=TRUE)
    log_degree = log_degree[index]
    log_distribution = log_distribution[index] # Exclude points whose value of log_distribution is -inf
    
    plot(main = paste("Degree distribution with", n, "nodes"), log_degree, log_distribution, 
         abline(lm(log_distribution ~ log_degree))) # Plot degree distribution and linear regression, find slope 
    print(paste("Slope of the plot is ", cov(log_degree, log_distribution) / var(log_degree)))
}

degree_distribution(g, 1000)
degree_distribution(g_c, 10000)

# e
node_neighbor_degree_distribution = function(graph, n) {
    degree_neighbors = c()
    for (i in 1:n) {
        node = sample(n, 1) # Random pick a node
        neighbor_node = neighbors(graph, node) # Random pick a neighbor of that node
        if (length(neighbor_node) == 1) {
            neighbor = neighbor_node
        } else {
            neighbor = sample(neighbor_node, 1)
        }
        degree_neighbors = c(degree_neighbors, degree(graph, neighbor))
    }    
    df <- as.data.frame(table(degree_neighbors))
    
    log_degree <- log(as.numeric(df[, "degree_neighbors"]))
    log_distribution <- log(as.numeric(df[, "Freq"] / n))
  
    print(paste("Slope of the plot is ", cov(log_degree, log_distribution) / var(log_degree)))
    plot(main = paste("Degree distribution of network with", n, "nodes"), 
         log_degree, log_distribution, abline(lm(log_distribution ~ log_degree)))
}
node_neighbor_degree_distribution(g, 1000)
node_neighbor_degree_distribution(g_c, 10000)

# f
exp_degree = array(0, 1000)
# Expected degree of a node that is added at time step i for 1 ≤ i ≤ 1000
for (i in 1:1000) {
    exp_degree <- exp_degree + degree(g)
}
exp_degree <- exp_degree / 1000
age <- c(999:0)
plot(data.frame(age, exp_degree), main = "Expected Degree of Nodes with age of 1 to 1000", 
     xlab = "Age of nodes",ylab = "Expected Degree")

# g
m_node <- c(2, 5)
n_node <- c(1000, 10000)

degree_distribution = function(graph, n, m) {
    log_degree = log(seq(1:length(degree.distribution(graph))))
    log_distribution = log(degree.distribution(graph))
        
    index = which(!is.infinite(log_distribution), arr.ind=TRUE)
    log_degree = log_degree[index]
    log_distribution = log_distribution[index]
   
    plot(main = paste("Degree distribution with", n, "nodes, m = ", m), log_degree, log_distribution, 
         abline(lm(log_distribution ~ log_degree)))
    print(paste("Slope of the plot is ", cov(log_degree, log_distribution) / var(log_degree)))
}

for (i in m_node) {
    for (j in n_node) {
        g_g <- barabasi.game(j, m = i, directed = F)
    
        plot(main = paste("Undirected network with ", j, "nodes, m = ", i), g_g, vertex.color = "gold", 
             vertex.size = 3, vertex.frame.color = "gray", vertex.label = "")

        community <- cluster_fast_greedy(g_g)
        modularity(community)
        print(community)
        plot(main = paste("Undirected network with ", j, "nodes, m = ", i), g_g, mark.groups = groups(community), 
             vertex.color = "gold",vertex.size = 3, vertex.label.cex = 0.6, edge.curved = 0.2, vertex.label = "")

        index <- c(1:length(community))
        size <- as.vector(sizes(community))

        ggplot(data.frame(index, size), aes(index, size))+ 
        geom_bar(stat = "identity", fill = "blue")+
        labs(title = "Community Structure", x = "Community Index", y = "Community Size")+
        theme(plot.title = element_text(hjust = 0.5))
        
        degree_distribution(g_g, j, i)
    } 
}



# h
g_h_1 <- barabasi.game(1000, m = 1, directed = F)
plot(main = "Original network with 1000 nodes, m = 1", g_h_1, vertex.color = "gold", vertex.size = 3, 
     vertex.frame.color = "gray", vertex.label = "")
degree_seq = degree(g_h_1)

# Original network
community_h_1 <- cluster_fast_greedy(g_h_1)
print(community_h_1)
plot(community_h_1, g_h_1, vertex.color = "gold", vertex.size = 4, vertex.frame.color = "gray", vertex.label = "")
print(paste("Modularity of original network with 1000 nodes, m = 1 is ", modularity(community_h_1)))

# Degree sequence network
g_h_2 <- sample_degseq(degree_seq, method = 'simple.no.multiple')
plot(main = "Degree sequence network", g_h_2, vertex.frame.color = "gray", vertex.size = 3, vertex.label = "")
community_h_2 <- cluster_fast_greedy(g_h_2)
print(community_h_2)
plot(community_h_2, g_h_2, vertex.color = "gold", vertex.size = 4, vertex.frame.color = "gray", vertex.label = "")
print(paste("Modularity of new network with 1000 nodes, m = 1 is ", modularity(community_h_2)))

# 3.

# a
# Modified preferential attachment model that penalizes the age of a node
g = sample_pa_age(n = 1000, pa.exp = 1, ,m = 1, aging.exp = -1, zero.deg.appeal = 1, 
                  zero.age.appeal = 0, deg.coef = 1, age.coef = 1, aging.bin = 1000, directed = FALSE)

plot(main = "Preferential attachment model that penalizes the age of a node", g, vertex.color="gold", 
     vertex.size = 4, vertex.label = "")

log_degree = log(seq(1:length(degree.distribution(g))))
log_distribution = log(degree.distribution(g))

index = which(!is.infinite(log_distribution), arr.ind=TRUE)
log_degree = log_degree[index]
log_distribution = log_distribution[index]

plot(main = "Degree distribution in log-log scale", log_degree, 
     log_distribution, abline(lm(log_distribution ~ log_degree)), xlab = "log(degree)",ylab = "log(distribution)")

print(paste("Slope of the plot is ", cov(log_degree, log_distribution) / var(log_degree)))

# b
community <- cluster_fast_greedy(g)
modularity(community)
print(community)
plot(main = "Community structure of modified preferential attachment model", g, mark.groups=groups(community), 
     vertex.color = "gold", vertex.size = 6, vertex.label = "")