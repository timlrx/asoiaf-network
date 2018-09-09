library(tidyverse)
library(tidygraph)
library(igraph)
library(ggplot2)
library(ggraph)

set.seed(101)

### Read in data
node_files <- list.files(path = "data/", pattern = "nodes")
edge_files <- list.files(path = "data/", pattern = "edges")
node_df = do.call(rbind, lapply(paste0("data/",node_files), function(x) read_csv(x)))
edge_df = do.call(rbind, lapply(paste0("data/",edge_files), function(x) read_csv(x)))

#saveRDS(node_df, "data/asoiaf_node_df.rds")
#saveRDS(edge_df, "data/asoiaf_edge_df.rds")

node_df <- node_df %>%
  group_by(Id) %>%
  filter(row_number()==1) %>%
  mutate(Label_short = word(Label, 1)) %>%
  mutate(Label_short = case_when(
    Label == 'Aemon Targaryen (Maester Aemon)' ~ 'Maester Aemon',
    Label == 'Aegon I Targaryen' ~ 'Aegon I Targaryen',
    Label == 'Aegon V' ~ 'Aegon V',
    Label == 'Brynden Rivers' ~ 'Bloodraven',
    Label == 'High Sparrow' ~ 'High Sparrow',
    Label == 'Roose Bolton' ~ 'Roose Bolton',
    Label == 'Walder Frey' ~ 'Walder Frey',
    Label == 'Jon Arryn' ~ 'Jon Arryn',
    Label == 'Jon Snow' ~ 'Jon Snow',
    Label == 'Robert Arryn' ~ 'Robert Arryn',
    Label == 'Robert Baratheon' ~ 'Robert Baratheon',
    TRUE ~ Label_short
  ))

### Check duplicate names
node_df %>% group_by(Label_short) %>%
  mutate(c = n()) %>%
  ungroup() %>%
  arrange(Label_short, desc(c)) %>%
  filter(c>1) %>%
  View()

edge_df <- edge_df %>%
  filter(!is.na(book)) %>%
  rename(from = Source, to = Target) %>%
  select(from, to, weight, book)

book1 = edge_df %>%
  filter(book==1)

### Plot distribution of weights
book1 %>%
  ggplot(aes(x=weight)) +
  geom_bar() +
  xlim(0,100) +
  theme_bw()

edge_df %>% 
  group_by(book = as.character(book)) %>%
  summarise(pc_25 = quantile(weight, 0.25),
            pc_50 = quantile(weight, 0.5),
            pc_75 = quantile(weight, 0.75))

df <- tbl_graph(nodes = node_df[node_df$Id %in% union(book1$from, book1$to),],
                edges = book1,
                directed = FALSE)

df2 <- df %>%
  activate(nodes) %>%
  mutate(pagerank = centrality_pagerank(weights = weight, directed = FALSE),
         degree = centrality_degree(weights = weight),
         pagerank_75pc = quantile(pagerank, 0.75)) %>%
  activate(edges) %>%  
  filter(weight >= quantile(weight, 0.75)) %>%
  activate(nodes) %>%
  filter(pagerank > pagerank_75pc) %>%
  filter(!node_is_isolated())

ggraph(df2, layout = "fr") +
  geom_edge_link(color='blue') +
  geom_node_point() +
  geom_node_text(aes(label = Label_short), repel = TRUE) +
  theme_graph()

### Filter out the small component
df3 <- df2 %>%
  mutate(component = group_components(),
         community = as.factor(group_infomap(weights = weight))) %>%
  group_by(component) %>%
  mutate(component_size = n()) %>%
  filter(component_size > 5) %>%
  ungroup() %>%
  arrange(community)

df3 <- df2 %>%
  mutate(community = as.factor(group_infomap(weights = weight))) %>%
  
df3 %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  group_by(component) %>% 
  count()

### Plot communities
ggraph(df3, layout = "fr") +
  geom_edge_link(alpha=0.2) +
  geom_node_point(aes(color = community), size = 5) +
  geom_node_text(aes(label = Label_short), repel = TRUE) +
  theme_graph()

ggraph(df3, layout = "linear", circular=T) +
  geom_edge_arc(alpha=0.2) +
  geom_node_point(aes(color = community, size = pagerank)) +
  geom_node_text(aes(label = Label_short), repel = TRUE) +
  theme_graph()

### Extend the above analysis to all the books

### Function to create graphs for each book
process_graph <- function(node_df, edge_df, book_num, q = 0.75){

book = edge_df %>%
  filter(book==book_num)  

df <- tbl_graph(nodes = node_df[node_df$Id %in% union(book$from, book$to),],
                edges = book,
                directed = FALSE)
  
df2 <- df %>%
  activate(nodes) %>%
  mutate(pagerank = centrality_pagerank(weights = weight, directed = FALSE),
         degree = centrality_degree(weights = weight),
         pagerank_qpc = quantile(pagerank, q)) %>%
  activate(edges) %>%  
  filter(weight >= quantile(weight, q)) %>%
  activate(nodes) %>%
  filter(pagerank > pagerank_qpc) %>%
  filter(!node_is_isolated()) %>%
  select('Id', 'Label', 'Label_short', 'pagerank') %>%
  rename(!! paste0('pagerank',book_num) := pagerank)

df3 <- df2 %>%
  mutate(component = group_components()) %>%
  group_by(component) %>%
  mutate(component_size = n()) %>%
  filter(component_size > 5) %>%
  ungroup() %>%
  select(-component, -component_size)

return(df3)
}

book2 <- process_graph(node_df, edge_df, book_num = 2, q=0.8)

all_graphs <- lapply(1:5, function(x) process_graph(node_df, edge_df, book_num=x, q=0.8))

full_graph <- Reduce(function(...) graph_join(..., by = c('Id', 'Label', 'Label_short')), all_graphs) %>%
  convert(to_undirected)

### Sum multiple edges to create single edge between nodes
full_graph_edges <- full_graph %>%
  activate(edges) %>%
  as_tibble() %>%
  group_by(from, to) %>%
  summarise(weight = sum(weight), book = first(book))

full_graph <- full_graph %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) %>%
  select(from, to) %>%
  left_join(full_graph_edges, by = c('from', 'to'))

### Create community based on total page_rank score
full_graph <- full_graph %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_infomap(weights = weight))) %>%
  arrange(community)

full_graph <- full_graph %>%  
  mutate_at(vars(contains('pagerank')), funs(if_else(is.na(.), 0, .))) %>%
  mutate(pagerank = pagerank1 + pagerank2 + pagerank3 + pagerank4 + pagerank5)

full_layout <- create_layout(graph = full_graph, layout = "linear", circular = T)
max(as.numeric(full_layout$community))
xmin <- min(full_layout$x)
xmax <- max(full_layout$x)
ymin <- min(full_layout$y)
ymax <- max(full_layout$y)

plot_graph <- function(graph) {
  
  graph <- graph %>%
    left_join(full_layout[full_layout$Id %in% V(graph)$Id, c('x', 'y', 'Id', 'community', 'pagerank')],
              by = 'Id')

  graph %>%
    ggraph(layout = "manual", x = x, y = y, circular = T) +
    geom_edge_arc(aes(alpha = weight)) +
    geom_node_point(aes(color = community, size = pagerank)) +
    # data = filter(graph %>% as_tibble(), x>0),
    geom_node_text(aes(label = Label_short, x = x * 1.04, y = y* 1.04,
                       angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                                      90 + atan(-(x/y))*(180/pi),
                                      270 + atan(-x/y)*(180/pi)),
                       hjust = ifelse(x > 0, 0 ,1))) +
    theme_graph() +
    expand_limits(x = c(xmin-0.2, xmax+0.2), y = c(ymin-0.2, ymax+0.2))
}

plot_graph(full_graph %>% select(-community, -pagerank))
plot_graph(all_graphs[[1]])

