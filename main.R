########################################################################################################################
#                                                          ##                                                          #
#                             Random sampling of road sections with minimum and maximum length                         #
#                                                          ##                                                          #
########################################################################################################################



library(sfnetworks) # https://github.com/luukvdmeer/sfnetworks
library(sf)
library(dplyr)
library(progress)



####################################################### Functions ######################################################

#### Function for select random nodes in network
sample_nodes <- function(net,                             # Network
                         nb_nodes = 2                     # Number of terminal nodes to calculate the path
                         ) {

  n_nodes <- net %>% st_as_sf("nodes") %>% nrow()         # take nodes numbers
  from_node <- sample(1:n_nodes, size = 1)                # sample 1 node into all nodes, it's the start of path
  to_nodes <- sample(1:n_nodes[n_nodes != from_node],
                     size = nb_nodes-1,
                     replace = F)                         # samples x node in all nodes by deleting the starting node

  # Put the result in a list
  res <- list(from_node, to_nodes)
  names(res) <- c("from", "to")

  return(res)
}




#### Function for calculate the shortest path in network between nodes
random_path <- function(net,                              # Network
                        sample_nodes                      # ID of object nodes (from/to)
                        ) {

# Calculat the shortest path
  paths <- st_network_paths(net,
                            from = sample_nodes$from,
                            to = sample_nodes$to,
                            weights = "weight")

  # extract the node IDs
  node_path <- paths %>%
    pull(node_paths) %>%
    unlist()

  # only keep the network for these nodeIDs
  path_sf <- net %>%
    activate(nodes) %>%
    slice(node_path) %>%
    st_as_sf("edges")                                     # reformat edges into sf object

  return(path_sf)
}



#
# #### Function to randomly sample a number of paths between a min and max length
# get_me_sample_path <- function(net,                       # Network
#                                number = 1,                # number of desired section
#                                min_length = 2000,         # minimal length of section (using unit of crs)
#                                max_length = 5000,         # maximal length of section (using unit of crs)
#                                n_nodes = 2 ) {
#
# # just progress bar for long statment
#   pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsed || Estimated time: :eta]",
#                          total = number,
#                          clear = FALSE,
#                          show_after = 2)
#
#   i <- 0                                                  # count good paths
#   while (i != number) {                                   # keep searching until the number of good paths is reached
#       while(TRUE) {                                       # just a little hack to continue until counter order
#         random_nodes <- sample_nodes(net, n_nodes)        # select random nodes in network
#         path <- random_path(net, random_nodes) %>%        # calculate the shortest path into nodes
#           select(geometry) %>%                            # keep only geom
#           summarise()                                     # summarise multiline into one single line
#
#         current_length <- sum(st_length(path)) %>%        # calculate the length of the path
#           as.integer()
#
#         ifelse(current_length > min_length &
#                  current_length < max_length,             # checks if the length is between min/max
#                ifelse(i == 0,                             # If yes, check if this is the first path
#                       break,                              # If yes, break the loop
#                       ifelse(sum(st_intersects(path, paths_df, sparse = F)) == 0, # check if there is no intersection
#                              break,                       # if there is no intersection, break the loop
#                              next)),                      # if there is intersection, try again
#               next)                                       # if the length is not between min/max, try again
#         }
#     i <- i+1                                              # if all checks are ok, increases the counter by 1
#     path <- mutate(path, id_path = as.integer(i))         # add id number of the path
#
#     if (i == 1) {                                         # check if this is the first path
#       paths_df <- path                                    # if yes, creat dataframe of paths with first path
#     } else if (i > 1) {
#       paths_df <- rbind(paths_df, path)                   # Then, add row of new path into exsiting dataframe of paths
#     }
#     pb$tick()                                             # increases the progress bar statment
#   }
#   return(paths_df)
# }



#### Function to randomly sample a number of paths between a min and max length
get_me_sample_path <- function(net,                       # Network
                               number = 1,                # number of desired section
                               min_length = 2000,         # minimal length of section (using unit of crs)
                               max_length = 5000,         # maximal length of section (using unit of crs)
                               overlap = FALSE,           # can the paths overlap? (TRUE/FALSE)
                               n_nodes = 2) {


  # just progress bar for long statment
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsed || Estimated time: :eta]",
                         total = number,
                         clear = FALSE,
                         show_after = 2)

  i <- 0                                                  # count good paths
  while (i != number) {                                   # keep searching until the number of good paths is reached
      while(TRUE) {                                       # just a little hack to continue until counter order
        random_nodes <- sample_nodes(net, n_nodes)        # select random nodes in network

        path <- suppressWarnings(                         # stop "Couldn't reach some vertices." warning
          random_path(net, random_nodes)                  # calculate the shortest path into nodes
                                ) %>%
          select(geometry) %>%                            # keep only geom
          summarise()                                     # summarise multiline into one single line

        current_length <- sum(st_length(path)) %>%        # calculate the length of the path
          as.integer()

        ifelse(current_length > min_length &
                 current_length < max_length,             # checks if the length is between min/max
               ifelse(i == 0,                             # If yes, check if this is the first path
                      break,                              # If is the first path, break the loop
                      ifelse(isFALSE(overlap),            # If user don't want overlap, check intersection
                             ifelse(sum(st_intersects(path, paths_df, sparse = F)) == 0,
                                    break,                # if there is no intersection, break the loop
                                    next                  # if there is intersection, try again
                                    ),
                             ifelse(isTRUE(overlap),      # If user want overlap,
                                    break,                # break loop
                                    stop('\n',            # If the answer is not clear, put an error
                                         'Do you want overlapping paths?','\n',
                                         'Accepted values: "TRUE" for yes, "FALSE" for no',
                                         call. = F)
                                   )
                             )
                      ),
              next                                        # if the length is not between min/max, try again
              )
        }
    i <- i+1                                              # if all checks are ok, increases the counter by 1
    path <- mutate(path, id_path = as.integer(i))         # add id number of the path

    if (i == 1) {                                         # check if this is the first path
      paths_df <- path                                    # if yes, creat dataframe of paths with first path
    } else if (i > 1) {
      paths_df <- rbind(paths_df, path)                   # Then, add row of new path into exsiting dataframe of paths
    }
    pb$tick()                                             # increases the progress bar statment
  }
  return(paths_df)
}



################################################# load and clean datas #################################################



# load roads layer (linestring, WGS 84)
ign_road <- st_read("./input/small_ign_road.shp")

# transform the road layer to network
net <- ign_road %>%
  st_transform(4326) %>%
  st_zm(drop = T) %>%                                     # drop Z and M geometry for hide warnings
  as_sfnetwork(directed = FALSE) %>%                      # transform the road layer to not directed network
  activate("edges") %>%
  mutate(weight = edge_length())




################################################### random sampling ####################################################




path <- get_me_sample_path(net, 5, 1000, 10000, overlap = F, 2)


# Visualise result
library(ggplot2)

ggplot() +
  geom_sf(data = ign_road, color = "gray") +
  geom_sf(data = path, aes(color = as.factor(id_path)), size = 1.5) +
  theme_minimal() +
  labs(color = "Paths")



