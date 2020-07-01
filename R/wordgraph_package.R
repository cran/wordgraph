freq.of.all.combinations.df.numeric <- function(column1, column2, data.df, order = 0, removeNA = TRUE) {
  ## Defining a data frame that will receive the data ...
  all.freq.df = data.frame(word1code = integer(), word2code = integer(), word1 = character(), word2 = character(), freq = integer())
  colnames(all.freq.df) = c('word1code', 'word2code', 'word1', 'word2', 'freq')

  ## Selection of the variables containing the words ...
  vars_to_consider <- c(column1, column2)
  graph_Data = data.df[vars_to_consider]
  graph_Data = graph_Data[stats::complete.cases(graph_Data), ]

  ## All frequencies of all combinations ...
  all.comps.freqs = tibble::as_tibble(plyr::count(graph_Data, vars = c(column1, column2)))

  ## Preparing to record all the combinations of the first and the second word ...
  min1 = min(all.comps.freqs[, column1])
  max1 = max(all.comps.freqs[, column1])

  min2 = min(all.comps.freqs[, column2])
  max2 = max(all.comps.freqs[, column2])

  min1 = min(min1, min2)
  max2 = max(max1, max2)

  ## Check for the presence of value.labels
  if(is.null(attr(data.df[, column1], "value.labels"))){
    attr(data.df[, column1], "value.labels") = data.df[, column1]
    warning(paste("No labels were found for variable ", column1, ". The values of the variable will used instead.", sep = ""))
  }
  if(is.null(attr(data.df[, column2], "value.labels"))){
    attr(data.df[, column2], "value.labels") = data.df[, column2]
    warning(paste("No labels were found for variable ", column2, ". The values of the variable will used instead.", sep = ""))
  }

  ## Record labels to match codes with words ...
  labels1 = as.data.frame(attr(data.df[, column1], "value.labels"))
  names(labels1)[1] = "code"
  labels2 = as.data.frame(attr(data.df[, column2], "value.labels"))
  names(labels2)[1] = "code"

  ## Retrospection of all two variables (First and second word) ...
  for (firstwordid in min1:max1) {
    for (secondwordid in firstwordid:max2) {

      ## calculation of the frequency of occurrence 1st - 2nd and 2nd - 1st ...
      totalfreqforthecombination = sum(all.comps.freqs[which((all.comps.freqs[, column1] == firstwordid & all.comps.freqs[, column2] == secondwordid) |
                                                               (all.comps.freqs[, column1] == secondwordid & all.comps.freqs[, column2] == firstwordid)),]$freq)
      ## find the word from the code ...
      indexword1 <- which(labels1 == firstwordid, arr.ind=TRUE)
      indexword2 <- which(labels2 == secondwordid, arr.ind=TRUE)

      ## Create the new line and add it to the data frame ...
      new.line <- list(word1code = firstwordid, word2code = secondwordid, word1 = rownames(labels1)[indexword1[1]], word2 = rownames(labels2)[indexword2[1]], freq = totalfreqforthecombination)
      all.freq.df = rbind(all.freq.df, new.line, stringsAsFactors=FALSE)
    }
  }
  ## ... we only keep those combinations that have a frequency above 0 ...
  all.freq.df = all.freq.df[which(all.freq.df$freq > 0),]

  if(removeNA){
    # ... we remove the lines that have at least one NA ...
    all.freq.df = all.freq.df[stats::complete.cases(all.freq.df), ]
  }

  ## we put a normal numbering on the lines ...
  rownames(all.freq.df) = 1:nrow(all.freq.df)

  ## Calculate the percentage of occurrence of a combination of words ...
  all.freq.df$percent = all.freq.df$freq / sum(all.freq.df$freq) * 100

  if(order == 1){all.freq.df = all.freq.df[order(-all.freq.df$freq),]}
  if(order == 2){all.freq.df = all.freq.df[order(all.freq.df$word1code),]}
  if(order == 3){all.freq.df = all.freq.df[order(all.freq.df$word1),]}

  return(all.freq.df)
}


#' Count all combinations between the values of column1 and column2 and return a data frame
#' with the combination of the items and the respecting codes.
#'
#' @param column1 The first column name of data.df (string)
#' @param column2 The second column name of data.df (string)
#' @param data.df The data frame where column1, column2 belong.
#' @param order 0: No order, 1: decreasing frequency, 2: increasing word1code, 3: increasing word1
#' @return A data frame contains the columns "word1code" "word2code" "word1" "word2" "freq" and "percent"
#' @examples
#'
#'df1 = freq.of.all.combinations.df("diet1stword",
#'"diet2ndword", freeassociationdata)
#'
#'head(df1)
#'
#'     word1code word2code word1               word2 freq   percent
#' 1         0         0  <NA>                <NA>    2 0.6896552
#'
#' @export
#'
freq.of.all.combinations.df <- function(column1, column2, data.df, order = 0, removeNA = TRUE) {

  ## If the variables are numeric, then use freq.of.all.combinations.df.numeric ...
  ## If the function is called by get.all.word.pairs.frequency then it is guaranteed that they will be numeric ...
  if(is.numeric(data.df[, column1]) & is.numeric(data.df[, column2])){
    return(freq.of.all.combinations.df.numeric(column1, column2, data.df, order, removeNA))
  }

  # In case the function is called autonomously, with two non-numerical variables then the following code will be executed ...
  # If the variables are not arithmetic then use the values themselves as value labels and replace the values with their numbering ...

  ## Defining a data frame that will receive the data ...

  all.freq.df = data.frame(word1code = integer(), word2code = integer(), word1 = character(), word2 = character(), freq = integer())
  colnames(all.freq.df) = c('word1code', 'word2code', 'word1', 'word2', 'freq')

  ## Select variables with words ...
  vars_to_consider <- c(column1, column2)
  graph_Data = data.df[vars_to_consider]
  graph_Data = graph_Data[stats::complete.cases(graph_Data), ]

  ## All frequencies of all combinations ...
  all.comps.freqs = tibble::as_tibble(plyr::count(graph_Data, vars = c(column1, column2)))

  # Create a list that will contain all the unique prices ...
  labels1 = sort(unique(as.vector(data.df[, column1])))
  labels2 = sort(unique(as.vector(data.df[, column2])))

  all.labels = c(labels1, labels2)
  all.labels.unique = sort(unique(all.labels))

  # Retrospective of each possible pair 1st - 2nd variable ...
  for(pos1 in 1:length(all.labels.unique)){
    label1 = all.labels.unique[pos1]
    for(pos2 in pos1:length(all.labels.unique)){
      label2 = all.labels.unique[pos2]

      ## calculation of the frequency of occurrence 1st - 2nd and 2nd - 1st ...
      totalfreqforthecombination = sum(all.comps.freqs[which((all.comps.freqs[, column1] == label1 & all.comps.freqs[, column2] == label2) |
                                                               (all.comps.freqs[, column1] == label2 & all.comps.freqs[, column2] == label1)),]$freq)
      word1code = which(all.labels.unique == label1, arr.ind=TRUE)
      word2code = which(all.labels.unique == label2, arr.ind=TRUE)

      ## Create the new line and add it to the data frame ...
      new.line <- list(word1code = word1code, word2code = word2code, word1 = label1, word2 = label2, freq = totalfreqforthecombination)
      all.freq.df = rbind(all.freq.df, new.line, stringsAsFactors=FALSE)
    }
  }
  ## ... we only keep those combinations that have a frequency above 0 ...
  all.freq.df = all.freq.df[which(all.freq.df$freq > 0),]

  if(removeNA){
    # ... we remove the lines that have at least one NA ...
    all.freq.df = all.freq.df[stats::complete.cases(all.freq.df), ]
  }

  ## we put normal numbering in the lines ...
  rownames(all.freq.df) = 1:nrow(all.freq.df)

  ## Calculate the percentage of occurrence of a combination of words ...
  all.freq.df$percent = all.freq.df$freq / sum(all.freq.df$freq) * 100

  if(order == 1){all.freq.df = all.freq.df[order(-all.freq.df$freq),]}
  if(order == 2){all.freq.df = all.freq.df[order(all.freq.df$word1code),]}
  if(order == 3){all.freq.df = all.freq.df[order(all.freq.df$word1),]}

  return(all.freq.df)
}


#' Provides a column name like the likewhatvariable that is not already exists in the data frame data.
#'
#' @param likewhatvariable The initial name (string)
#' @param data.df The data frame to be added.
#' @return A string with the suggested name
#' @example
#'
#' newvariablename = create_a_new_unique_var_name("diet1stword",
#' freeassociationdata)
#'
#'[1] "diet1stword_id"
#'
#' @export
create_a_new_unique_var_name <- function(likewhatvariable, data.df){
  newpossiblevariable1 = paste(likewhatvariable, "_id", sep = "")
  if(length(names(data.df)[which(names(data.df) == newpossiblevariable1)]) == 0){
    return(newpossiblevariable1)
  }
  else{
    anadditionalindex = 1
    repeat{
        newpossiblevariable1 = paste(likewhatvariable, "_id", anadditionalindex, sep = "")
      if(length(names(data.df)[which(names(data.df) == newpossiblevariable1)]) == 0){
        break
      }
        anadditionalindex = anadditionalindex + 1
    }
    return(newpossiblevariable1)
  }
}




#' Use multiple times the function freq.of.all.combinations.df to count all combinations between
#' each concecutive pair of variables of wordvars ( var1 - var2, var2 - var3, ..., var_{n-1} - var_n).
#' Then combines all pair frequencies into one data frame with the columns "word1code" "word2code" "word1" "word2" "cumsums"
#'
#' @param wordvars The vector containing the names of the variables
#' @param iscircled Should take also the combination between last and first variable (var_n - var1)?
#' @param data.df The data frame where the variables belong.
#' @inherit The function freq.of.all.combinations.df
#' @return A data frame contains the columns "word1code" "word2code" "word1" "word2" "cumsums"
#' @examples
#'   varlist = c("diet1stword", "diet2ndword", "diet3rdword")
#'   df1 = get.all.word.pairs.frequency(varlist, freeassociationdata)
#' @export
#'
get.all.word.pairs.frequency <- function(wordvars, data.df, iscircled = FALSE){
  if(is.null(wordvars) | length(wordvars) <= 1){
    print("To create a graph, at least two word variables should provided in order to compute combination frequencies...")
    return(NULL);
  }

  allvariablesarenumeric = TRUE
  for(avariable in wordvars){
    allvariablesarenumeric = allvariablesarenumeric & is.numeric(data.df[, avariable])
  }

  if(allvariablesarenumeric == TRUE){
    messageforvariables = paste(c("The variables", wordvars, "are numeric."), collapse = " ")
  }
  else{
    messageforvariables = paste(c("The variables", wordvars, "are not numeric. They will replaced internally by their index and their values will be the corresponding labels..."),  collapse = " ")
  }
 # print(messageforvariables)

  if(allvariablesarenumeric == FALSE){
    # Create a list that will contain all the unique values for all variables ...
    labels1 = sort(unique(as.vector(data.df[, wordvars[1]])))
    all.labels = c(labels1)

    for(avariable in wordvars[-1]){
      labels2 = sort(unique(as.vector(data.df[, avariable])))
      all.labels = c(all.labels, labels2)
    }
    all.labels.unique = sort(unique(all.labels))

    newwordvars = c()
    # Now create a new variable for each of the non-numeric variables where each value is replaced by its code.
    for(avariable in wordvars){
      newname = create_a_new_unique_var_name(avariable, data.df)
      newwordvars = c(newwordvars, newname)

      # Add a new column ...
      data.df[,newname] = c(rep(NA, nrow(data.df)))
      # Now fill it with the initial price index ...
      for(onerow in 1:nrow(data.df)){
        replacement = which(all.labels.unique == data.df[, avariable][onerow], arr.ind=TRUE)
        if(length(replacement) == 0){
          data.df[,newname][onerow] = NA
        }
        else{
          data.df[,newname][onerow] = replacement
        }
      }

      thelevels = as.vector(stats::na.omit(unique(data.df[,newname])))
      thelabels = as.vector(unique(stats::na.omit(data.df[,avariable])))
      factor(data.df[, newname], levels = thelevels, labels = thelabels)

      # The new variable will have value.labels the values of the old variable ...

      thelevels = as.vector(stats::na.omit(unique(data.df[,newname])))
      thelabels = as.vector(unique(stats::na.omit(data.df[,avariable])))
      data.df[, newname] <- factor(data.df[, newname], levels = thelevels, labels = thelabels)

      attr(data.df[, newname], "value.labels") = thelabels
    }
    # The continuation of the process will be done for the new numerical variables, which have as labels the initial non numeric ...
    wordvars = newwordvars
  }


  ## Check for presence value.labels
  for(avariable in wordvars){
    if(is.null(attr(data.df[, avariable], "value.labels"))){
      attr(data.df[, avariable], "value.labels") = data.df[, avariable]
      warning(paste("No labels were found for variable ", avariable, ". The values of the variable will used instead.", sep = ""))
    }
  }

  df1 = freq.of.all.combinations.df(wordvars[1], wordvars[2], data.df)
  all.freq.to.return = df1
  for (pos in 2:(length(wordvars) - 1)) {
    df.another = freq.of.all.combinations.df(wordvars[pos], wordvars[pos + 1], data.df)
    all.freq.to.return = rbind(all.freq.to.return, df.another)
  }
  if(iscircled == TRUE){
    df.last.to.first = freq.of.all.combinations.df(wordvars[length(wordvars)], wordvars[1], data.df)
    all.freq.to.return = rbind(all.freq.to.return, df.last.to.first)
  }
  all.freq.to.return$all.words.comb = paste(all.freq.to.return$word1, all.freq.to.return$word2)
  all.freq.to.return$cumsums <- stats::ave(all.freq.to.return$freq, all.freq.to.return$all.words.comb, FUN=sum)
  all.freq.to.return = dplyr::distinct(all.freq.to.return, all.words.comb, .keep_all = TRUE)

  all.freq.to.return <- all.freq.to.return[order(-all.freq.to.return$cumsums),]
#  all.freq.to.return <- all.freq.to.return[order(all.freq.to.return$word1code, all.freq.to.return$word2code, -all.freq.to.return$freq),]
  all.freq.to.return$percent <- NULL
  all.freq.to.return$cumpercent <- NULL
  all.freq.to.return$all.words.comb <- NULL
  all.freq.to.return$freq <- NULL


 # all.freq.to.return = unique(all.freq.to.return)
  return(all.freq.to.return)
}




#' Calculate all word pairs among the variables in wordvars vector and
#' return a tidygraph::as_tbl_graph object in order to create the graph in a
#' latter step from function get.graph.1.n.group.centrality.with.function.
#'
#' @param wordvars The vector containing the names of the variables
#' @param iscircled Should take also the combination between last and first variable (var_n - var1)?
#' @param data.df The data frame where column1, column2 belong.
#' @inherit The function get.all.word.pairs.frequency
#' @return A data frame contains the columns "word1code" "word2code" "word1" "word2" "freq" and "percent"
#' @export
get.word.combinations.as.tbl_graph <- function(wordvars, data.df, iscircled = FALSE){
  all.freq.1.to.n = get.all.word.pairs.frequency(wordvars, data.df, iscircled = iscircled)
  all.freq.1.to.n <- all.freq.1.to.n[c("word1", "word2", "cumsums", "word1code", "word2code")]
  all.freq.1.to.n.as.tibble <- tibble::as_tibble(all.freq.1.to.n)
  get.word.combinations.as.tbl_graph <- tidygraph::as_tbl_graph(all.freq.1.to.n.as.tibble, directed = FALSE)
}





#' Creates the ggraph object, i.e. the graph depicting the words containing the variables vector wordvars
#' The combinations of all word pairs are computed for all concecutive pair of variables and
#' the centrality of each word is calculated according to centralityfunction.
#'
#' @param wordvars The vector containing the names of the variables
#' @param centralityfunction The centrality function to use
#' @param data.df The data frame where wordvars belong.
#' @param iscircled Should take also the combination between last and first variable (var_n - var1)?
#' @inherit The function get.word.combinations.as.tbl_graph
#' @return A graph, that is a list of "data" "layers" "scales" "mapping" "theme" "coordinates" "facet" "plot_env" "labels" "guides"
#' @return The graph$data list contains the graph plot data (coordinates, centrality)
#' @note The function tidygraph::centrality_edge_betweenness() results in an error since it requires edges to be active.
#' @examples
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_authority(), freeassociationdata)
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_betweenness(), freeassociationdata)
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_closeness(), freeassociationdata)
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_pagerank(), freeassociationdata)
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_eigen(), freeassociationdata)
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_alpha(), freeassociationdata)
#'     agraph = get.graph.1.n.group.centrality.with.function(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), tidygraph::centrality_edge_betweenness(), freeassociationdata)
#' @export
get.graph.1.n.group.centrality.with.function <- function(wordvars, centralityfunction, data.df, iscircled = FALSE) {

    '%>%' <- purrr::'%>%'

    # Get the tbl.graph of the graph that describes the relationships of var1, ..., var_n,
    word.combinations.as.tbl_graph <- get.word.combinations.as.tbl_graph(wordvars, data.df, iscircled = iscircled)

    # Draw the graph with grouping and centrality 1, 2 and 3
    agraph.1.n.group.centrality = word.combinations.as.tbl_graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::mutate(community = as.factor(tidygraph::group_infomap()), centrality = centralityfunction) %>%
        ggraph::ggraph(layout = "graphopt") +
        ggraph::geom_edge_link(width = 1, colour = "lightgray") +
        ggraph::geom_node_point(ggplot2::aes(colour = community, size = centrality)) +
        ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
        ggraph::theme_graph() +
        ggplot2::labs(colour = "Group", size = "Centrality") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4)))
    get.graph.1.n.group.centrality.with.function <- agraph.1.n.group.centrality
  }





#' Creates a ggraph object for all available centrality functions
#' The combinations of all word pairs are computed for all concecutive pair of variables and
#' the centrality of each word is calculated according to all available centrality functions
#'
#' @note  http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/
#' @param wordvars The vector containing the names of the variables
#' @param data.df The data frame where wordvars belong.
#' @param iscircled Should take also the combination between last and first variable (var_n - var1)?  Default is FALSE.
#' @param verbose Set to TRUE if warning messages are desired. Default is set to FALSE (do not show warning messages)
#' @inherit The function get.graph.1.n.group.centrality.with.function
#' @return A list of objects "graphs" and "wordreport" which are themselves also lists.
#' @return The graphs list contains the graph data for each one centrality index
#' @return The wordreport list contains all centrality indexes for every different word that participates in a combination among wordvars variable vector.
#' @examples
#'     allgraphs = get.all.graphs(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), freeassociationdata)
#' @export
get.all.graphs <- function(wordvars, data.df, iscircled = FALSE, verbose = FALSE) {

  ## Check for presence value.labels
  for(avariable in wordvars){
    if(is.null(attr(data.df[, avariable], "value.labels"))){
      attr(data.df[, avariable], "value.labels") = data.df[, avariable]
      warning(paste("No labels were found for variable ", avariable, ". The values of the variable will used instead.", sep = ""))
    }
  }

  graphs_list = list()

  centrality.list <- list()
  centrality.list[[1]] <- tidygraph::centrality_alpha
  centrality.list[[2]] <- tidygraph::centrality_authority
  centrality.list[[3]] <- tidygraph::centrality_betweenness
  centrality.list[[4]] <- tidygraph::centrality_closeness
  centrality.list[[5]] <- tidygraph::centrality_pagerank
  centrality.list[[6]] <- tidygraph::centrality_eigen
#  centrality.list[[7]] <- tidygraph::centrality_edge_betweenness #  Error: This call requires edges to be active

  centralities.names = c("centrality_alpha", "centrality_authority", "centrality_betweenness", "centrality_closeness", "centrality_pagerank", "centrality_eigen", "centrality_edge_betweenness")

  firstNonErrorId = 0
  IHaveFoundTheFirstOkCentrality = FALSE
  report_for_word = NULL

    for (fun.id in 1:length(centrality.list)) {
    skip_to_next <- FALSE
    tryCatch(
    {
      agraph.1.n = get.graph.1.n.group.centrality.with.function(wordvars, centrality.list[[fun.id]](), data.df, iscircled)
      graphs_list[centralities.names[fun.id]] = agraph.1.n
      print(paste("Id = ", fun.id, ". Graph corresponding to the centrality function ", centralities.names[fun.id], " was computed OK!", sep = ""))
      if(is.null(report_for_word)){
        report_for_word = agraph.1.n$data[, c("name", "centrality")]
      }
      else{
        report_for_word[centralities.names[fun.id]] <- agraph.1.n$data[, c("centrality")]
      }
      if(IHaveFoundTheFirstOkCentrality == FALSE){
        firstNonErrorId = fun.id
        IHaveFoundTheFirstOkCentrality = TRUE
      }
    },
    error = function(e) {
      message(paste("Error with the centrality ", centralities.names[fun.id], ", error message: ", e))
      skip_to_next <<- TRUE
    }
    )
    if(skip_to_next) { next }
    }
  names(report_for_word)[names(report_for_word) == "centrality"] <- centralities.names[firstNonErrorId]

  if(length(names(report_for_word)) < length(centrality.list) + 1){
    for (fun.id in 1:length(centrality.list)) {
      if(is.na(pmatch(centralities.names[fun.id], names(report_for_word)))){
        print(paste("Adding a blank column for ", centralities.names[fun.id], " at position ", fun.id + 1))
        blank_column = rep(NA, nrow(report_for_word))
        report_for_word = tibble::add_column(report_for_word, blank_column, .after = fun.id)
        names(report_for_word)[fun.id + 1] <- centralities.names[fun.id]
      }
    }
  }

  list_to_return <- list(graphs = graphs_list, wordreport = report_for_word)
  return(list_to_return)
}




#' Plots a graph created by get.all.graphs or get.graph.1.n.group.centrality.with.function functions.
#'
#' @param agraph_object A graph object
#' @return A plot of the graph.
#' @examples
#'     allgraphs = get.all.graphs(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), freeassociationdata)
#'     graph_plot(allgraphs$graphs$centrality_authority)
#' @export
graph_plot <- function(agraph_object){
  ggraph::ggraph(agraph_object, layout = 'graphopt') +
    ggraph::geom_edge_link(width = 1, colour = "lightgray") +
    ggraph::geom_node_point(ggplot2::aes(colour = community, size = centrality)) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggraph::theme_graph() +
    ggplot2::labs(colour = "Group", size = "Centrality") +
    ggplot2::theme(legend.position = 'bottom')+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4)))
}




#' Search at wordreporttolookat$name and find the index of the word with name awordtolookfor
#' The index is the code of the word in the column
#' Equivalent to which(grepl(awordtolookfor, allcentralitiesandgraphs$wordreport$name))
#'
#' @param awordtolookfor The word to look for
#' @param wordreporttolookat The wordreport list to look for the centrality of the word (put the wordreport part of allcentralitiesandgraphs object)
#' @param verbose Set to TRUE if warning messages are desired. Default is set to FALSE (do not show warning messages)
#' @return An integer with the index of the word awordtolookfor
#' @examples
#'   allgraphs = get.all.graphs(c("diet1stword", "diet2ndword",
#'   "diet3rdword"), freeassociationdata)
#'   find.word.code.in.wordreport("Caution", allgraphs$wordreport)
#' @seealso Equivalent to which(grepl("Caution", allcentralitiesandgraphs$wordreport$name))
#' @export
#'
find.word.code.in.wordreport <- function(awordtolookfor, wordreporttolookat, verbose = FALSE){
  allnames = wordreporttolookat$name
  howmanywordsinreport = length(wordreporttolookat$name)
  for(i in 1:howmanywordsinreport) {
    if(!is.na(awordtolookfor)){
      if(is.null(allnames[i])){
        warning(paste("NULL at position ", i))
      }
      else{
        if(allnames[i] == awordtolookfor){
          return(i)
        }
      }
    }
  }
  if(verbose){warning(paste("The word ", awordtolookfor, " was not found!"))}
  return(NA)
}




#' Loop into the entries of the wordreporttolookat, find the centrality function described by centralityfunctionstr and returns the centrality of the word.
#' If there is not a corresponding code then the function return NA
#'
#' @param awordtolookfor The word to look for
#' @param centralityfunctionstr The centrality function description string
#' @param wordreporttolookat The wordreport list to look for the centrality of the word
#' @param verbose Set to TRUE if warning messages are desired. Default is set to FALSE (do not show warning messages)
#' @return A double with the centrality of the word awordtolookfor
#' @examples
#'   allcentralitiesandgraphs = get.all.centralities(c("diet1stword", "diet2ndword",
#'   "diet3rdword"), freeassociationdata)
#'   get.word.centrality("Caution", "centrality_authority", allcentralitiesandgraphs$wordreport)
#' @export
#'
get.word.centrality <- function(awordtolookfor, centralityfunctionstr, wordreporttolookat, verbose = FALSE){
  wordcode = find.word.code.in.wordreport(awordtolookfor, wordreporttolookat)
  if(is.na(wordcode)){
    if(verbose){warning(paste("There is not corresponding code for the word ", awordtolookfor))}
    return(NA)
  }
  if(!is.na(wordcode)){
    column.of.centrality.value = grep(centralityfunctionstr, colnames(wordreporttolookat))
    word.centrality = wordreporttolookat[[column.of.centrality.value]][wordcode]
    return(word.centrality)
  }
  return(NA)
}




#' Find the label (string) of a variable specific value at the row.
#' If there is not a label then the function return the value.
#' If there is not value or the nrow is not in an acceptable range then it returns NA.
#'
#' @param arow The row to look at
#' @param column.of.data.frame The column to look for value labels
#' @param verbose Set to TRUE if warning messages are desired. Default is set to FALSE (do not show warning messages)
#' @return A string with the label of the value
#' @example get_label_from_columnrow(21, freeassociationdata$gender)
#' @export
#'
get_label_from_columnrow <- function(arow, column.of.data.frame, verbose = FALSE){
  if(arow <= 0){
    if(verbose){warning(paste("There is not position ", arow))}
    return(NA)
  }
  if(arow > length(column.of.data.frame)){
    if(verbose){warning(paste("The column has ", length(column.of.data.frame) , " rows, thus no available label at position ", arow))}
    return(NA)
  }
  if(is.null(attr(column.of.data.frame, "value.labels")) | length(attr(column.of.data.frame, "value.labels")) == 0){
    if(verbose){warning("There are not attached labels to the data vector. The value is returned as the label.")}
    return(column.of.data.frame[arow])
  }
  for(i in 1:length(attr(column.of.data.frame, "value.labels"))){
    if(!is.na(column.of.data.frame[arow])){
      if(column.of.data.frame[arow] == attr(column.of.data.frame, "value.labels")[i]){
        return(labels(attr(column.of.data.frame, "value.labels")[i]))
      }
    }
    else{
      if(verbose){warning(paste("The position ", arow, " is NA."))}
      return(NA)
    }
  }
  return(NA)
}



#' Takes one variable (columnwithoriginalwords) and return a vector with the centrality scores of the corresponding items.
#' The centrality scores are retreived from the wordreport, an object that has been created previously by the function get.all.centralities
#'
#' @param centralityfunctionstr The type of the centrality function to use
#' @param columnwithoriginalwords The column to look for the words
#' @param awordreport The awordreport to look for the centrality of each word
#' @param verbose Set to TRUE to print every correspondence (default is FALSE)
#' @return A vector containing the centralities of the words contained in columnwithoriginalwords
#' @note The centrality method "centrality_edge_betweenness" requires active nodes.
#' @examples
#'  centralities_vector1 = create.centrality.variable("centrality_betweenness",
#'  freeassociationdata$exercise1stword, full_report_for_word_diatrofi$wordreport)
#'  centralities_vector2 = create.centrality.variable("centrality_alpha",
#'  freeassociationdata$exercise1stword, full_report_for_word_diatrofi$wordreport)
#'  centralities_vector3 = create.centrality.variable("centrality_authority",
#'  freeassociationdata$exercise1stword, full_report_for_word_diatrofi$wordreport)
#'  centralities_vector4 = create.centrality.variable("centrality_closeness",
#'  freeassociationdata$exercise1stword, full_report_for_word_diatrofi$wordreport)
#'  centralities_vector5 = create.centrality.variable("centrality_pagerank",
#'  freeassociationdata$exercise1stword, full_report_for_word_diatrofi$wordreport)
#'  centralities_vector6 = create.centrality.variable("centrality_eigen",
#'  freeassociationdata$exercise1stword, full_report_for_word_diatrofi$wordreport)
#' @export
#'
create.centrality.variable <- function(centralityfunctionstr, columnwithoriginalwords, awordreport, verbose = FALSE){
  vector.to.return = c()
  for(i in 1:length(columnwithoriginalwords)){
    label.at.position.i = get_label_from_columnrow(i, columnwithoriginalwords)
    centr.to.save = get.word.centrality(label.at.position.i, centralityfunctionstr, awordreport)
    vector.to.return = c(vector.to.return, centr.to.save)
    if(verbose){print(paste("i = ", i, "label.at.position.i = ", label.at.position.i, ", centr.to.save = ", centr.to.save))}
  }
  return(vector.to.return)
}




#' Creates the graph of all words contained in the wordvars, computes the centrality of each word
#' and for each wordvars variable, it adds to the dataframe a column containing the
#' corresponding centrality score.
#'
#' Supported centrality functions are "centrality_alpha", "centrality_authority", "centrality_betweenness", "centrality_closeness", "centrality_pagerank", "centrality_eigen".
#' Thus, subsequent analysis can follow where each word is replaced
#' by its correspondent centrality score.
#'
#' @param wordvars The vector containing the names of the variables
#' @param centralityfunctionstr The type of the centrality function to use
#' @param data.df The data frame where wordvars belong.
#' @param iscircled Should take also the combination between last and first variable (var_n - var1)?  Default is FALSE.
#' @param verbose Set to TRUE if warning messages are desired. Default is set to FALSE (do not show warning or error messages)
#' @return The data frame where centrality score variable are added, one for each column of wordvars vector.
#' @examples
#'   wordvars = c("diet1stword", "diet2ndword", "diet3rdword")
#'   df.with.centralities1 = add.centrality.variables.to.data.frame(wordvars,
#'   "centrality_eigen", freeassociationdata)
#'   df.with.centralities2 = add.centrality.variables.to.data.frame(wordvars,
#'   c("centrality_authority", "centrality_betweenness"), freeassociationdata)
#'   df.with.centralities3 = add.centrality.variables.to.data.frame(wordvars,
#'   "all", freeassociationdata)
#' @export
#'
add.centrality.variables.to.data.frame <- function(wordvars, centralityfunctionstr, data.df, iscircled = FALSE, verbose = FALSE){

  data.frame.to.return = data.df

  if(length(centralityfunctionstr) == 1){
    if(centralityfunctionstr == "all"){
    centralityfunctionstr = c("centrality_alpha", "centrality_authority", "centrality_betweenness", "centrality_closeness", "centrality_pagerank", "centrality_eigen")
    }
  }

  for (acentrality in centralityfunctionstr) {
    skip_to_next <- FALSE
    tryCatch(
      {
        centralityfunction = NULL

        switch(acentrality,
               centrality_alpha={centralityfunction = tidygraph::centrality_alpha},
               centrality_authority={centralityfunction = tidygraph::centrality_authority},
               centrality_betweenness={centralityfunction = tidygraph::centrality_betweenness},
               centrality_closeness={centralityfunction = tidygraph::centrality_closeness},
               centrality_pagerank={centralityfunction = tidygraph::centrality_pagerank},
               centrality_eigen={centralityfunction = tidygraph::centrality_eigen})

        if(!is.null(centralityfunction) & verbose){message(paste("Centrality ", acentrality, " is accepted and processed.", sep = ""))}

        if(is.null(centralityfunction)){
          message(paste("The centrality \"", acentrality,
                        "\" is not an acceptable entry. Please, provide one of the following:\n1. all, ",
                        "\n2. centrality_alpha, ", "\n3. centrality_authority, ", "\n4. centrality_betweenness, ",
                        "\n5. centrality_closeness, ", "\n6. centrality_pagerank, ", "\n7. centrality_eigen.\n\n"))
        }
        else{
          the.graph.corresponding.to.the.centrality = get.graph.1.n.group.centrality.with.function(wordvars, centralityfunction(), data.df, iscircled = iscircled)
          awordreport = the.graph.corresponding.to.the.centrality$data[, c("name", "centrality")]
          colnames(awordreport)[which(names(awordreport) == "centrality")] <- acentrality
          for (avar in wordvars) {
            anewvariable = create.centrality.variable(acentrality, data.frame.to.return[[avar]], awordreport, verbose = verbose)
            data.frame.to.return = cbind(data.frame.to.return, anewvariable)
            colnames(data.frame.to.return)[ncol(data.frame.to.return)] <- paste(avar, "_", acentrality, sep = "")
          }
        }
      },
      error = function(e) {
        message(paste("Error with the centrality ", acentrality, ", error message: ", e, sep = ""))
        skip_to_next <<- TRUE
      }
    )
    if(skip_to_next) { next }
  }
add.centrality.variables.to.data.frame <- data.frame.to.return
}




#' Remove all the multiple blanks of the selected R output, in order to copied correctly to LibreOffice Calc.
#'
#' @param afile A file
#' @return The same file with reduced blank spaces.
#' @examples
#'     allgraphs = get.all.graphs(c("diet1stword", "diet2ndword",
#'     "diet3rdword"), freeassociationdata)
#'     graph_plot(allgraphs$graphs$centrality_authority)
#' @export
prepare.file = function(afile){
  # Removal of gaps and change "." in "," to identify numbers ...
  output.with.blanks = readLines(afile,-1)
  for (i in 1:length(output.with.blanks)){
    for (blanks in 1:30) {
      output.with.blanks[i] = gsub("  ", " ", output.with.blanks[i])
    }
    output.with.blanks[i] = chartr(old = ".", new = ",", output.with.blanks[i])
  }
  # Save the file ...
  writeLines(output.with.blanks, afile)
}


