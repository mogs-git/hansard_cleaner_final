# Hansard cleaner script created by Matt Chaib (mattchaib.com)

# FUNCTIONS ---------------------------------------------------------------

create_header_table <- function(hansard_plaintext) {
  tibble(headers = unlist(str_extract_all(hansard_plaintext, "\n[^\n\r]*\r"))) %>% # pull out all headers
    mutate(possible_name = str_length(headers) < 150 & !str_detect(headers, "\r\n\r\n")) %>% # mark those short enough to be a name
    filter(possible_name)
}

condense_header_table <- function(header_table) {
  header_table %>% 
    mutate(headers = str_sub(headers, start = 2, end = -2), # chop off escape characters
           first_word = str_sub(str_extract(headers, "[a-zA-Z]*\\W"), 1, -2), # pull out first word of header
           first_two_words = str_sub(str_extract(headers, "[a-zA-Z]*\\W[a-zA-Z'-]*"), 1, -1)) %>% # pull out first two words of header
    group_by(first_word) %>% 
    nest() %>%
    mutate(n_obs = map_int(data, nrow), first_two_word = map(data, "first_two_words"), entire_title = map(data, "headers")) 
}

peers_from_condensed_table <- function(c_ht) {
  titles <- c("Lord", "The Lord", "The Advocate", "The Advocate-General", "The Earl", "Earl", "The Countess","Countess","Archbishop", "The Archbishop","The Parliamentary","Viscount","Baroness", "The Baroness")
  
  peers_identified <- c_ht %>%
    select(-data, -n_obs) %>%
    unnest() %>%
    filter(first_word %in% titles | first_two_word %in% titles) 
}

mark_peer_metadata <- function(peers_id) {
  peers_id %>%
    mutate(contains_party = str_count(entire_title, "\\(")>0,
           awkward = str_count(entire_title, "\\(")==2)
}

extract_base_names <- function(peers_id) {
  peers_id %>% 
    mutate(base_name = case_when(
      awkward ~ str_sub(str_extract(entire_title, "\\([^)]+\\)"), 2, -2), # if two sets of brackets, get name from first set
      !awkward & contains_party ~ str_sub(str_extract(entire_title, "[a-zA-Z ',\\-]+"), 1, -2), # if party, ignore party
      !awkward & !contains_party ~ entire_title # if just a name, just use the name
    ))
}

create_party_id_table <- function(peers_id) {
  out <- peers_id %>%
    filter(contains_party) %>%
    mutate(party = str_extract(entire_title, "\\([a-zA-Z-]*\\)")) %>%
    distinct(base_name, party) 
  
  out
}

bind_party <- function(peers_id, party_id) {
  printf <- function(...) invisible(print(sprintf(...)))
  
  out <- peers_id %>%
    select(entire_title, base_name) %>%
    left_join(party_id, by = "base_name") %>%
    mutate(party = ifelse(str_detect(base_name, "Bishop|Archbishop"), "spiritual", party))
  
  n_unassigned <- sum(is.na(out$party))
  
  if (n_unassigned == 0) {
    printf("All Peers assigned to a party!")
  } else {
    printf("%i Peers could not be assigned to a party. Check is.na(.$party).", n_unassigned)
  }
  
  return(out)
}

correct_parties_from_wiki <- function(peers) {
  printf <- function(...) invisible(print(sprintf(...)))
  
  load_lord_wiki_data <- function() {
    lords <- readRDS("data//lords_table.RDAT")
    deceased <- readRDS("data//lords_deceased_table.RDAT")
    loa <- readRDS("data//lords_leave_table.RDAT")
    lords %<>% select(Peer, Party) %>% rename(name = Peer, party = Party)
    deceased %<>% select(Name, Party) %>% rename(name = Name, party = Party)
    loa %<>% select(Lord, Party) %>% rename(name = Lord, party = Party)
    wiki_ids <- bind_rows(lords, deceased, loa)  
  }
  wiki_ids <- load_lord_wiki_data()
  # Party abbreviations
  abbs <- tibble(party = c("Conservative", "Labour", "Liberal Democrats", "Crossbencher", "Ulster Unionist", "Green", "Plaid Cymru", "spiritual", "Non-affiliated"), party_missing =  c("(Con)", "(Lab)", "(LD)", "(CB)", "(UUP)", "(GP)", "(PC)", "(spiritual)", "(Non-Afl)"))
  
  wiki_ids <- wiki_ids %>% left_join(abbs, by = "party") %>% select(-party)
  
  missing_party <- peers %>%
    filter(is.na(party))
  
  found_in_wiki <- wiki_ids %>%
    filter(name %in% peers$base_name) %>%
    rename(base_name=name)
  
  out <- peers %>% 
    left_join(found_in_wiki, by = "base_name") %>%
    mutate(has_missing_party = is.na(party)) %>%
    mutate(party = ifelse(has_missing_party, party_missing, party))
  
  n_unassigned <- sum(is.na(out$party))
  
  if (n_unassigned == 0) {
    printf("All Peers assigned to a party after using wiki!")
  } else {
    printf("%i Peers could not be assigned to a party AFTER USING WIKI. Check is.na(.$party).", n_unassigned)
  }
  
  return(out)
}

add_gender <- function(df) {
  printf <- function(...) invisible(print(sprintf(...)))
  
  gender_vector <- vector("character", length(df$base_name))
  for (i in seq_along(df$base_name)) {
    if (str_detect(df$base_name[i], "Lord|Viscount|Archbishop|Bishop|Earl")) {
      gender_vector[i] <- "male"
    } else if (str_detect(df$base_name[i], "Baroness|Lady|Countess|Duchess")){
      gender_vector[i] <- "female"
    } else {
      gender_vector[i] <- NA
    }
  }
  
  out <- df %>% add_column(gender = gender_vector)
  
  n_unassigned <- sum(is.na(out$gender))
  
  if (n_unassigned == 0) {
    printf("All Peers assigned to a gender!")
  } else {
    printf("%i Peers could not be assigned to a gender. Check is.na(.$gender).", n_unassigned)
  }
  
  return(out)
}

bind_text <- function(hansard_plaintext, peers, remove_exclamations=T) {
  
  # First task, find all headers and their start and end locations
  loc <- tibble(headers = str_sub(unlist(str_extract_all(hansard_plaintext, "\n[^\n\r]*\r")), start = 2, end = -2),
                start = str_locate_all(hansard_plaintext, "\n[^\n\r]*\r")[[1]][,1],
                end = str_locate_all(hansard_plaintext, "\n[^\n\r]*\r")[[1]][,2]) 
  
  # extract the headers which we have previously confirmed as belonging to peers
  peer_headers <- unique(peers$entire_title) # all headers corresponding to a peer
  
  # Using the headers assigned to peers and their locations in the text,
  # assume that all the text between these headers belongs to a single speech for that peer, and
  # assign it to that Lord.
  peers <- distinct(peers)
  
  text <- loc %>%
    filter(headers %in% peer_headers) %>%
    mutate(speech_start = end, speech_end = lead(start),
           speech_end = ifelse(is.na(speech_end), str_length(hansard_plaintext), speech_end)) %>% # If last speech, it ends at the end of the hansard (bit sketchy)
    select(-start, -end) %>%
    mutate(text = str_sub(hansard_plaintext, speech_start, speech_end),
           text = str_replace_all(text, "\\n|\\r", ""),
           text = str_replace_all(text, "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", "")) %>%
    select(-speech_start, -speech_end) 
  
  # Now clean up this table using the peers tibble
  
  text <- text %>%
    rename(entire_title=headers) %>%
    left_join(peers, by = "entire_title") %>%
    rename(name=base_name) %>%
    select(name, text, party, gender) %>%
    {if(remove_exclamations) filter(., !str_detect(text, "^My Lords—$")) else .} %>%
    mutate(speech_id = row_number())
  
  return(text)
}

get_hansard_key <- function(hansard_filepath) {
  tibble(hansard_key = str_sub(read_file(hansard_filepath), 1, 200),
         hansard_start_time = str_extract(hansard_key, "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"),
         hansard_date= str_extract(hansard_filepath, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
         hansard_datetime = map2_chr(hansard_date, hansard_start_time, ~str_c(.x, " ", .y)),
         hansard_datetime2 = as.POSIXct(hansard_datetime,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
  )
}

# Process Hansard

process_hansard <- function(hansard_plaintext) {
  
  print(1)
  header_table <- create_header_table(hansard_plaintext)
  
  print(2)
  c_ht <- condense_header_table(header_table)
  
  print(3)
  peers_identified <- peers_from_condensed_table(c_ht)
  
  print(4)
  peers_identified <- mark_peer_metadata(peers_identified)
  
  print(5)
  peers_identified <- extract_base_names(peers_identified)
  
  print(6)
  party_id <- create_party_id_table(peers_identified)
  
  print(7)
  peers <- bind_party(peers_identified, party_id)
  
  print(8)
  peers <- correct_parties_from_wiki(peers)
  
  print(9)
  peers <- add_gender(peers)
  
  print(10)
  speeches <- bind_text(hansard_plaintext, peers)
  
  print("Processing Completed")
  
  return(speeches)
}

read_hansards <- function(hansard_filepaths, include_key_as_metadata=F, bind_metadata=F) {
  printf <- function(...) invisible(print(sprintf(...)))
  
  outlist <- list()
  
  for (i in seq_along(hansard_filepaths)) {
    print(i) 
    
    h <- read_file(hansard_filepaths[[i]])
    
    outlist[[i]] <- process_hansard(h)
    
    printf("!!!! Hansard %i completed !!!!", i)
  }
  
  if(include_key_as_metadata & !bind_metadata) {
    meta_list <- list()
    for(i in seq_along(hansard_filepaths)) {
      
      meta_list[[i]] <- get_hansard_key(hansard_filepaths[[i]])
    }
    print("Metadata added!")
    return(list(outlist, meta_list))
  }
  
  if(include_key_as_metadata & bind_metadata) {
    meta_list <- list()
    for(i in seq_along(hansard_filepaths)) {
      
      meta_list[[i]] <- get_hansard_key(hansard_filepaths[[i]])
    }
    print("Metadata added!")
    out <-list(outlist, meta_list)
    
    out <- map2(outlist, meta_list, cbind)
    return(out)
  }
  
  outlist
}


# Example workflow --------------------------------------------------------

library(tidyverse)

hansard_filepath <- "data/European Union (Notification of Withdrawal) Bill 2017-03-01 (1).txt"
dat <- read_hansards(hansard_filepath, include_key_as_metadata = T)

# Or read all files from a directory

hdpath <- "hansard_directory"
filenames <- list.files(hdpath)
dat <- read_hansards(hansard_filepath)

# If you include metadata, you will get an output of two lists, one containing the cleaned hansards,
# A second one containing the metadata (dates+times of speech) for that hansard which is extracted from the filenames and 
# text itself. 

# a third option is to switch on both include metadata and bind metadata, which will output
# a single list of tidy hansard data frames with the metadata added as extra columns, but this
# redundancy increases the size of the data so might cause problems if you're working with lots of
# hansards. 
