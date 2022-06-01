library(tidyverse)

# list files
f <- list.files("ReN_anno_2019-08-14/", full.names = T)

for(j in 1:length(f)) {
  
  # read data
  d <- readLines(f[j])
  
  # get metadata
  header_name <- gsub("\".*", "", gsub(".*name=\"", "", grep("<cora-header name", d, value = T)))
  
  # get header
  header_start <- grep("<header", d)
  header_end   <- grep("</header", d)
  hdr <- d[header_start:header_end]
  
  
  
  hdr <- sub(":", "=\"", hdr)
  hdr <- gsub("(?<=$)", "\"", hdr, perl = T)
  text_header <- paste0("<text id=\"", header_name, "\"", gsub("text=\"", "text_name=\"", gsub("</?header>", "", paste0(hdr, collapse = " "))), ">", collapse = " ")
  
  # get tokens
  token_start <- grep("<token id", d)
  token_end   <- grep("</token", d)
  
  anno_start <- grep("<anno", d)
  
  
  # empty table for dipl, lemma, POS, morph
  cur_tbl <- tibble(
    word      = character(length(anno_start)),
    token_id  = character(length(anno_start)),
    dipl      = character(length(anno_start)),
    lemma     = character(length(anno_start)),
    pos       = character(length(anno_start)),
    morph     = character(length(anno_start)),
    lemma_wsd = character(length(anno_start)),
    bound_sent = character(length(anno_start)),
  )
  
  
  # extract values
  
  for(i in 1:length(anno_start)) {
    cur <- d[anno_start[i]:token_end[which(token_end>anno_start[i])][1]]
    
    # cur <-d[token_start[i]:token_end[i]]
    
    
    
    if(length(grep(".*<anno .* id=\"", cur)) > 0) {
      cur_tbl$token_id[i] <- gsub("\".*", "", gsub(".*id=\"", "", grep("<anno .* id=\"", cur, value = T)[1])) # gsub("\".*", "", gsub(".*<token id=\"", "", cur[grep("token id", cur)]))
    } else {
      cur_tbl$token_id[i] <- "-"
    }
    
    if(length(grep(".*<anno ascii=\".*trans", cur)) > 0) {
      
      cur_tbl$dipl[i] <- gsub("\".*", "", gsub(".*trans=\"", "", grep("anno ascii.*trans=\"", cur, value = T)[1])) # gsub("\".*", "", gsub(".*trans=\"", "", cur[grep("token id", cur)]))
    } else {
      cur_tbl$dipl[i] <- "-"
    }
    
    if(length(grep(".*<anno ascii=\".*trans", cur)) > 0) {
      
      cur_tbl$word[i] <- gsub("\".*", "", gsub(".*trans=\"", "", grep("anno ascii.*trans=\"", cur, value = T)[1])) # gsub("\".*", "", gsub(".*trans=\"", "", cur[grep("token id", cur)]))
    } else {
      cur_tbl$word[i] <- "-"
    }
    
    if(length(grep(".*<lemma tag=\"", cur)) > 0) {
      cur_tbl$lemma[i] <- gsub("\".*", "", gsub(".*<lemma tag=\"", "", cur[grep("lemma tag", cur)[1]]))
    } else {
      cur_tbl$lemma[i] <- "-"
    }
    
    if(length(grep(".*<lemma_wsd tag=\"", cur)) > 0) {
      cur_tbl$lemma_wsd[i] <- gsub("\".*", "", gsub(".*<lemma_wsd tag=\"", "", cur[grep("lemma_wsd tag", cur)[1]]))
    } else {
      cur_tbl$lemma_wsd[i] <- "-"
    }
    
    if(length(grep(".*<morph tag=\"", cur)) > 0) {
      cur_tbl$morph[i] <- gsub("\".*", "", gsub(".*<morph tag=\"", "", cur[grep("morph tag", cur)[1]]))
    } else {
      cur_tbl$morph[i] <- "-"
    }
    
    if(length(grep(".*<pos tag=\"", cur)) > 0) {
      cur_tbl$pos[i] <- gsub("\".*", "", gsub(".*<pos tag=\"", "", cur[grep("pos tag", cur)[1]]))
    } else {
      cur_tbl$pos[i] <- "-"
    }
    
    if(length(grep(".*<bound_sent tag=\"", cur)) > 0) {
      cur_tbl$bound_sent[i] <- gsub("\".*", "", gsub(".*<bound_sent tag=\"", "", cur[grep("bound_sent tag", cur)[1]]))
    } else {
      cur_tbl$bound_sent[i] <- "-"
    }
    
    # print(i)
    
  }
  
  
  # create CWB file
  
  # file header
  
  if(j == 1) {
    write.table(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<corpus>"),
                "ren.vrt", row.names = F, col.names = F, quote = F)
  }
  
  write.table(text_header,
              "ren.vrt", row.names = F, col.names = F, quote = F, append = T)
  write.table(cur_tbl,
              "ren.vrt", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  write.table("</text>",
              "ren.vrt", row.names = F, col.names = F, quote = F, append = T)
  
  if(j == length(f)) {
    write.table("</corpus>",
                "ren.vrt", row.names = F, col.names = F, quote = F, append = T)
  }
  
  print(j)
  
}


# In a second step, we add sentence boundaries
# to the data. (This could have been done before,
# but well...)
rm(list = ls())
d <- readLines("ren.vrt")

# function for inserting line
insert_line <- function(vec, position, what) {
  vec1 <- vec[1:(position-1)]
  vec2 <- vec[position:length(vec)]
  
  return(c(vec1, what, vec2))
  
}

# find start and end of texte
text_start <- grep("<text id", d)
text_end   <- grep("</text", d)

# insert sentence start tag at the beginning
# of the text and sentence end tag at its end

for(i in 1:length(text_start)) {
  d <- insert_line(d, (text_start[i]+1), "<s>")
  d <- insert_line(d, (text_end[i]+1), "</s>")
  text_start <- text_start+2
  text_end   <- text_end+2
}

# insert sentence tags in-between after 
# tokens with sent="Satz", except in cases where
# the </s> tag already follows
find_satz <- grep("Satz$", d)


# remove cases where item after find_satz is already </s>
find_satz <- find_satz[-which(d[find_satz+1] == "</s>")]

for(i in 1:length(find_satz)) {
  d <- insert_line(d, (find_satz[i]+1), "</s>")
  find_satz <- find_satz + 1
  d <- insert_line(d, (find_satz[i]+1), "<s>")
  find_satz <- find_satz + 1
  print(i)
}

write.table(d, "ren_sentence.vrt", row.names = F, col.names = F, quote = F, append = F)

# 
# 
# # read data
# d <- readLines(f[1])
# 
# # get metadata
# header_name <- gsub("\".*", "", gsub(".*name=\"", "", grep("<cora-header name", d, value = T)))
# 
# # get header
# header_start <- grep("<header", d)
# header_end   <- grep("</header", d)
# hdr <- d[header_start:header_end]
# 
# 
# 
# hdr <- sub(":", "=\"", hdr)
# hdr <- gsub("(?<=$)", "\"", hdr, perl = T)
# text_header <- paste0("<text id=\"", header_name, "\"", gsub("text=\"", "text_name=\"", gsub("</?header>", "", paste0(hdr, collapse = " "))), ">", collapse = " ")
# 
# # get tokens
# token_start <- grep("<token id", d)
# token_end   <- grep("</token", d)
# 
# # empty table for dipl, lemma, POS, morph
# cur_tbl <- tibble(
#   word     = character(length(token_start)),
#   token_id = character(length(token_start)),
#   dipl     = character(length(token_start)),
#   lemma    = character(length(token_start)),
#   pos      = character(length(token_start)),
#   morph    = character(length(token_start))
# )
# 
# 
# # extract values
# 
# for(i in 1:length(token_start)) {
#   cur <-d[token_start[i]:token_end[i]]
#   
#   if(length(grep(".*<token id=\"", cur)) > 0) {
#     cur_tbl$token_id[i] <- gsub("\".*", "", gsub(".*<token id=\"", "", cur[grep("token id", cur)]))
#   } else {
#     cur_tbl$token_id[i] <- "-"
#   }
#   
#   if(length(grep(".*<token id=\".*trans", cur)) > 0) {
#     cur_tbl$dipl[i] <- gsub("\".*", "", gsub(".*trans=\"", "", cur[grep("token id", cur)]))
#   } else {
#     cur_tbl$dipl[i] <- "-"
#   }
#   
#   if(length(grep(".*<token id=\".*trans", cur)) > 0) {
#     cur_tbl$word[i] <- gsub("\".*", "", gsub(".*trans=\"", "", cur[grep("token id", cur)]))
#   } else {
#     cur_tbl$word[i] <- "-"
#   }
#   
#   if(length(grep(".*<lemma tag=\"", cur)) > 0) {
#     cur_tbl$lemma[i] <- gsub("\".*", "", gsub(".*<lemma tag=\"", "", cur[grep("lemma tag", cur)]))
#   } else {
#     cur_tbl$lemma[i] <- "-"
#   }
#   
#   if(length(grep(".*<morph tag=\"", cur)) > 0) {
#     cur_tbl$morph[i] <- gsub("\".*", "", gsub(".*<morph tag=\"", "", cur[grep("morph tag", cur)]))
#   } else {
#     cur_tbl$morph[i] <- "-"
#   }
#   
#   if(length(grep(".*<pos tag=\"", cur)) > 0) {
#     cur_tbl$pos[i] <- gsub("\".*", "", gsub(".*<pos tag=\"", "", cur[grep("pos tag", cur)]))
#   } else {
#     cur_tbl$pos[i] <- "-"
#   }
#   
#   
#   
# }
# 
# 
# # create CWB file
# 
# 
# 
# # file header
# write.table(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<corpus>"),
#             "ren.vrt", row.names = F, col.names = F, quote = F)
# write.table(text_header,
#             "ren.vrt", row.names = F, col.names = F, quote = F, append = T)
# write.table(cur_tbl,
#             "ren.vrt", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
# write.table(c("</text>", "</corpus>"),
#             "ren.vrt", row.names = F, col.names = F, quote = F, append = T)
# 
# 
# 
# 
# cur <-d[token_start[1]:token_end[1]]
# 
# if(length(grep(".*<token id=\"", cur)) > 0) {
#   cur_tbl$token_id[i] <- gsub("\".*", "", gsub(".*<token id=\"", "", cur[grep("token id", cur)]))
# } else {
#   cur_tbl$token_id[i] <- "-"
# }
# 
# if(length(grep(".*<token id=\".*trans", cur)) > 0) {
#   cur_tbl$dipl[i] <- gsub("\".*", "", gsub(".*trans=\"", "", cur[grep("token id", cur)]))
# } else {
#   cur_tbl$dipl[i] <- "-"
# }
# 
# if(length(grep(".*<lemma tag=\"", cur)) > 0) {
#   cur_tbl$lemma[i] <- gsub("\".*", "", gsub(".*<lemma tag=\"", "", cur[grep("lemma tag", cur)]))
# } else {
#   cur_tbl$lemma[i] <- "-"
# }
# 
# if(length(grep(".*<morph tag=\"", cur)) > 0) {
#   cur_tbl$morph[i] <- gsub("\".*", "", gsub(".*<morph tag=\"", "", cur[grep("morph tag", cur)]))
# } else {
#   cur_tbl$morph[i] <- "-"
# }
# 
# if(length(grep(".*<pos tag=\"", cur)) > 0) {
#   cur_tbl$morph[i] <- gsub("\".*", "", gsub(".*<pos tag=\"", "", cur[grep("pos tag", cur)]))
# } else {
#   cur_tbl$morph[i] <- "-"
# }
# 
# 
