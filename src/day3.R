
day3 = function() {
  input = readInputFile(3)
  
  q1 = input %>%
    mutate(front = substr(X1, 1, nchar(X1)/2)) %>% 
    mutate(back = substr(X1, nchar(X1)/2 + 1, nchar(X1))) %>%
    mutate(target = map2_chr(front, back, ~intersect(strsplit(.x, "")[[1]], strsplit(.y, "")[[1]]))) %>%
    mutate(priority = if_else(is.na(match(target, letters)), as.numeric(match(target, LETTERS) + 26), as.numeric(match(target, letters))))
  
  a1 = sum(q1$priority)  
  
  elfGroups = rep(1:(nrow(input)/3), each = 3)
  q2 = input %>%
    mutate(groupId = elfGroups) %>% 
    group_by(groupId) %>%
    mutate(elf = row_number()) %>%
    pivot_wider(id_cols = groupId, values_from = X1, names_from = elf, names_prefix = "elf_") %>%
    mutate(target = pmap_chr(list(elf_1, elf_2, elf_3), function(a, b, c) intersect(intersect(strsplit(a, "")[[1]], strsplit(b, "")[[1]]), strsplit(c, "")[[1]]))) %>%
    mutate(priority = if_else(is.na(match(target, letters)), as.numeric(match(target, LETTERS) + 26), as.numeric(match(target, letters))))
  
  a2 = sum(q2$priority)  
  return(returnAnswerRow(3, a1, a2))
}

