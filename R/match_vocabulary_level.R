#'A Self-made Function
#'
#' This function allows you to match the vocabulary level of your data.
#' @param data A dataframe
#' @keywords match vocabulary level
#' @export
#' @examples
#' match_vocabulary_level()

match_vocabulary_level = function(data){
  nikl_vocabulary = read_excel("/Users/ChiangJou/Desktop/paper/paper_input/2017_korean_vocabulary_grammar.xlsx", sheet = "vocabulary", range = cell_cols("C:E"))
  names(nikl_vocabulary) = c("level", "word", "pos")

  data$target_word = "NA"
  data$target_level = "NA"

  # 보통명사, 고유명사, 대명사, 일반의존명사, 단위의존명사
  N = data[which(data$pos == "NNG" | data$pos == "NNP" |
                   data$pos == "NP" | data$pos == "NNB" |
                   data$pos == "NNM"), ]

  nikl_N = nikl_vocabulary[grep("명사", nikl_vocabulary$pos),]
  nikl_N$target_word = gsub("(\\W+)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_N$word)

  nikl_N_1 = nikl_N[which(nikl_N$level == "1급"), ]
  nikl_N_2 = nikl_N[which(nikl_N$level == "2급"), ]
  nikl_N_3 = nikl_N[which(nikl_N$level == "3급"), ]
  nikl_N_4 = nikl_N[which(nikl_N$level == "4급"), ]
  nikl_N_5 = nikl_N[which(nikl_N$level == "5급"), ]
  nikl_N_6 = nikl_N[which(nikl_N$level == "6급"), ]

  for(i in 1:nrow(nikl_N_6)){
    if(length(which(N$word == nikl_N_6$target_word[i])) == 0) next
    N[which(N$word == nikl_N_6$target_word[i]),]$target_word = nikl_N_6$word[i]
    N[which(N$word == nikl_N_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_N_5)){
    if(length(which(N$word == nikl_N_5$target_word[i])) == 0) next
    N[which(N$word == nikl_N_5$target_word[i]),]$target_word = nikl_N_5$word[i]
    N[which(N$word == nikl_N_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_N_4)){
    if(length(which(N$word == nikl_N_4$target_word[i])) == 0) next
    N[which(N$word == nikl_N_4$target_word[i]),]$target_word = nikl_N_4$word[i]
    N[which(N$word == nikl_N_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_N_3)){
    if(length(which(N$word == nikl_N_3$target_word[i])) == 0) next
    N[which(N$word == nikl_N_3$target_word[i]),]$target_word = nikl_N_3$word[i]
    N[which(N$word == nikl_N_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_N_2)){
    if(length(which(N$word == nikl_N_2$target_word[i])) == 0) next
    N[which(N$word == nikl_N_2$target_word[i]),]$target_word = nikl_N_2$word[i]
    N[which(N$word == nikl_N_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_N_1)){
    if(length(which(N$word == nikl_N_1$target_word[i])) == 0) next
    N[which(N$word == nikl_N_1$target_word[i]),]$target_word = nikl_N_1$word[i]
    N[which(N$word == nikl_N_1$target_word[i]),]$target_level = "1"
  }

  # 수사
  NR = data[which(data$pos == "NR"), ]

  nikl_NR = nikl_vocabulary[grep("수사", nikl_vocabulary$pos),]
  nikl_NR$target_word = gsub("(\\W+)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_NR$word)

  nikl_NR_1 = nikl_NR[which(nikl_NR$level == "1급"), ]
  nikl_NR_2 = nikl_NR[which(nikl_NR$level == "2급"), ]
  nikl_NR_3 = nikl_NR[which(nikl_NR$level == "3급"), ]
  nikl_NR_4 = nikl_NR[which(nikl_NR$level == "4급"), ]
  nikl_NR_5 = nikl_NR[which(nikl_NR$level == "5급"), ]
  nikl_NR_6 = nikl_NR[which(nikl_NR$level == "6급"), ]

  for(i in 1:nrow(nikl_NR_6)){
    if(length(which(NR$word == nikl_NR_6$target_word[i])) == 0) next
    NR[which(NR$word == nikl_NR_6$target_word[i]),]$target_word = nikl_NR_6$word[i]
    NR[which(NR$word == nikl_NR_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_NR_5)){
    if(length(which(NR$word == nikl_NR_5$target_word[i])) == 0) next
    NR[which(NR$word == nikl_NR_5$target_word[i]),]$target_word = nikl_NR_5$word[i]
    NR[which(NR$word == nikl_NR_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_NR_4)){
    if(length(which(NR$word == nikl_NR_4$target_word[i])) == 0) next
    NR[which(NR$word == nikl_NR_4$target_word[i]),]$target_word = nikl_NR_4$word[i]
    NR[which(NR$word == nikl_NR_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_NR_3)){
    if(length(which(NR$word == nikl_NR_3$target_word[i])) == 0) next
    NR[which(NR$word == nikl_NR_3$target_word[i]),]$target_word = nikl_NR_3$word[i]
    NR[which(NR$word == nikl_NR_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_NR_2)){
    if(length(which(NR$word == nikl_NR_2$target_word[i])) == 0) next
    NR[which(NR$word == nikl_NR_2$target_word[i]),]$target_word = nikl_NR_2$word[i]
    NR[which(NR$word == nikl_NR_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_NR_1)){
    if(length(which(NR$word == nikl_NR_1$target_word[i])) == 0) next
    NR[which(NR$word == nikl_NR_1$target_word[i]),]$target_word = nikl_NR_1$word[i]
    NR[which(NR$word == nikl_NR_1$target_word[i]),]$target_level = "1"
  }

  # 동사, 보조동사
  VV = data[which(data$pos == "VV" | data$pos == "VXV"), ]

  nikl_VV = nikl_vocabulary[grep("동사", nikl_vocabulary$pos),]
  nikl_VV$target_word = gsub("(\\W+)다(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_VV$word)

  nikl_VV_1 = nikl_VV[which(nikl_VV$level == "1급"), ]
  nikl_VV_2 = nikl_VV[which(nikl_VV$level == "2급"), ]
  nikl_VV_3 = nikl_VV[which(nikl_VV$level == "3급"), ]
  nikl_VV_4 = nikl_VV[which(nikl_VV$level == "4급"), ]
  nikl_VV_5 = nikl_VV[which(nikl_VV$level == "5급"), ]
  nikl_VV_6 = nikl_VV[which(nikl_VV$level == "6급"), ]

  for(i in 1:nrow(nikl_VV_6)){
    if(length(which(VV$word == nikl_VV_6$target_word[i])) == 0) next
    VV[which(VV$word == nikl_VV_6$target_word[i]),]$target_word = nikl_VV_6$word[i]
    VV[which(VV$word == nikl_VV_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_VV_5)){
    if(length(which(VV$word == nikl_VV_5$target_word[i])) == 0) next
    VV[which(VV$word == nikl_VV_5$target_word[i]),]$target_word = nikl_VV_5$word[i]
    VV[which(VV$word == nikl_VV_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_VV_4)){
    if(length(which(VV$word == nikl_VV_4$target_word[i])) == 0) next
    VV[which(VV$word == nikl_VV_4$target_word[i]),]$target_word = nikl_VV_4$word[i]
    VV[which(VV$word == nikl_VV_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_VV_3)){
    if(length(which(VV$word == nikl_VV_3$target_word[i])) == 0) next
    VV[which(VV$word == nikl_VV_3$target_word[i]),]$target_word = nikl_VV_3$word[i]
    VV[which(VV$word == nikl_VV_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_VV_2)){
    if(length(which(VV$word == nikl_VV_2$target_word[i])) == 0) next
    VV[which(VV$word == nikl_VV_2$target_word[i]),]$target_word = nikl_VV_2$word[i]
    VV[which(VV$word == nikl_VV_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_VV_1)){
    if(length(which(VV$word == nikl_VV_1$target_word[i])) == 0) next
    VV[which(VV$word == nikl_VV_1$target_word[i]),]$target_word = nikl_VV_1$word[i]
    VV[which(VV$word == nikl_VV_1$target_word[i]),]$target_level = "1"
  }

  # 형용사, 보조형용사
  VA = data[which(data$pos == "VA" | data$pos == "VXA"), ]

  nikl_VA = nikl_vocabulary[grep("형용사", nikl_vocabulary$pos),]
  nikl_VA$target_word = gsub("(\\W+)다(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_VA$word)

  nikl_VA_1 = nikl_VA[which(nikl_VA$level == "1급"), ]
  nikl_VA_2 = nikl_VA[which(nikl_VA$level == "2급"), ]
  nikl_VA_3 = nikl_VA[which(nikl_VA$level == "3급"), ]
  nikl_VA_4 = nikl_VA[which(nikl_VA$level == "4급"), ]
  nikl_VA_5 = nikl_VA[which(nikl_VA$level == "5급"), ]
  nikl_VA_6 = nikl_VA[which(nikl_VA$level == "6급"), ]

  for(i in 1:nrow(nikl_VA_6)){
    if(length(which(VA$word == nikl_VA_6$target_word[i])) == 0) next
    VA[which(VA$word == nikl_VA_6$target_word[i]),]$target_word = nikl_VA_6$word[i]
    VA[which(VA$word == nikl_VA_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_VA_5)){
    if(length(which(VA$word == nikl_VA_5$target_word[i])) == 0) next
    VA[which(VA$word == nikl_VA_5$target_word[i]),]$target_word = nikl_VA_5$word[i]
    VA[which(VA$word == nikl_VA_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_VA_4)){
    if(length(which(VA$word == nikl_VA_4$target_word[i])) == 0) next
    VA[which(VA$word == nikl_VA_4$target_word[i]),]$target_word = nikl_VA_4$word[i]
    VA[which(VA$word == nikl_VA_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_VA_3)){
    if(length(which(VA$word == nikl_VA_3$target_word[i])) == 0) next
    VA[which(VA$word == nikl_VA_3$target_word[i]),]$target_word = nikl_VA_3$word[i]
    VA[which(VA$word == nikl_VA_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_VA_2)){
    if(length(which(VA$word == nikl_VA_2$target_word[i])) == 0) next
    VA[which(VA$word == nikl_VA_2$target_word[i]),]$target_word = nikl_VA_2$word[i]
    VA[which(VA$word == nikl_VA_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_VA_1)){
    if(length(which(VA$word == nikl_VA_1$target_word[i])) == 0) next
    VA[which(VA$word == nikl_VA_1$target_word[i]),]$target_word = nikl_VA_1$word[i]
    VA[which(VA$word == nikl_VA_1$target_word[i]),]$target_level = "1"
  }

  # 지정사
  VC = data[which(data$pos == "VCP" | data$pos == "VCN"), ]
  VC[which(VC$word == "이"),]$target_word = "이다"
  VC[which(VC$word == "이"),]$target_level = "1"
  VC[which(VC$word == "아니"),]$target_word = "이 아니다"
  VC[which(VC$word == "아니"),]$target_level = "1"

  # 관형사
  MD = data[which(data$pos == "MDN" | data$pos == "MDT"), ]

  nikl_MD = nikl_vocabulary[grep("관형사", nikl_vocabulary$pos),]
  nikl_MD$target_word = gsub("(\\W+)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_MD$word)

  nikl_MD_1 = nikl_MD[which(nikl_MD$level == "1급"), ]
  nikl_MD_2 = nikl_MD[which(nikl_MD$level == "2급"), ]
  nikl_MD_3 = nikl_MD[which(nikl_MD$level == "3급"), ]
  nikl_MD_4 = nikl_MD[which(nikl_MD$level == "4급"), ]
  nikl_MD_5 = nikl_MD[which(nikl_MD$level == "5급"), ]
  nikl_MD_6 = nikl_MD[which(nikl_MD$level == "6급"), ]

  for(i in 1:nrow(nikl_MD_6)){
    if(length(which(MD$word == nikl_MD_6$target_word[i])) == 0) next
    MD[which(MD$word == nikl_MD_6$target_word[i]),]$target_word = nikl_MD_6$word[i]
    MD[which(MD$word == nikl_MD_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_MD_5)){
    if(length(which(MD$word == nikl_MD_5$target_word[i])) == 0) next
    MD[which(MD$word == nikl_MD_5$target_word[i]),]$target_word = nikl_MD_5$word[i]
    MD[which(MD$word == nikl_MD_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_MD_4)){
    if(length(which(MD$word == nikl_MD_4$target_word[i])) == 0) next
    MD[which(MD$word == nikl_MD_4$target_word[i]),]$target_word = nikl_MD_4$word[i]
    MD[which(MD$word == nikl_MD_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_MD_3)){
    if(length(which(MD$word == nikl_MD_3$target_word[i])) == 0) next
    MD[which(MD$word == nikl_MD_3$target_word[i]),]$target_word = nikl_MD_3$word[i]
    MD[which(MD$word == nikl_MD_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_MD_2)){
    if(length(which(MD$word == nikl_MD_2$target_word[i])) == 0) next
    MD[which(MD$word == nikl_MD_2$target_word[i]),]$target_word = nikl_MD_2$word[i]
    MD[which(MD$word == nikl_MD_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_MD_1)){
    if(length(which(MD$word == nikl_MD_1$target_word[i])) == 0) next
    MD[which(MD$word == nikl_MD_1$target_word[i]),]$target_word = nikl_MD_1$word[i]
    MD[which(MD$word == nikl_MD_1$target_word[i]),]$target_level = "1"
  }

  # 일반부사, 접속부사
  MA = data[which(data$pos == "MAG" | data$pos == "MAC"), ]

  nikl_MA = nikl_vocabulary[grep("부사", nikl_vocabulary$pos),]
  nikl_MA$target_word = gsub("(\\W+)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_MA$word)

  nikl_MA_1 = nikl_MA[which(nikl_MA$level == "1급"), ]
  nikl_MA_2 = nikl_MA[which(nikl_MA$level == "2급"), ]
  nikl_MA_3 = nikl_MA[which(nikl_MA$level == "3급"), ]
  nikl_MA_4 = nikl_MA[which(nikl_MA$level == "4급"), ]
  nikl_MA_5 = nikl_MA[which(nikl_MA$level == "5급"), ]
  nikl_MA_6 = nikl_MA[which(nikl_MA$level == "6급"), ]

  for(i in 1:nrow(nikl_MA_6)){
    if(length(which(MA$word == nikl_MA_6$target_word[i])) == 0) next
    MA[which(MA$word == nikl_MA_6$target_word[i]),]$target_word = nikl_MA_6$word[i]
    MA[which(MA$word == nikl_MA_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_MA_5)){
    if(length(which(MA$word == nikl_MA_5$target_word[i])) == 0) next
    MA[which(MA$word == nikl_MA_5$target_word[i]),]$target_word = nikl_MA_5$word[i]
    MA[which(MA$word == nikl_MA_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_MA_4)){
    if(length(which(MA$word == nikl_MA_4$target_word[i])) == 0) next
    MA[which(MA$word == nikl_MA_4$target_word[i]),]$target_word = nikl_MA_4$word[i]
    MA[which(MA$word == nikl_MA_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_MA_3)){
    if(length(which(MA$word == nikl_MA_3$target_word[i])) == 0) next
    MA[which(MA$word == nikl_MA_3$target_word[i]),]$target_word = nikl_MA_3$word[i]
    MA[which(MA$word == nikl_MA_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_MA_2)){
    if(length(which(MA$word == nikl_MA_2$target_word[i])) == 0) next
    MA[which(MA$word == nikl_MA_2$target_word[i]),]$target_word = nikl_MA_2$word[i]
    MA[which(MA$word == nikl_MA_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_MA_1)){
    if(length(which(MA$word == nikl_MA_1$target_word[i])) == 0) next
    MA[which(MA$word == nikl_MA_1$target_word[i]),]$target_word = nikl_MA_1$word[i]
    MA[which(MA$word == nikl_MA_1$target_word[i]),]$target_level = "1"
  }

  # 감탄사
  IC = data[which(data$pos == "IC"), ]

  nikl_IC = nikl_vocabulary[grep("감탄사", nikl_vocabulary$pos),]
  nikl_IC$target_word = gsub("(\\W+)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_IC$word)

  nikl_IC_1 = nikl_IC[which(nikl_IC$level == "1급"), ]
  nikl_IC_2 = nikl_IC[which(nikl_IC$level == "2급"), ]
  nikl_IC_3 = nikl_IC[which(nikl_IC$level == "3급"), ]
  nikl_IC_4 = nikl_IC[which(nikl_IC$level == "4급"), ]
  nikl_IC_5 = nikl_IC[which(nikl_IC$level == "5급"), ]
  nikl_IC_6 = nikl_IC[which(nikl_IC$level == "6급"), ]

  for(i in 1:nrow(nikl_IC_6)){
    if(length(which(IC$word == nikl_IC_6$target_word[i])) == 0) next
    IC[which(IC$word == nikl_IC_6$target_word[i]),]$target_word = nikl_IC_6$word[i]
    IC[which(IC$word == nikl_IC_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_IC_5)){
    if(length(which(IC$word == nikl_IC_5$target_word[i])) == 0) next
    IC[which(IC$word == nikl_IC_5$target_word[i]),]$target_word = nikl_IC_5$word[i]
    IC[which(IC$word == nikl_IC_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_IC_4)){
    if(length(which(IC$word == nikl_IC_4$target_word[i])) == 0) next
    IC[which(IC$word == nikl_IC_4$target_word[i]),]$target_word = nikl_IC_4$word[i]
    IC[which(IC$word == nikl_IC_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_IC_3)){
    if(length(which(IC$word == nikl_IC_3$target_word[i])) == 0) next
    IC[which(IC$word == nikl_IC_3$target_word[i]),]$target_word = nikl_IC_3$word[i]
    IC[which(IC$word == nikl_IC_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_IC_2)){
    if(length(which(IC$word == nikl_IC_2$target_word[i])) == 0) next
    IC[which(IC$word == nikl_IC_2$target_word[i]),]$target_word = nikl_IC_2$word[i]
    IC[which(IC$word == nikl_IC_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_IC_1)){
    if(length(which(IC$word == nikl_IC_1$target_word[i])) == 0) next
    IC[which(IC$word == nikl_IC_1$target_word[i]),]$target_word = nikl_IC_1$word[i]
    IC[which(IC$word == nikl_IC_1$target_word[i]),]$target_level = "1"
  }

  # 접두사, 접미사
  XP = data[which(data$pos == "XPN" | data$pos == "XPV"), ]
  XS = data[which(data$pos == "XSN" | data$pos == "XSV"
                  | data$pos == "XSA"), ]

  nikl_XP_XS = nikl_vocabulary[grep("접사", nikl_vocabulary$pos),]
  nikl_XP = nikl_XP_XS[grep("(\\W+)[[:punct:]](\\d*)", nikl_XP_XS$word),]
  nikl_XS = nikl_XP_XS[grep("[[:punct:]](\\W+)(\\d*)", nikl_XP_XS$word),]

  nikl_XP$target_word = gsub("(\\W+)[[:punct:]](\\d+)*", "\\1", nikl_XP$word)
  nikl_XS$target_word = gsub("[[:punct:]](\\W+)(\\d)*", "\\1", nikl_XS$word)

  nikl_XP_1 = nikl_XP[which(nikl_XP$level == "1급"), ]
  nikl_XP_2 = nikl_XP[which(nikl_XP$level == "2급"), ]
  nikl_XP_3 = nikl_XP[which(nikl_XP$level == "3급"), ]
  nikl_XP_4 = nikl_XP[which(nikl_XP$level == "4급"), ]
  nikl_XP_5 = nikl_XP[which(nikl_XP$level == "5급"), ]
  nikl_XP_6 = nikl_XP[which(nikl_XP$level == "6급"), ]

  nikl_XS_1 = nikl_XS[which(nikl_XS$level == "1급"), ]
  nikl_XS_2 = nikl_XS[which(nikl_XS$level == "2급"), ]
  nikl_XS_3 = nikl_XS[which(nikl_XS$level == "3급"), ]
  nikl_XS_4 = nikl_XS[which(nikl_XS$level == "4급"), ]
  nikl_XS_5 = nikl_XS[which(nikl_XS$level == "5급"), ]
  nikl_XS_6 = nikl_XS[which(nikl_XS$level == "6급"), ]

  for(i in 1:nrow(nikl_XP_6)){
    if(length(which(XP$word == nikl_XP_6$target_word[i])) == 0) next
    XP[which(XP$word == nikl_XP_6$target_word[i]),]$target_word = nikl_XP_6$word[i]
    XP[which(XP$word == nikl_XP_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_XP_5)){
    if(length(which(XP$word == nikl_XP_5$target_word[i])) == 0) next
    XP[which(XP$word == nikl_XP_5$target_word[i]),]$target_word = nikl_XP_5$word[i]
    XP[which(XP$word == nikl_XP_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_XP_4)){
    if(length(which(XP$word == nikl_XP_4$target_word[i])) == 0) next
    XP[which(XP$word == nikl_XP_4$target_word[i]),]$target_word = nikl_XP_4$word[i]
    XP[which(XP$word == nikl_XP_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_XP_3)){
    if(length(which(XP$word == nikl_XP_3$target_word[i])) == 0) next
    XP[which(XP$word == nikl_XP_3$target_word[i]),]$target_word = nikl_XP_3$word[i]
    XP[which(XP$word == nikl_XP_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_XP_2)){
    if(length(which(XP$word == nikl_XP_2$target_word[i])) == 0) next
    XP[which(XP$word == nikl_XP_2$target_word[i]),]$target_word = nikl_XP_2$word[i]
    XP[which(XP$word == nikl_XP_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_XP_1)){
    if(length(which(XP$word == nikl_XP_1$target_word[i])) == 0) next
    XP[which(XP$word == nikl_XP_1$target_word[i]),]$target_word = nikl_XP_1$word[i]
    XP[which(XP$word == nikl_XP_1$target_word[i]),]$target_level = "1"
  }

  for(i in 1:nrow(nikl_XS_6)){
    if(length(which(XS$word == nikl_XS_6$target_word[i])) == 0) next
    XS[which(XS$word == nikl_XS_6$target_word[i]),]$target_word = nikl_XS_6$word[i]
    XS[which(XS$word == nikl_XS_6$target_word[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_XS_5)){
    if(length(which(XS$word == nikl_XS_5$target_word[i])) == 0) next
    XS[which(XS$word == nikl_XS_5$target_word[i]),]$target_word = nikl_XS_5$word[i]
    XS[which(XS$word == nikl_XS_5$target_word[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_XS_4)){
    if(length(which(XS$word == nikl_XS_4$target_word[i])) == 0) next
    XS[which(XS$word == nikl_XS_4$target_word[i]),]$target_word = nikl_XS_4$word[i]
    XS[which(XS$word == nikl_XS_4$target_word[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_XS_3)){
    if(length(which(XS$word == nikl_XS_3$target_word[i])) == 0) next
    XS[which(XS$word == nikl_XS_3$target_word[i]),]$target_word = nikl_XS_3$word[i]
    XS[which(XS$word == nikl_XS_3$target_word[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_XS_2)){
    if(length(which(XS$word == nikl_XS_2$target_word[i])) == 0) next
    XS[which(XS$word == nikl_XS_2$target_word[i]),]$target_word = nikl_XS_2$word[i]
    XS[which(XS$word == nikl_XS_2$target_word[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_XS_1)){
    if(length(which(XS$word == nikl_XS_1$target_word[i])) == 0) next
    XS[which(XS$word == nikl_XS_1$target_word[i]),]$target_word = nikl_XS_1$word[i]
    XS[which(XS$word == nikl_XS_1$target_word[i]),]$target_level = "1"
  }

  XR = data[which(data$pos == "XR"), ]

  nikl_VA$target_word_XR = gsub("(\\W+)(하다*)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_VA$word)
  nikl_VV$target_word_XR = gsub("(\\W+)(하다*)(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*[[:punct:]]*(\\W+)*(\\d+)*", "\\1", nikl_VV$word)

  nikl_VA_1 = nikl_VA[which(nikl_VA$level == "1급"), ]
  nikl_VA_2 = nikl_VA[which(nikl_VA$level == "2급"), ]
  nikl_VA_3 = nikl_VA[which(nikl_VA$level == "3급"), ]
  nikl_VA_4 = nikl_VA[which(nikl_VA$level == "4급"), ]
  nikl_VA_5 = nikl_VA[which(nikl_VA$level == "5급"), ]
  nikl_VA_6 = nikl_VA[which(nikl_VA$level == "6급"), ]

  nikl_VV_1 = nikl_VV[which(nikl_VV$level == "1급"), ]
  nikl_VV_2 = nikl_VV[which(nikl_VV$level == "2급"), ]
  nikl_VV_3 = nikl_VV[which(nikl_VV$level == "3급"), ]
  nikl_VV_4 = nikl_VV[which(nikl_VV$level == "4급"), ]
  nikl_VV_5 = nikl_VV[which(nikl_VV$level == "5급"), ]
  nikl_VV_6 = nikl_VV[which(nikl_VV$level == "6급"), ]

  for(i in 1:nrow(nikl_VA_6)){
    if(length(which(XR$word == nikl_VA_6$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VA_6$target_word_XR[i]),]$target_word = nikl_VA_6$word[i]
    XR[which(XR$word == nikl_VA_6$target_word_XR[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_VA_5)){
    if(length(which(XR$word == nikl_VA_5$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VA_5$target_word_XR[i]),]$target_word = nikl_VA_5$word[i]
    XR[which(XR$word == nikl_VA_5$target_word_XR[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_VA_4)){
    if(length(which(XR$word == nikl_VA_4$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VA_4$target_word_XR[i]),]$target_word = nikl_VA_4$word[i]
    XR[which(XR$word == nikl_VA_4$target_word_XR[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_VA_3)){
    if(length(which(XR$word == nikl_VA_3$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VA_3$target_word_XR[i]),]$target_word = nikl_VA_3$word[i]
    XR[which(XR$word == nikl_VA_3$target_word_XR[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_VA_2)){
    if(length(which(XR$word == nikl_VA_2$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VA_2$target_word_XR[i]),]$target_word = nikl_VA_2$word[i]
    XR[which(XR$word == nikl_VA_2$target_word_XR[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_VA_1)){
    if(length(which(XR$word == nikl_VA_1$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VA_1$target_word_XR[i]),]$target_word = nikl_VA_1$word[i]
    XR[which(XR$word == nikl_VA_1$target_word_XR[i]),]$target_level = "1"
  }

  for(i in 1:nrow(nikl_VV_6)){
    if(length(which(XR$word == nikl_VV_6$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VV_6$target_word_XR[i]),]$target_word = nikl_VV_6$word[i]
    XR[which(XR$word == nikl_VV_6$target_word_XR[i]),]$target_level = "6"
  }

  for(i in 1:nrow(nikl_VV_5)){
    if(length(which(XR$word == nikl_VV_5$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VV_5$target_word_XR[i]),]$target_word = nikl_VV_5$word[i]
    XR[which(XR$word == nikl_VV_5$target_word_XR[i]),]$target_level = "5"
  }

  for(i in 1:nrow(nikl_VV_4)){
    if(length(which(XR$word == nikl_VV_4$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VV_4$target_word_XR[i]),]$target_word = nikl_VV_4$word[i]
    XR[which(XR$word == nikl_VV_4$target_word_XR[i]),]$target_level = "4"
  }

  for(i in 1:nrow(nikl_VV_3)){
    if(length(which(XR$word == nikl_VV_3$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VV_3$target_word_XR[i]),]$target_word = nikl_VV_3$word[i]
    XR[which(XR$word == nikl_VV_3$target_word_XR[i]),]$target_level = "3"
  }

  for(i in 1:nrow(nikl_VV_2)){
    if(length(which(XR$word == nikl_VV_2$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VV_2$target_word_XR[i]),]$target_word = nikl_VV_2$word[i]
    XR[which(XR$word == nikl_VV_2$target_word_XR[i]),]$target_level = "2"
  }

  for(i in 1:nrow(nikl_VV_1)){
    if(length(which(XR$word == nikl_VV_1$target_word_XR[i])) == 0) next
    XR[which(XR$word == nikl_VV_1$target_word_XR[i]),]$target_word = nikl_VV_1$word[i]
    XR[which(XR$word == nikl_VV_1$target_word_XR[i]),]$target_level = "1"
  }

  vocabulary = rbind(N, NR, VV, VA, VC, MD, MA, IC, XP, XS, XR)
  return(vocabulary)
}
