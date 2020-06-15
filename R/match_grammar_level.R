#'A Self-made Function
#'
#' This function allows you to match the grammar level of your data.
#' @param data A dataframe
#' @keywords match grammar level
#' @export
#' @examples
#' match_grammar_level()

match_grammar_level = function(data){
  J = data[which(data$pos == "JKS" | data$pos == "JKC" |
                   data$pos == "JKG" | data$pos == "JKO" |
                   data$pos == "JKM" | data$pos == "JKI" |
                   data$pos == "JKQ" | data$pos == "JC" |
                   data$pos == "JX"), ]
  EP = data[which(data$pos == "EPH" | data$pos == "EPT" |
                    data$pos == "EPP"), ]
  EF = data[which(data$pos == "EFN" | data$pos == "EFQ" |
                    data$pos == "EFO" | data$pos == "EFA" |
                    data$pos == "EFI" | data$pos == "EFR"), ]
  EC = data[which(data$pos == "ECE" | data$pos == "ECS" |
                    data$pos == "ECD"), ]
  ET = data[which(data$pos == "ETN" | data$pos == "ETD"), ]

  for (i in seq_along(J$word)){
    if (J$word[i] == "이" | J$word[i] == "가"){
      J$target_word[i] = "이"
      J$target_level[i] = "1"
    }else if(J$word[i] == "과" | J$word[i] == "와"){
      J$target_word[i] = "과"
      J$target_level[i] = "1"
    }else if(J$word[i] == "까지"){
      J$target_word[i] = "까지"
      J$target_level[i] = "1"
    }else if(J$word[i] == "께서"){
      J$target_word[i] = "께서"
      J$target_level[i] = "1"
    }else if(J$word[i] == "은" | J$word[i] == "는" | J$word[i] == "ㄴ"){
      J$target_word[i] = "은1"
      J$target_level[i] = "1"
    }else if(J$word[i] == "도"){
      J$target_word[i] = "도"
      J$target_level[i] = "1"
    }else if(J$word[i] == "을" | J$word[i] == "를" | J$word[i] == "ㄹ"){
      J$target_word[i] = "을1"
      J$target_level[i] = "1"
    }else if(J$word[i] == "이랑" | J$word[i] == "랑"){
      J$target_word[i] = "이랑"
      J$target_level[i] = "1"
    }else if(J$word[i] == "으로" | J$word[i] == "로"){
      J$target_word[i] = "으로"
      J$target_level[i] = "1"
    }else if(J$word[i] == "부터"){
      J$target_word[i] = "부터"
      J$target_level[i] = "1"
    }else if(J$word[i] == "에" | J$word[i] == "다가"){
      J$target_word[i] = "에"
      J$target_level[i] = "1"
    }else if(J$word[i] == "에게"){
      J$target_word[i] = "에게"
      J$target_level[i] = "1"
    }else if(J$word[i] == "에서" | J$word[i] == "서"){
      J$target_word[i] = "에서"
      J$target_level[i] = "1"
    }else if(J$word[i] == "의"){
      J$target_word[i] = "의"
      J$target_level[i] = "1"
    }else if(J$word[i] == "하고"){
      J$target_word[i] = "하고"
      J$target_level[i] = "1"
    }else if(J$word[i] == "만"){
      J$target_word[i] = "만"
      J$target_level[i] = "1"
    }else if(J$word[i] == "이다"){
      J$target_word[i] = "이다"
      J$target_level[i] = "1"
    }else if(J$word[i] == "한테"){
      J$target_word[i] = "한테"
      J$target_level[i] = "1"
    }else if(J$word[i] == "보다"){
      J$target_word[i] = "보다"
      J$target_level[i] = "1"
    }else if(J$word[i] == "께"){
      J$target_word[i] == "께"
      J$target_level[i] == "2"
    }else if(J$word[i] == "마다"){
      J$target_word[i] = "마다"
      J$target_level[i] = "2"
    }else if(J$word[i] == "밖에"){
      J$target_word[i] = "밖에"
      J$target_level[i] = "2"
    }else if(J$word[i] == "처럼"){
      J$target_word[i] = "처럼"
      J$target_level[i] = "2"
    }else if(J$word[i] == "에서부터" | J$word[i] == "서부터"){
      J$target_word[i] = "에서부터"
      J$target_level[i] = "2"
    }else if(J$word[i] == "에다가" | J$word[i] == "에다"){
      J$target_word[i] = "에다가"
      J$target_level[i] = "2"
    }else if(J$word[i] == "에게로"){
      J$target_word[i] = "에게로"
      J$target_level[i] = "2"
    }else if(J$word[i] == "에게서"){
      J$target_word[i] = "에게서"
      J$target_level[i] = "2"
    }else if(J$word[i] == "한테서"){
      J$target_word[i] = "한테서"
      J$target_level[i] = "2"
    }else if(J$word[i] == "이나" | J$word[i] == "나"){
      J$target_word[i] = "이나"
      J$target_level[i] = "2"
    }else if(J$word[i] == "같이"){
      J$target_word[i] = "같이"
      J$target_level[i] = "3"
    }else if(J$word[i] == "이고" | J$word[i] == "고"){
      J$target_word[i] = "이고"
      J$target_level[i] = "3"
    }else if(J$word[i] == "대로"){
      J$target_word[i] = "대로"
      J$target_level[i] = "3"
    }else if(J$word[i] == "으로부터"){
      J$target_word[i] = "으로부터"
      J$target_level[i] = "3"
    }else if(J$word[i] == "만큼" | J$word[i] == "만치"){
      J$target_word[i] = "만큼"
      J$target_level[i] = "3"
    }else if(J$word[i] == "보고"){
      J$target_word[i] = "보고"
      J$target_level[i] = "3"
    }else if(J$word[i] == "뿐"){
      J$target_word[i] = "뿐"
      J$target_level[i] = "3"
    }else if(J$word[i] == "아"){
      J$target_word[i] = "아1"
      J$target_level[i] = "3"
    }else if(J$pos == "JKI" & J$word[i] == "야"){
      J$target_word[i] = "아1"
      J$target_level[i] = "3"
    }else if(J$word[i] == "요"){
      J$target_word[i] = "요1"
      J$target_level[i] = "3"
    }else if(J$word[i] == "이라고" | J$word[i] == "라고" | J$word[i] == "라" | J$word == "이라"){
      J$target_word[i] = "이라고1"
      J$target_level[i] = "3"
    }else if(J$word[i] == "커녕" | J$word[i] == "ㄴ커녕" | J$word[i] == "는커녕" | J$word == "은커녕"){
      J$target_word[i] = "커녕"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이나마" | J$word[i] == "나마"){
      J$target_word[i] = "이나마"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이며" | J$word[i] == "며" | J$word[i] == "하며"){
      J$target_word[i] = "이며"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이든" | J$word[i] == "든" | J$word[i] == "이든지" | J$word == "든지" | J$word == "이든가" | J$word == "든가"){
      J$target_word[i] = "이든"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이란" | J$word[i] == "란"){
      J$target_word[i] = "이란"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이면" | J$word[i] == "면"){
      J$target_word[i] = "이면"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이야" | J$word[i] == "야"){
      J$target_word[i] = "이야"
      J$target_level[i] = "4"
    }else if(J$word[i] == "치고"){
      J$target_word[i] = "치고"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이라도" | J$word[i] == "라도"){
      J$target_word[i] = "이라도"
      J$target_level[i] = "4"
    }else if(J$word[i] == "으로서" | J$word[i] == "로서"){
      J$target_word[i] = "으로서"
      J$target_level[i] = "4"
    }else if(J$word[i] == "으로써" | J$word[i] == "로써"){
      J$target_word[i] = "으로써"
      J$target_level[i] = "4"
    }else if(J$word[i] == "마저"){
      J$target_word[i] = "마저"
      J$target_level[i] = "4"
    }else if(J$word[i] == "이라든가" | J$word[i] == "라든가" | J$word[i] == "이라든지" | J$word == "라든지"){
      J$target_word[i] = "이라든가"
      J$target_level[i] = "5"
    }else if(J$word[i] == "따라"){
      J$target_word[i] = "따라"
      J$target_level[i] = "5"
    }else if(J$word[i] == "조차"){
      J$target_word[i] = "조차"
      J$target_level[i] = "5"
    }else if(J$word[i] == "마는"){
      J$target_word[i] = "마는"
      J$target_level[i] = "6"
    }else if(J$word[i] == "깨나"){
      J$target_word[i] = "깨나"
      J$target_level[i] = "6"
    }else if(J$word[i] == "을랑"){
      J$target_word[i] = "을랑"
      J$target_level[i] = "6"
    }else if(J$word[i] == "이라면" | J$word[i] == "라면"){
      J$target_word[i] = "이라면"
      J$target_level[i] = "6"
    }else{
      J$target_word[i] = "NA"
      J$target_level[i] = "NA"
    }
  }

  for (i in seq_along(EP$word)){
    if (EP$word[i] == "겠"){
      EP$target_word[i] = "-겠-"
      EP$target_level[i] = "1"
    }else if(EP$word[i] == "었" | EP$word[i] == "았" | EP$word[i] == "였"){
      EP$target_word[i] = "-었-"
      EP$target_level[i] = "1"
    }else if(EP$word[i] == "으시" | EP$word[i] == "시"){
      EP$target_word[i] = "-으시-"
      EP$target_level[i] = "1"
    }else{
      EP$target_word[i] = "NA"
      EP$target_level[i] = "NA"
    }
  }

  for (i in seq_along(EF$word)){
    if (EF$word[i] == "습니까" | EF$word[i] == "ㅂ니까"){
      EF$target_word[i] = "-습니까"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "습니다" | EF$word[i] == "ㅂ니다" | EF$word[i] == "니다"){
      EF$target_word[i] = "-습니다"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "읍시다" | EF$word[i] == "ㅂ시다"){
      EF$target_word[i] = "-읍시다"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "으세요" | EF$word[i] == "세요" | EF$word[i] == "으셔요" | EF$word[i] == "셔요" | EF$word[i] == "으시어요" | EF$word[i] == "시어요"){
      EF$target_word[i] = "-으세요"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "으십시오" | EF$word[i] == "십시오" | EF$word[i] == "ㅂ시오"){
      EF$target_word[i] = "-으십시오"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "고" | EF$word[i] == "고요"){
      EF$target_word[i] = "-고4"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "을까" | EF$word[i] == "ㄹ까" | EF$word[i] == "을까요" | EF$word[i] == "ㄹ까요"){
      EF$target_word[i] = "-을까"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "어" | EF$word[i] == "아" | EF$word[i] == "여" | EF$word[i] == "야" | EF$word[i] == "어요" | EF$word[i] == "아요" | EF$word[i] == "여요" | EF$word[i] == "에요"){
      EF$target_word[i] = "-어2"
      EF$target_level[i] = "1"
    }else if(EF$word[i] == "는군" | EF$word[i] == "군" | EF$word[i] == "는군요" | EF$word[i] == "군요"){
      EF$target_word[i] = "-는군"
      EF$target_level[i] = "2"
    }else if(EF$word[i] == "을게" | EF$word[i] == "ㄹ게" | EF$word[i] == "을게요" | EF$word[i] == "ㄹ게요"){
      EF$target_word[i] = "-을게"
      EF$target_level[i] = "2"
    }else if(EF$word[i] == "지" | EF$word[i] == "지요" | EF$word[i] == "죠"){
      EF$target_word[i] = "-지"
      EF$target_level[i] = "2"
    }else if(EF$word[i] == "는데" | EF$word[i] == "ㄴ데" | EF$word[i] == "은데" | EF$word[i] == "는데요" | EF$word[i] == "ㄴ데요" | EF$word[i] == "은데요"){
      EF$target_word[i] = "-는데2"
      EF$target_level[i] = "2"
    }else if(EF$word[i] == "네" | EF$word[i] == "네요"){
      EF$target_word[i] = "-네"
      EF$target_level[i] = "2"
    }else if(EF$word[i] == "을래" | EF$word[i] == "을래요" | EF$word[i] == "ㄹ래요"){
      EF$target_word[i] = "-을래"
      EF$target_level[i] = "2"
    }else if(EF$word[i] == "거든" | EF$word[i] == "거든요"){
      EF$target_word[i] = "-거든2"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "는구나" | EF$word[i] == "구나"){
      EF$target_word[i] = "-는구나"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "는다" | EF$word[i] == "ㄴ다" | EF$word[i] == "다"){
      EF$target_word[i] = "-는다"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "던데" | EF$word[i] == "던데요"){
      EF$target_word[i] = "-던데"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "잖아" | EF$word[i] == "잖아요"){
      EF$target_word[i] = "-잖아"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "자"){
      EF$target_word[i] = "-자3"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "니" | EF$word[i] == "으니"){
      EF$target_word[i] = "-니2"
      EF$target_level[i] = "3"
    }else if(EF$word[i] == "는다니" | EF$word[i] == "ㄴ다니" | EF$word[i] == "다니" | EF$word[i] == "라니"){
      EF$target_word[i] = "-는다니2"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "더군" | EF$word[i] == "더군요"){
      EF$target_word[i] = "-더군"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "더라"){
      EF$target_word[i] = "-더라"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "어라" | EF$word[i] == "아라" | EF$word[i] == "여라"){
      EF$target_word[i] = "-어라1"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "게" | EF$word[i] == "게요"){
      EF$target_word[i] = "-게5"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "는다면서" | EF$word[i] == "ㄴ다면서" | EF$word[i] == "다면서" | EF$word[i] == "라면서" | EF$word[i] == "는다면서요" | EF$word[i] == "다면서요" | EF$word[i] == "라면서요"){
      EF$target_word[i] = "-는다면서1"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "나" | EF$word[i] == "나요"){
      EF$target_word[i] = "-나3"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "을걸" | EF$word[i] == "ㄹ걸" | EF$word[i] == "을걸요" | EF$word[i] == "ㄹ걸요"){
      EF$target_word[i] = "-을걸"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "어야지" | EF$word[i] == "아야지" | EF$word[i] == "여야지" | EF$word[i] == "어야지요" | EF$word[i] == "아야지요" | EF$word[i] == "여야지요"){
      EF$target_word[i] = "-어야지2"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "다니" | EF$word[i] == "다니요" | EF$word[i] == "라니" | EF$word[i] == "라니요"){
      EF$target_word[i] = "-다니1"
      EF$target_level[i] = "4"
    }else if(EF$word[i] == "으려고" | EF$word[i] == "려고" | EF$word[i] == "려고요" | EF$word[i] == "으려고요"){
      EF$target_word[i] = "-으려고2"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "거나"){
      EF$target_word[i] = "-거나"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "고말고" | EF$word[i] == "고말고요"){
      EF$target_word[i] = "-고말고"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "는가" | EF$word[i] == "ㄴ가" | EF$word[i] == "은가"){
      EF$target_word[i] = "-는가1"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "는걸" | EF$word[i] == "ㄴ걸" | EF$word[i] == "은걸" | EF$word[i] == "ㄴ걸요" | EF$word[i] == "는걸요" | EF$word[i] == "은걸요"){
      EF$target_word[i] = "-는걸"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "데" | EF$word[i] == "데요"){
      EF$target_word[i] = "-데"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "더라고" | EF$word[i] == "더라고요"){
      EF$target_word[i] = "-더라고"
      EF$target_level[i] = "5"
    }else if(EF$word[i] == "는구려" | EF$word[i] == "구려"){
      EF$target_word[i] = "-는구려"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "는구먼" | EF$word[i] == "구먼" | EF$word[i] == "구먼요" | EF$word[i] == "는구먼요"){
      EF$target_word[i] = "-는구먼"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "그려"){
      EF$target_word[i] = "-그려"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "으리라" | EF$word[i] == "리라"){
      EF$target_word[i] = "-으리라"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "으오" | EF$word[i] == "오"){
      EF$target_word[i] = "-으오"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "거들랑" | EF$word[i] == "걸랑"){
      EF$target_word[i] = "-거들랑2"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "는구만" | EF$word[i] == "구만"){
      EF$target_word[i] = "-는구만"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "던"){
      EF$target_word[i] = "-던2"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "던가"){
      EF$target_word[i] = "-던가1"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "라"){
      EF$target_word[i] = "-라2"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "으리오" | EF$word[i] == "리오"){
      EF$target_word[i] = "-으리오"
      EF$target_level[i] = "6"
    }else if(EF$word[i] == "소"){
      EF$target_word[i] = "-소"
      EF$target_level[i] = "6"
    }else{
      EF$target_word[i] = "NA"
      EF$target_level[i] = "NA"
    }
  }

  for (i in seq_along(EC$word)){
    if (EC$word[i] == "고"){
      EC$target_word[i] = "-고3"
      EC$target_level[i] = "1"
    }else if(EC$word[i] == "으니까" | EC$word[i] == "니까"){
      EC$target_word[i] = "-으니까"
      EC$target_level[i] = "1"
    }else if(EC$word[i] == "으러" | EC$word[i] == "러"){
      EC$target_word[i] = "-으러"
      EC$target_level[i] = "1"
    }else if(EC$word[i] == "어서" | EC$word[i] == "아서" | EC$word[i] == "여서" | EC$word[i] == "어" | EC$word[i] == "아" | EC$word[i] == "여" | EC$word[i] == "라서" | EC$word[i] == "라"){
      EC$target_word[i] = "-어서"
      EC$target_level[i] = "1"
    }else if(EC$word[i] == "지만"){
      EC$target_word[i] = "-지만"
      EC$target_level[i] = "1"
    }else if(EC$word[i] == "으려고" | EC$word[i] == "려고" | EC$word[i] == "으려" | EC$word[i] == "려"){
      EC$target_word[i] = "-으려고1"
      EC$target_level[i] = "1"
    }else if(EC$word[i] == "거나"){
      EC$target_word[i] = "-거나"
      EC$target_level[i] = "2"
    }else if(EC$word[i] == "는데" | EC$word[i] == "은데" | EC$word[i] == "ㄴ데"){
      EC$target_word[i] = "-는데1"
      EC$target_level[i] = "2"
    }else if(EC$word[i] == "으면" | EC$word[i] == "면"){
      EC$target_word[i] = "-으면"
      EC$target_level[i] = "2"
    }else if(EC$word[i] == "으면서" | EC$word[i] == "면서"){
      EC$target_word[i] = "-으면서"
      EC$target_level[i] = "2"
    }else if(EC$word[i] == "게"){
      EC$target_word[i] = "-게2"
      EC$target_level[i] = "2"
    }else if(EC$word[i] == "거든" | EC$word[i] == "거들랑"){
      EC$target_word[i] = "-거든1"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "는다거나" | EC$word[i] == "ㄴ다거나" | EC$word[i] == "다거나" | EC$word[i] == "라거나"){
      EC$target_word[i] = "-는다거나1"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "는다고" | EC$word[i] == "다고" | EC$word[i] == "라고" | EC$word[i] == "으라고" | EC$word[i] == "자고"){
      EC$target_word[i] = "-는다고1"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "으나" | EC$word[i] == "나"){
      EC$target_word[i] = "-으나"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "느라고" | EC$word[i] == "느라"){
      EC$target_word[i] = "-느라고"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "도록"){
      EC$target_word[i] = "-도록"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "어다가" | EC$word[i] == "아다가" | EC$word[i] == "여다가" | EC$word[i] == "어다" | EC$word[i] == "아다" | EC$word[i] == "여다"){
      EC$target_word[i] = "-어다가"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "어도" | EC$word[i] == "아도" | EC$word[i] == "여도" | EC$word[i] == "라도" | EC$word[i] == "이라도"){
      EC$target_word[i] = "-어도"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "어야" | EC$word[i] == "아야" | EC$word[i] == "여야" | EC$word[i] == "어야만" | EC$word[i] == "아야만" | EC$word[i] == "여야만"){
      EC$target_word[i] = "-어야"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "어야지" | EC$word[i] == "아야지" | EC$word[i] == "여야지"){
      EC$target_word[i] = "-어야지1"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "었더니" | EC$word[i] == "았더니" | EC$word[i] == "였더니"){
      EC$target_word[i] = "-었더니"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "자마자" | EC$word[i] == "자"){
      EC$target_word[i] = "-자마자"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "으니" | EC$word[i] == "니"){
      EC$target_word[i] = "-으니2"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "으려면" | EC$word[i] == "려면"){
      EC$target_word[i] = "-으려면"
      EC$target_level[i] = "3"
    }else if(EC$word[i] == "거니와"){
      EC$target_word[i] = "-거니와"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "고도"){
      EC$target_word[i] = "-고도"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "고자"){
      EC$target_word[i] = "-고자"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "기에"){
      EC$target_word[i] = "-기에"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "는지" | EC$word[i] == "ㄴ지" | EC$word[i] == "은지" | EC$word[i] == "을지"){
      EC$target_word[i] = "-는지"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "다시피"){
      EC$target_word[i] = "-다시피"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "더라도"){
      EC$target_word[i] = "-더라도"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "든지" | EC$word[i] == "든" | EC$word[i] == "든가"){
      EC$target_word[i] = "-든지2"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "으므로" | EC$word[i] == "므로"){
      EC$target_word[i] = "-으므로"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "을래야" | EC$word[i] == "ㄹ래야"){
      EC$target_word[i] = "-을래야"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "고서" | EC$word[i] == "고서는" | EC$word[i] == "고서야"){
      EC$target_word[i] = "-고서"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "는다면" | EC$word[i] == "ㄴ다면" | EC$word[i] == "다면" | EC$word[i] == "라면"){
      EC$target_word[i] = "-는다면1"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "더니"){
      EC$target_word[i] = "-더니"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "던데"){
      EC$target_word[i] = "-던데1"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "듯이"){
      EC$target_word[i] = "-듯이"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "을수록" | EC$word[i] == "ㄹ수록"){
      EC$target_word[i] = "-을수록"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "으며" | EC$word[i] == "며"){
      EC$target_word[i] = "-으며"
      EC$target_level[i] = "4"
    }else if(EC$word[i] == "을뿐더러" | EC$word[i] == "ㄹ뿐더러"){
      EC$target_word[i] = "-을뿐더러"
      EC$target_level[i] = "5"
    }else if(EC$word[i] == "고는" | EC$word[i] == "곤" | EC$word[i] == "고는 하다" | EC$word[i] == "곤 하다"){
      EC$target_word[i] = "-고는"
      EC$target_level[i] = "5"
    }else if(EC$word[i] == "길래"){
      EC$target_word[i] = "-길래"
      EC$target_level[i] = "5"
    }else if(EC$word[i] == "다가는" | EC$word[i] == "다간" | EC$word[i] == "단"){
      EC$target_word[i] = "-다가는"
      EC$target_level[i] = "5"
    }else if(EC$word[i] == "을지라도" | EC$word[i] == "ㄹ지라도"){
      EC$target_word[i] = "-을지라도"
      EC$target_level[i] = "5"
    }else if(EC$word[i] == "느니" | EC$word[i] == "느니보다" | EC$word[i] == "느니보다는"){
      EC$target_word[i] = "-느니1"
      EC$target_level[i] = "5"
    }else if(EC$word[i] == "거들랑" | EC$word[i] == "걸랑"){
      EC$target_word[i] = "-거들랑"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "노라면"){
      EC$target_word[i] = "-노라면"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "건대"){
      EC$target_word[i] = "-건대"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "건만" | EC$word[i] == "건마는"){
      EC$target_word[i] = "-건만"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "기로서니"){
      EC$target_word[i] = "-기로서니"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "느니만큼" | EC$word[i] == "니만큼" | EC$word[i] == "으니만큼" | EC$word[i] == "느니만큼" | EC$word[i] == "니만치" | EC$word[i] == "으니만치"){
      EC$target_word[i] = "-느니만큼"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "되" | EC$word[i] == "으되" | EC$word[i] == "로되"){
      EC$target_word[i] = "-되"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "디"){
      EC$target_word[i] = "-디1"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "이라야" | EC$word[i] == "라야" | EC$word[i] == "이라야만" | EC$word[i] == "라야만"){
      EC$target_word[i] = "-이라야"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "으련마는" | EC$word[i] == "련마는" | EC$word[i] == "으련만" | EC$word[i] == "련만"){
      EC$target_word[i] = "-으련마는"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "자니" | EC$word[i] == "자니까"){
      EC$target_word[i] = "-자니3"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "자면"){
      EC$target_word[i] = "-자면1"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "은들" | EC$word[i] == "ㄴ들" | EC$word[i] == "인들"){
      EC$target_word[i] = "-은들"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "을망정" | EC$word[i] == "ㄹ망정" | EC$word[i] == "ㄹ지언정" | EC$word[i] == "을지언정"){
      EC$target_word[i] = "-을망정"
      EC$target_level[i] = "6"
    }else if(EC$word[i] == "을라치면" | EC$word[i] == "ㄹ라치면"){
      EC$target_word[i] = "-을라치면"
      EC$target_level[i] = "6"
    }else{
      EC$target_word[i] = "NA"
      EC$target_level[i] = "NA"
    }
  }

  for (i in seq_along(ET$word)){
    if (ET$word[i] == "기"){
      ET$target_word[i] = "-기"
      ET$target_level[i] = "2"
    }else if(ET$word[i] == "는"){
      ET$target_word[i] = "-는2"
      ET$target_level[i] = "2"
    }else if(ET$word[i] == "은" | ET$word[i] == "ㄴ"){
      ET$target_word[i] = "-은2"
      ET$target_level[i] = "2"
    }else if(ET$word[i] == "을" | ET$word[i] == "ㄹ"){
      ET$target_word[i] = "-을2"
      ET$target_level[i] = "2"
    }else if(ET$word[i] == "음" | ET$word[i] == "ㅁ"){
      ET$target_word[i] = "-음"
      ET$target_level[i] = "2"
    }else if(ET$word[i] == "던"){
      ET$target_word[i] = "-던-"
      ET$target_level[i] = "3"
    }else{
      ET$target_word[i] = "NA"
      ET$target_level[i] = "NA"
    }
  }
  grammar = rbind(EC, EF, EP, ET, J)
  return(grammar)
}
