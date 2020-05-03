librarys <- function(){
    library(httr)
    library(dplyr)
    library(stringr)
}
librarys()



t1 <- GET('https://kr.api.riotgames.com/lol/summoner/v4/summoners/by-name/%EC%97%A0%EB%84%B7%EB%A7%88%EC%9D%B4%EB%84%88%EA%B0%A4%EB%9F%AC%EB%A6%AC?api_key=RGAPI-33568088-b4b7-496b-8114-f263bb9c00a7')

t1 %>% content()


# basic_setup

api_key <- readLines('api_key.txt') # api 키값
matchnum <- 30                      # 가져올 경기 수

# 내 경기 정보 가져오기

matchid <- str_glue('https://asia.api.riotgames.com/tft/match/v1/matches/by-puuid/_TinLXdqnvAgl4zEF4QA2-3yp8_cn3qq_vC-w3fdYMKCpXfOfmsQE4XjT5bFIVoGDdO3jTJZwMfYXw/ids?count={matchnum}&api_key={api_key}') %>%
    as.character() %>%
    GET() %>%
    content() %>%
    unlist()

i = 1
gamedata <- c()
for(matchid in matchid){

    gamedata[i] <- str_glue('https://asia.api.riotgames.com/tft/match/v1/matches/{matchid}?api_key={api_key}')

    i = i + 1
}




t1 <- gamedata %>% .[1] %>% GET() %>% content()


t1 %>% glimpse()

t1[["info"]][["participants"]][[1]][["traits"]] %>%
    lapply(., function(x){as.data.frame(x)}) %>%
    lapply(., function(x){length(x)})
t1[["info"]][["participants"]][[1]][["traits"]] %>% do.call(rbind, .) %>% as.data.frame()
t1[["info"]][["participants"]][[2]][["traits"]] %>% do.call(rbind, .)
t1[["info"]][["participants"]][[3]][["traits"]] %>% do.call(rbind, .) %>% as.data.frame() %>% View()
t1[["info"]][["participants"]][[2]][["traits"]] %>% do.call(rbind, .)
t1[["info"]][["participants"]][[2]][["traits"]] %>% do.call(rbind, .)

length(t1[["info"]][["participants"]])
str(t1[["info"]][["participants"]])


t2 <- c()
t3 <- c()
i = 0
for(i in 1:length(t1[["info"]][["participants"]])){

    t2[[i]] <- t1[["info"]][["participants"]][[i]][["traits"]] %>%
        do.call(rbind, .) %>%
        as.data.frame()

    t3[[i]] <- t1[["info"]][["participants"]][[i]][["units"]] %>%
        do.call(rbind, .) %>%
        as.data.frame()


}


# 모호한것들은 넘어가자
# 아는것을 최대한 활용하는 쪽으로
# 경기에서 사용한 챔피언들
champions <- c()
temp_champ <-c()
i = 0
for(i in 1:8){
    temp_champ <- t1[["info"]][["participants"]][[i]][['units']] %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        select(1) %>%
        unlist() %>%
        paste(., collapse = ',')
    champions <- c(champions, temp_champ)
}
champions

# 챔피언들 특성
traits <- c()
i = 0

for(i in 1:8){
    traits[i] <- t1[["info"]][["participants"]][[i]][['traits']] %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        select(1) %>%
        unlist() %>%
        paste(., collapse = ',')
}
traits

t1[["info"]][["participants"]][[i]][['traits']] %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    select(name) %>%
    unlist() %>%
    paste(., collapse = ',')

# 등수
i <- 0
play_time <- c()
for(i in 1:8){
    play_time[i] <- t1[["info"]][["participants"]][[i]][["time_eliminated"]]
}

play_time # 플레이타임의 재정렬을 통해 등수를 도출할 수 있음 (값이 가장 큰 유저가 1등)
sort(play_time, decreasing = T)

# 남은 골드
i <- 0
gold_left <- c()
for(i in 1:8){
    gold_left[i] <- t1[["info"]][["participants"]][[i]][["gold_left"]]
}
gold_left


# 마지막 라운드
i = 0
last_round <- c()
for(i in 1:8){
    last_round[i] <- t1[["info"]][["participants"]][[1]][["last_round"]]
}


## 데이터 추출 종합
i <- 0
ranking <- c()
play_time <- c()
gold_left <- c()
last_round <- c()
player_level <- c()
total_damage_to_players <- c()
for(i in 1:8){
    ranking[i] <- t1[["info"]][["participants"]][[i]][["placement"]]
    play_time[i] <- t1[["info"]][["participants"]][[i]][["time_eliminated"]]
    gold_left[i] <- t1[["info"]][["participants"]][[i]][["gold_left"]]
    last_round[i] <- t1[["info"]][["participants"]][[i]][["last_round"]]
    player_level[i] <- t1[["info"]][["participants"]][[i]][["level"]]
    total_damage_to_players[i] <- t1[["info"]][["participants"]][[i]][["total_damage_to_players"]]
}


# 추출한 데이터 묶기(data.frame)
matchdata <- data.frame(ranking = ranking,
                        play_time = play_time,
                        champions = champions,
                        gold_left = gold_left,
                        last_round = last_round,
                        player_level = player_level,
                        total_damage_to_players = total_damage_to_players)

matchdata %>% arrange(ranking)


matchdata %>% glimpse()













































