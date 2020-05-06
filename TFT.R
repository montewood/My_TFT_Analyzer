librarys <- function(){
    library(httr)
    library(dplyr)
    library(stringr)
}
librarys()

# basic_setup

api_key <- readLines('api_key.txt') # api 키값
matchnum <- 40                      # 가져올 경기 수 , 1초에 20개넘는 요청 보내면 blacklist 처리 됨.

# 내 경기 정보 가져오기

matchids <- str_glue('https://asia.api.riotgames.com/tft/match/v1/matches/by-puuid/_TinLXdqnvAgl4zEF4QA2-3yp8_cn3qq_vC-w3fdYMKCpXfOfmsQE4XjT5bFIVoGDdO3jTJZwMfYXw/ids?count={matchnum}&api_key={api_key}') %>%
    as.character() %>%
    GET() %>%
    content() %>%
    unlist()

i = 1
matchid <- NULL
gamedata <- c()
matchdata <- c()
ranking <- c()
play_time <- c()
gold_left <- c()
last_round <- c()
player_level <- c()
total_damage_to_players <- c()
champions <- c()
traits <- c()
for(matchid in matchids){
    print(matchid)

    gamedata <- str_glue('https://asia.api.riotgames.com/tft/match/v1/matches/{matchid}?api_key={api_key}') %>%
        as.character() %>%
        GET() %>%
        content()

    ranking <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["placement"]]})
    play_time <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["time_eliminated"]]})
    gold_left <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["gold_left"]]})
    last_round <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["last_round"]]})
    player_level <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["level"]]})
    total_damage_to_players <- sapply(seq(1,8), function(x){
        gamedata[["info"]][["participants"]][[x]][["total_damage_to_players"]]
        })
    champions <- sapply(seq(1,8), function(x){
        gamedata[["info"]][["participants"]][[x]][['units']] %>% champ_traits_function()
        })
    traits <- sapply(seq(1,8), function(x){
        gamedata[["info"]][["participants"]][[x]][['traits']] %>% champ_traits_function()
        })


    temp_matchdata <- data.frame(ranking = ranking,
                                 play_time = play_time,
                                 gold_left = gold_left,
                                 last_round = last_round,
                                 player_level = player_level,
                                 total_damage_to_players = total_damage_to_players,
                                 champions = champions,
                                 traits = traits)

    matchdata <- rbind(matchdata, temp_matchdata)

}


# 순위와 나머지 변수간의 선형성 파악

library(GGally)
## 상관성
cordata <- matchdata %>% select(-c(champions, traits)) %>% cor() %>% as.data.frame()
cordata %>% ggpairs()
cor.test(cordata$ranking, cordata$play_time)

## 회귀분석
glimpse(matchdata)
lmdata <- matchdata %>% select(-champions)

Reg <- lm(ranking ~ play_time + gold_left + last_round + player_level + total_damage_to_players, data = lmdata)
Reg
summary(Reg)

## 다중공선성 확인
library(car)
vif(Reg) # gold_left, player_level 만이 다중공선성 5 미만

## 변수 조정하고 다시 회귀분석
Reg2 <- lm(ranking ~ gold_left + player_level + total_damage_to_players, data = lmdata)
summary(Reg2)

par(mfrow = c(2,2))
plot(Reg2)
par(mfrow = c(1,1))

## 잔차의 정규성 검증
shapiro.test(Reg2$residuals) # p-value = 0.39 -> 잔차의 정규성에 위배되지 않는다.


## step() 함수 사용한 다중 회귀
lmdata2 <- matchdata %>% select(-c(champions, traits))
Reg <- lm(ranking ~., data = lmdata2)
summary(Reg)

stepReg <- step(Reg)
stepReg
summary(stepReg)

plot(stepReg)

par(mfrow = c(1,1))
boxplot(lmdata2)


stepReg %>% summary()

stepReg$residuals %>% plot()


# 챌린저 게임 데이터 수집하기

## basic_setup
api_key <- readLines('api_key.txt') # api 키값
matchnum <- 40                      # 가져올 경기 수 , 1초에 20개넘는 요청 보내면 blacklist 처리 됨.

## 챌린저 유저 리스트
challengerList <- str_glue('https://kr.api.riotgames.com/tft/league/v1/challenger?api_key={api_key}') %>%
    as.character() %>%
    GET() %>%
    content() %>%
    as.list() %>%
    .$entries %>%
    lapply(., function(x){as.data.frame(x)}) %>% do.call(rbind, .) %>%
    as.data.frame()

summonerIDs <- challengerList %>% select(summonerId)

## 챌린저 유저 ID 수집
i = 0
summonerID <- c()
getuserid <- list()
for(i in seq(1, nrow(summonerIDs))){

    print(i)

    summonerID <- summonerIDs$summonerId[i] %>% as.character()
    getuserid[[i]] <- str_glue('https://kr.api.riotgames.com/tft/summoner/v1/summoners/{summonerID}?api_key={api_key}') %>%
        as.character() %>%
        GET() %>%
        content() %>%
        as.data.frame()

    # 1초에 20 request 넘어가면 안됨
    if(i %% 20 == 0){
        Sys.sleep(1)
    }

    # 2분에 100 request 넘어가면 안됨
    if(i %% 100 == 0){
        Sys.sleep(121)
    }
}

getuserid <- getuserid %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    rename(summonerId = id)

challenger <- left_join(x = challengerList, y = getuserid, "summonerId") # 챌린저 데이터


## 챌린저 유저 매치 기록 가져오기
puuid <- challenger %>% select(puuid)

i = 1
x = 1
a = 1
puuid_num <- c()
puuid_rawmatchs <- c()
puuid_matchs <- c()
gamerawdata <- c()
gamedata <- c()
temp_matchdata1 <- c()
temp_matchdata2 <- c()
matchdata <- c()
ranking <- c()
play_time <- c()
gold_left <- c()
last_round <- c()
player_level <- c()
total_damage_to_players <- c()
champions <- c()
traits <- c()

for(i in 1:nrow(puuid)){
    try({
        print(a)

        if(a %% 20 == 0){        # 1초에 20 request 넘어가면 안됨
            Sys.sleep(1.5)
            if(a %% 100 == 0){
                Sys.sleep(125)   # 2분에 100 request 넘어가면 안됨
            }
        }

        a = a + 1
        puuid_num <- puuid[i,] %>% as.character()
        puuid_rawmatchs <-
            str_glue('https://asia.api.riotgames.com/tft/match/v1/matches/by-puuid/{puuid_num}/ids?count={matchnum}&api_key={api_key}') %>%
            as.character() %>%
            GET()

        if(puuid_rawmatchs$status_code != 200){
            print(paste('error point : ', a))
            break
        }else{
            puuid_matchs <- puuid_rawmatchs %>% content() %>% unlist()

            for(x in 1:length(puuid_matchs)){
                print(a)

                if(a %% 20 == 0){        # 1초에 20 request 넘어가면 안됨
                    Sys.sleep(1.5)
                    if(a %% 100 == 0){
                        Sys.sleep(125)   # 2분에 100 request 넘어가면 안됨
                    }
                }

                a = a + 1
                puuid_match <- puuid_matchs[x]

                gamerawdata <- str_glue('https://asia.api.riotgames.com/tft/match/v1/matches/{puuid_match}?api_key={api_key}') %>%
                    as.character() %>%
                    GET()

                if(gamerawdata$status_code != 200){
                    print(paste('error point : ', a))
                    break
                }else{
                    gamedata <- gamerawdata %>% content()

                    ranking <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["placement"]]})
                    play_time <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["time_eliminated"]]})
                    gold_left <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["gold_left"]]})
                    last_round <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["last_round"]]})
                    player_level <- sapply(seq(1,8), function(x){gamedata[["info"]][["participants"]][[x]][["level"]]})
                    total_damage_to_players <- sapply(seq(1,8), function(x){
                        gamedata[["info"]][["participants"]][[x]][["total_damage_to_players"]]
                    })
                    champions <- sapply(seq(1,8), function(x){
                        gamedata[["info"]][["participants"]][[x]][['units']] %>% champ_traits_function()
                    })
                    traits <- sapply(seq(1,8), function(x){
                        gamedata[["info"]][["participants"]][[x]][['traits']] %>% champ_traits_function()
                    })


                    temp_matchdata1 <- data.frame(ranking = ranking,
                                                  play_time = play_time,
                                                  gold_left = gold_left,
                                                  last_round = last_round,
                                                  player_level = player_level,
                                                  total_damage_to_players = total_damage_to_players,
                                                  champions = champions,
                                                  traits = traits)

                    temp_matchdata2 <- rbind(temp_matchdata2, temp_matchdata1)
                }
            }

            try(matchdata <- rbind(matchdata, temp_matchdata2))
            temp_matchdata2 <- c()
        }

        gc(reset = T)
    })
}
saveRDS(challenger, 'challenger_list.rds')
saveRDS(matchdata, 'challenger_data.rds')












