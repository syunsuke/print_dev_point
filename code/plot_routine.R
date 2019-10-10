library(tidyverse)
library(leaflet)
########################################################
# 位置情報をエクセルの「N」列に記入する
#  エクセルの14番目の列（エクセルでは「N」の列）に座標を書く
#  座標は、googlemapで指定の場所を右クリックして出るメニューから
#  この場所についてを選択すると、
#  緯度経度情報が画面下に表示される。
#  その部分をクリックすると左側の検索窓やその下に、
#  緯度経度の座標が表示されるので、それをコピーする。
#  緯度経度情報は以下の様なモノ
# 
# 34.690111, 135.515102
#
#  これをそのまま、「N」列にペーストします
#  緯度経度に分けたりする必要はありません。
#
# CSVファイルとして保存する
#  該当のシートを開いた状態で、名前を付けて保存を選択し、
#  ファイル名の下のファイルの種類ドロップボックスを開いて
#  CSV UTF-8（コンマ区切り）
#  を選択して保存します。
#########################################################


#########################################################
# 開発一覧シートの読み込みルーチン
#########################################################
read_dev_csv <- function(path){
  
  # 必要列のみ取得
  df <- read_csv(path) %>% filter(!is.na(`No.`))
  
  # 加工列を追加
  # 座標文字列のある列は14行目で決め打ち
  ans <- df %>% mutate(lng = dev_lng(.[[14]]),
                       lat = dev_lat(.[[14]]))
  
  return(ans)
}

########################################################
# 汎用便利ルーチン
########################################################


########################################################
# 緯度経度分割サブルーチン
# google形式の緯度経度コンマ区切り文字列
########################################################
dev_lat <- function(pos_str_v){
  ans <- c()
  for(i in seq_along(pos_str_v)){
    tmp <- strsplit(pos_str_v[i], ", ") %>% 
      unlist() %>% 
      .[1] %>% as.numeric()
    
    ans <- c(ans, tmp)
  }
  
  return(ans)
}

dev_lng <- function(pos_str_v){
  ans <- c()
  for(i in seq_along(pos_str_v)){
    tmp <- strsplit(pos_str_v[i], ", ") %>% 
      unlist() %>% 
      .[2] %>% as.numeric()
    
    ans <- c(ans, tmp)
  }
  
  return(ans)
}

#######################################################################
# 印刷用マップオブジェクトの作成
#######################################################################

draw_map <- function(df, bottom_f = c(), left_f = c(), right_f =c(), map = 1){
  
  
  not_top <- c(bottom_f,left_f,right_f)
  
  # topに表示
  if(length(not_top)!=0){
  top_df <- df[-not_top,]
  }else{
    top_df <- df
  }
  
  # bottomに表示
  bottom_df <- df[bottom_f,]
  
  # leftに表示
  left_df <- df[left_f,]
  
  # rightに表示
  right_df <- df[right_f,]
  
  ###################################################
  # メイン
  ###################################################
  
  if(map == 1){
    tmp_map <- leaflet() %>% addTiles()
    
  }else if(map == 2){
    # 国土地理院
    atr <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
    tmp_map <- leaflet() %>%
      addTiles("http://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",attribution = atr)
    
  }else if(map == 3){
    # 国土地理院
    atr <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
    tmp_map <- leaflet() %>%
      addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",attribution = atr)
    
  }else if(map == 4){
    # 国土地理院（航空写真）
    atr <- "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>"
    tmp_map <- leaflet() %>%
      addTiles("https://cyberjapandata.gsi.go.jp/xyz/seamlessphoto/{z}/{x}/{y}.jpg",attribution = atr)
    
  }else{
    tmp_map <- leaflet() %>% addTiles()
  }
  
  # オブジェクト追加
  ans <- tmp_map %>% 
    add_mymarker(top_df, "top") %>% 
    add_mymarker(left_df, "left") %>% 
    add_mymarker(right_df, "right") %>% 
    add_mymarker(bottom_df, "bottom") 
  

  return(ans)
  
}


add_mymarker <- function(map_obj, data, direction_str){
  ans_map <- map_obj
    
  if(nrow(data) != 0){
    ans_map <- ans_map %>% 
      addCircleMarkers(data = data,
                       lng = ~lng,
                       lat = ~lat,
                       label = ~`No.`, 
                       stroke = F,
                       color = "blue",
                       fillOpacity = 0.8,
                       radius = 2,
                       labelOptions = 
                         labelOptions(noHide = T,
                                      textOnly = F,
                                      opacity = 1.0,
                                      direction = direction_str,
                                      style = list("font-size" = "10px",
                                                   "color" = "blue")))
  }                    
  return(ans_map)                   
}


