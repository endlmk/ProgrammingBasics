(* 各人の名前身長、体重、誕生日と血液型を格納する *)
type person_t = {
    name : string; (* 名前 *)
    hight : float; (* 身長(m) *)
    weight : float; (* 体重(kg) *)
    birth_month : int; (* 誕生月 *)
    birth_day : int; (* 誕生日 *) 
    blood : string; (* 血液型 *)
}

(* 目的: person_tを受け取り、「〇〇さんの血液型は△型です」という文字列を返す *)
(* ketsueki_hyoji: person_t -> stirng *)
let ketsueki_hyoji p = match p with 
    { name = n; hight = _; weight = _; birth_month = _; birth_day = _; blood = b; } ->
        n ^ "さんの血液型は" ^ b ^ "型です"

let%test _ = ketsueki_hyoji { name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; } = "bearさんの血液型はA型です"
let%test _ = ketsueki_hyoji { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; } = "nekoさんの血液型はO型です"
let%test _ = ketsueki_hyoji { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; } = "orderさんの血液型はAB型です"

(* 駅名の情報を格納する *)
type ekimei_t = {
    kanji : string; (* 漢字の駅名 *)
    kana : string; (* ひらがなの駅名 *)
    romaji : string; (* ローマ字の駅名 *)
    shozoku : string; (* 駅が所属する路線名 *)
}

(* 目的:駅名の情報から「路線名、駅名（かな）」の文字列を返す *)
(* hyoji: ekimei_t -> string *)
let hyoji e = match e with 
    {kanji = k; kana = kn; romaji = _; shozoku = s;} ->
        s ^ ", " ^ k ^ " (" ^ kn ^ ")"

let%test _ = hyoji { kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"; } = "丸ノ内線, 茗荷谷 (みょうがだに)"
let%test _ = hyoji { kanji="亀岡"; kana="かめおか"; romaji="kameoka"; shozoku="嵯峨野線"; } = "嵯峨野線, 亀岡 (かめおか)"
let%test _ = hyoji { kanji="二条"; kana="にじょう"; romaji="nijo"; shozoku="地下鉄東西線"; } = "地下鉄東西線, 二条 (にじょう)"

(* 駅と駅の接続情報を格納する *)
type ekikan_t = {
    kiten : string; (* 起点の駅名 *)
    shuten : string; (* 終点の駅名 *)
    kyori : float; (* 2駅間の距離(km) *)
    jikan : int; (* 所要時間(分) *)
}