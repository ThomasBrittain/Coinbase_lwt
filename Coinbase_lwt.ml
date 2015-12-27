(* Coinbase Exchange REST API interface -- Lwt friendly *)

(* To Compile Manually: *)
(* ocamlfind ocamlc -c -o Coinbase_lwt *)
(* -thread -syntax camlp4o *)
(* -package base64,cryptokit,cohttp.lwt,uri,yojson,lwt,lwt.syntax,core *)
(* -linkpkg Coinbase_lwt.ml *)

open Yojson.Basic.Util

module type AccountInfo =
  sig
    val username : string
    val passphrase : string
    val api_key : string
    val secret_key : string
  end

module Make (U : AccountInfo) = struct

  let (>>=) = Lwt.bind

  let coinbase_api = "https://api.exchange.coinbase.com/"
  let quote_increment = 0.01
  let hmac_key = B64.decode U.secret_key

  (* Post request *)
  let post_it ~body path =
    let post_uri = Uri.of_string (coinbase_api ^ path) in
    let timestamp = string_of_int @@ int_of_float @@ Unix.time () in
    let message = timestamp ^ "POST/" ^ path ^ body in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 hmac_key in
      crypto_obj#add_string message;
      crypto_obj#result
    in
    let signature_b64 = B64.encode signature in
    let post_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-SIGN" signature_b64
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-TIMESTAMP" timestamp
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-KEY" U.api_key
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-PASSPHRASE" U.passphrase
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
      |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
    in
    Cohttp_lwt_unix.Client.post ~headers:post_header ~body:(Cohttp_lwt_body.of_string body) post_uri
    >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
    >>= fun s -> Yojson.Basic.from_string s |> Lwt.return

  (* Delete request *)
  let delete_it path =
    let delete_uri = Uri.of_string (coinbase_api ^ path) in
    let timestamp = string_of_int @@ int_of_float @@ Unix.time () in
    let message = timestamp ^ "DELETE/" ^ path in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 hmac_key in
      crypto_obj#add_string message;
      crypto_obj#result
    in
    let signature_b64 = B64.encode signature in
    let delete_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-SIGN" signature_b64
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-TIMESTAMP" timestamp
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-KEY" U.api_key
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-PASSPHRASE" U.passphrase
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
      |> fun h -> Cohttp.Header.add h "content-type" "application/json; charset=utf-8"
    in
    Cohttp_lwt_unix.Client.delete ~headers:delete_header delete_uri
    >>= fun (a, b) -> Cohttp_lwt_body.to_string b

  (* Get request - pvt -> is this a private API call*)
  let get_it ?(pvt = false) path =
    let get_uri = Uri.of_string (coinbase_api ^ path) in
    let timestamp = string_of_int @@ int_of_float @@ Unix.time () in
    let message = timestamp ^ "GET/" ^ path in
    let signature =
      let crypto_obj = Cryptokit.MAC.hmac_sha256 hmac_key in
      crypto_obj#add_string message;
      crypto_obj#result
    in
    let signature_b64 = B64.encode signature in
    let private_get_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-SIGN" signature_b64
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-TIMESTAMP" timestamp
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-KEY" U.api_key
      |> fun h -> Cohttp.Header.add h "CB-ACCESS-PASSPHRASE" U.passphrase
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
    in
    let public_get_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "User-Agent" "None-Of-Your-Business"
    in
    let get_header =
      match pvt with
      | true -> private_get_header
      | false -> public_get_header
    in
    Cohttp_lwt_unix.Client.get ~headers:get_header get_uri
    >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
    >>= fun s -> Yojson.Basic.from_string s |> Lwt.return

  (* Currency type *)
  type currency = USD | BTC | CurrencyError

  (* Symbol type *)
  type symbol = BtcUsd | BtcGbp | BtcEur | SymbolError

  (* Account type *)
  type account = {
    id : string;
    currency : currency;
    balance :float;
    hold : float;
    available : float;
    profile_id : string
  }

  (* Ticker type *)
  type ticker = {
    mid : float;
    bid : float;
    ask : float;
    last : float;
    ticker_time : float
  }

  (* Order kind *)
  type order_kind = Buy | Sell | OrderKindError

  (* Order type *)
  type order = {price : float; size : float; side : order_kind}

  (* Level 2 orderbook entry *)
  type level_2_entry = {price : float; size : float; num_orders : int}

  (* Level 3 orderbook entry *)
  type level_3_entry = {price : float; size : float; order_id : string; sequence : int}

  (* STP type *)
  type self_trade_prevention = DC | CO | CN | CB | StpTypeError

  (* Order Receipt type *)
  type order_receipt = {
    id : string;
    price : float;
    size : float;
    product_id : symbol;
    side : order_kind;
    stp : self_trade_prevention
  }

  (* Open Order type *)
  type open_order = {
    id : string;
    price : float;
    size : float;
    product_id : symbol;
    side : order_kind;
    created_at : string;
    done_at : string option;
    done_reason : string option;
    filled_size : float;
    fill_fees : float;
    status : string;
    settled : bool
  }

  (* New Order Result type *)
  type new_order_result = Success of order_receipt | Failure of string

  (* Trade type *)
  type trade = {price : float; amount : float; timestamp : UnixLabels.tm}

  (* Position type *)
  type position_type = Long | Short

  (* Product type *)
  type product = {
    id : string;
    base_currency : currency;
    quote_currency : currency;
    base_min_size : float;
    base_max_size : int;
    quote_increment : float;
    display_name : string
  }

  (* Order type *)
  type order_type = Limit
                  | Market
                  | Stop
                  | TrailingStop
                  | FillOrKill
                  | OrderTypeError

  (* Last Trade type *)
  type last_trade = {trade_id : int; price : float; size : float; time : Unix.tm}

  (* Websocket Feed Types - done, open  & received *)
  type ws_data = Done | Open | Received | Match | Change | WsDataError

  type ws_feed = {entry_type : ws_data;
                  price : float;
                  side : order_kind;
                  remaining_size :float;
                  sequence : int;
                  order_id : string;
                  reason : string;
                  product_id : symbol;
                  size : float;
                  client_oid : string;
                  trade_id : int;
                  maker_order_id : string;
                  taker_order_id : string;
                  new_size : float;
                  old_size : float;
                  time : string}

  (* TODO: Change Failure to Failure of string so that messages can be printed with a Failure *)
  (* Order Result type *)
  (*type order_result = Success | Failure*)

  let currency_of_string s =
    match s with
    | "USD" -> USD
    | "BTC" -> BTC
    | _ -> CurrencyError

  let string_of_currency c =
    match c with
    | USD -> "USD"
    | BTC -> "BTC"
    | CurrencyError -> "CurrencyError"

  let string_of_symbol (sym : symbol) =
    match sym with
    | BtcUsd -> "BTC-USD"
    | BtcGbp -> "BTC-GBP"
    | BtcEur -> "BTC-EUR"
    | SymbolError -> "SymbolError"

  let symbol_of_string s =
    match s with
    | "BTC-USD" -> BtcUsd
    | "BTC-GBP" -> BtcGbp
    | "BTC-EUR" -> BtcEur
    | _ -> SymbolError

  let string_of_order_kind (ok : order_kind) =
    match ok with
    | Buy -> "buy"
    | Sell -> "sell"
    | OrderKindError -> "order-kind-error"

  let order_kind_of_string s =
    match s with
    | "buy" -> Buy
    | "sell" -> Sell
    | _ -> OrderKindError

  let stp_of_string s =
    match s with
    | "dc" -> DC
    | "co" -> CO
    | "cn" -> CN
    | "cb" -> CB
    | _ -> StpTypeError

  (* TODO: Test this function more thoroughly *)
  let ws_type_of_string json_string =
    let open Yojson.Basic.Util in
    let ws_json = Yojson.Basic.from_string json_string in
    let entry = member "type" ws_json |> to_string in
    let f x json = member x json |> to_string |> float_of_string in
    let ok x json = member x json |> to_string |> order_kind_of_string in
    let i x json = member x json |> to_int in
    let sym x json = member x json |> to_string |> symbol_of_string in
    let s x json = member x json |> to_string in
    match entry with
    | "done" ->
        {entry_type = Done; price = f "price" ws_json; side = ok "side" ws_json;
         remaining_size = f "remaining_size" ws_json; sequence = i "sequence" ws_json;
         order_id = s "order_id" ws_json; reason = s "reason" ws_json;
         product_id = sym "product_id" ws_json; size = 0.0; client_oid = "NA"; trade_id = 0;
         maker_order_id = "NA"; taker_order_id = "NA"; new_size = 0.0; old_size = 0.0;
         time = s "time" ws_json}
    | "open" ->
        {entry_type = Open; price = f "price" ws_json; side = ok "side" ws_json;
         remaining_size = f "remaining_size" ws_json; sequence = i "sequence" ws_json;
         order_id = s "order_id" ws_json; reason = "NA"; product_id = sym "product_id" ws_json;
         size = 0.0; client_oid = "NA"; trade_id = 0; maker_order_id = "NA"; taker_order_id = "NA";
         new_size = 0.0; old_size = 0.0; time = s "time" ws_json}
    | "received" ->
        {entry_type = Received; price = f "price" ws_json; side = ok "side" ws_json;
         remaining_size = 0.0; sequence = i "sequence" ws_json; order_id = s "order_id" ws_json;
         reason = "NA"; product_id = sym "product_id" ws_json; size = f "size" ws_json;
         client_oid = "NA"; trade_id = 0; maker_order_id = "NA";
         taker_order_id = "NA"; new_size = 0.0; old_size = 0.0; time = s "time" ws_json}
    | "match" ->
        {entry_type = Match; price = f "price" ws_json; side = ok "side" ws_json;
         remaining_size = 0.0; sequence = i "sequence" ws_json; order_id = ""; reason = "NA";
         product_id = sym "product_id" ws_json; size = f "size" ws_json; client_oid = "";
         trade_id = i "trade_id" ws_json; maker_order_id = s "maker_order_id" ws_json;
         taker_order_id = s "taker_order_id" ws_json; new_size = 0.0; old_size = 0.0;
         time = s "time" ws_json}
    | "change" ->
        {entry_type = Change; price = f "price" ws_json; side = ok "side" ws_json;
         remaining_size = 0.0; sequence = i "sequence" ws_json; order_id = s "order_id" ws_json;
         reason = "NA"; product_id = sym "product_id" ws_json; size = 0.0; client_oid = "";
         trade_id = 0; maker_order_id = ""; taker_order_id = ""; new_size = f "new_size" ws_json;
         old_size = f "old_size" ws_json; time = s "time" ws_json}
    | _ ->
        {entry_type = WsDataError; price = 0.0; side = OrderKindError; remaining_size = 0.0;
         sequence = 0; order_id = ""; reason = ""; product_id = SymbolError; size = 0.0;
         client_oid = ""; trade_id = 0; maker_order_id = ""; taker_order_id = ""; new_size = 0.0;
         old_size = 0.0; time = ""}

  (* Add a new ws_feed record to a Level 3 orderbook and return the new sorted Level 3 orderbook *)
  let apply_ws_feed (feed : ws_feed) (book : level_3_entry list list) =
    let bid_book = List.hd book in
    let ask_book = List.hd @@ List.rev book in
    let sort_bids unsorted_bids =
      List.fast_sort (fun (x : level_3_entry) (y : level_3_entry) ->
                        if (x.price < y.price) then 1 else
                        (if (x.price = y.price) && (x.sequence > y.sequence) then 1 else -1)
                     )
                     unsorted_bids
    in
    let sort_asks unsorted_asks =
      List.fast_sort (fun (x : level_3_entry) (y : level_3_entry) ->
                        if (x.price > y.price) then 1 else
                        (if (x.price = y.price) && (x.sequence > y.sequence) then 1 else -1)
                     )
                unsorted_asks
    in
    match feed.entry_type with
    | Done -> 
        (
          match feed.side with
          | Buy ->
              [(List.filter (fun (x : level_3_entry) -> x.order_id <> feed.order_id) bid_book);
               ask_book]
          | Sell ->
              [bid_book;
               (List.filter (fun (x : level_3_entry) -> x.order_id <> feed.order_id) ask_book)]
          | OrderKindError -> [bid_book; ask_book] (* ??? Should the orderbook start over?! *)
        )
    | Open ->
        (
          match feed.side with
          | Buy ->
              [sort_bids ({price = feed.price; size = feed.remaining_size;
                           order_id = feed.order_id; sequence = feed.sequence} :: bid_book);
               ask_book]
          | Sell ->
              [bid_book;
               sort_asks ({price = feed.price; size = feed.remaining_size;
                           order_id = feed.order_id; sequence = feed.sequence} :: ask_book)]
          | OrderKindError -> book (* Should the process be restarted here? *)
        )
    | Received -> book (* Do Nothing *)
    | Match ->
        let mkr_id = feed.maker_order_id in
        let tkr_id = feed.taker_order_id in
        let (mkr_book, mkr_odr) = (* Return a tuple, the book the order is in, and the order *)
          let mkr_from_bids =
            List.filter (fun (x : level_3_entry) -> x.order_id = mkr_id) bid_book
          in
          let mkr_from_asks =
            List.filter (fun (x : level_3_entry) -> x.order_id = mkr_id) ask_book
          in
          match (mkr_from_bids, mkr_from_asks) with
          | (m, []) -> (`BidBook, m) (* m is a list *)
          | ([], m) -> (`AskBook, m)
          | (_, _)-> (`None, [])
        in
        let (tkr_book, tkr_odr) =
          let tkr_from_bids =
            List.filter (fun (x : level_3_entry) -> x.order_id = tkr_id) bid_book
          in
          let tkr_from_asks =
            List.filter (fun (x : level_3_entry) -> x.order_id = tkr_id) ask_book
          in
          match (tkr_from_bids, tkr_from_asks) with
          | (t, []) -> (`BidBook, t)
          | ([], t) -> (`AskBook, t)
          | (_, _) -> (`None, [])
        in
        let other_bids =
          List.filter
          (fun (x : level_3_entry) -> (x.order_id <> mkr_id) && (x.order_id <> tkr_id)) bid_book
        in
        let other_asks =
          List.filter
          (fun (x : level_3_entry) -> (x.order_id <> mkr_id) && (x.order_id <> tkr_id)) ask_book
        in
        let new_mkr_order =
          match mkr_odr with
          | [] -> []
          | hd :: tl ->
              if feed.size >= hd.size then []
              else
                [{price = hd.price; size = hd.size -. feed.size;
                  order_id = hd.order_id; sequence = hd.sequence}]
        in
        let new_tkr_order = 
          match tkr_odr with
          | [] -> []
          | hd :: tl ->
              if feed.size >= hd.size then []
              else
                [{price = hd.price; size = hd.size -. feed.size;
                  order_id = hd.order_id; sequence = hd.sequence}]
        in
        let new_book =
          match (mkr_book, tkr_book) with
          | (`BidBook, `AskBook) ->
              [sort_bids (new_mkr_order @ other_bids); sort_asks (new_tkr_order @ other_asks)]
          | (`AskBook, `BidBook) ->
              [sort_bids (new_tkr_order @ other_bids); sort_asks (new_mkr_order @ other_asks)]
          | (_, _) -> [other_bids; other_asks]
        in
        new_book
    | Change ->
        let new_entry =
          {price = feed.price; size = feed.size; order_id = feed.order_id; sequence = feed.sequence}
        in
        (
          match feed.side with
          | Buy ->
              let other_bids =
                List.filter (fun (x : level_3_entry) -> x.order_id <> feed.order_id) bid_book
              in
              [sort_bids (new_entry :: other_bids); ask_book]
          | Sell ->
              let other_asks =
              List.filter (fun (x : level_3_entry) -> x.order_id <> feed.order_id) ask_book
              in
              [bid_book; sort_asks (new_entry :: other_asks)]
          | OrderKindError -> book (* TODO: is this right? Should the whole process be restarted? *)
        )
    | WsDataError -> book (* TODO: is this right? Should the whole process be restarted? *)

  (***** PUBLIC FUNCTIONS *****)

  (* Get ticker for BtcUsd*)
  let get_last_trade () = 
    lwt json_response = get_it "products/BTC-USD/ticker" in
    let i x = member x json_response |> to_int in
    let f x = member x json_response |> to_string |> float_of_string in
    let datetime_list =
      member "time" json_response |> to_string
      |> Core.Core_string.split_on_chars ~on:['-'; ':'; '.'; 'T']
    in
    let year = List.nth datetime_list 0 |> int_of_string in
    let month = List.nth datetime_list 1 |> int_of_string in
    let day = List.nth datetime_list 2 |> int_of_string in
    let hour = List.nth datetime_list 3 |> int_of_string in
    let minute = List.nth datetime_list 4 |> int_of_string in
    let second = List.nth datetime_list 5 |> int_of_string in
    let unix_time =
      {Unix.tm_sec = second; Unix.tm_min = minute; Unix.tm_hour = hour; Unix.tm_mday = day;
       Unix.tm_mon = month; Unix.tm_year = year; Unix.tm_wday = 0; Unix.tm_yday = 0;
       Unix.tm_isdst = false}
    in
    Lwt.return {trade_id = i "trade_id"; price = f "price"; size = f "size"; time = unix_time}

  (* Get Products *)
  let get_products () =
    let open Yojson.Basic.Util in
    lwt json_response = get_it "/products" in
    let json_list = to_list json_response in
    let f x json = member x json |> to_float in
    let i x json = member x json |> to_int in
    let s x json = member x json |> to_string in
    let c x json = member x json |> to_string |> currency_of_string in
    let product_of_assoc p =
      {id = s "id" p; base_currency = c "base_currency" p; quote_currency = c "quote_currency" p;
       base_min_size = f "base_min_size" p; base_max_size = i "base_max_size" p;
       quote_increment = f "quote_increment" p; display_name = s "display_name" p}
    in
    Lwt.return @@ List.map (product_of_assoc) json_list

  (* Get BtcUsd Info *)
  let get_btcusd_info () =
    lwt product_list = get_products () in
    Lwt.return @@ List.hd product_list

  (* Get Product Order Book - first list are bids, second list are asks *)
  (* TODO: Handle `Two and `Three *)
  let get_product_orderbook ?(level=`One) (sym : symbol) =
    let level_string =
      match level with
      | `One -> "?level=1"
      | `Two -> "?level=2"
      | `Three -> "?level=3"
    in
    lwt json_response =
      get_it ("/products/" ^ (string_of_symbol sym) ^ "/book" ^ level_string)
    in
    let best_bid_price =
      member "bids" json_response |> to_list |> List.hd |> to_list |> List.hd |> to_string
      |> float_of_string
    in
    let best_bid_size =
      member "bids" json_response |> to_list |> List.hd |> to_list |> fun l -> List.nth l 1
      |> to_string |> float_of_string
    in
    let best_ask_price =
      member "asks" json_response |> to_list |> List.hd |> to_list |> List.hd |> to_string
      |> float_of_string
    in
    let best_ask_size =
      member "asks" json_response |> to_list |> List.hd |> to_list |> fun l -> List.nth l 1
      |> to_string |> float_of_string
    in
    Lwt.return
    [[{price = best_bid_price; amount = best_bid_size; timestamp = Unix.gmtime @@ Unix.time ()}];
     [{price = best_ask_price; amount = best_ask_size; timestamp = Unix.gmtime @@ Unix.time ()}]
    ]

  (* Get the Level 2 orderbook for BtcUsd - [[bids]; [asks]]*)
  let get_lvl_2_orderbook () =
    let open Yojson.Basic.Util in
    let json_to_entry json =
      let entry_price = json |> List.hd |> to_string |> float_of_string in
      let entry_size = List.nth json 1 |> to_string |> float_of_string in
      let entry_num_orders = List.nth json 2 |> to_int in
      {price = entry_price; size = entry_size; num_orders = entry_num_orders}
    in
    lwt json_response = get_it "/products/BTC-USD/book?level=2" in
    let bid_book =
      member "bids" json_response |> to_list |> List.map to_list |> List.map json_to_entry
    in
    let ask_book =
      member "asks" json_response |> to_list |> List.map to_list |> List.map json_to_entry
    in
    Lwt.return [bid_book; ask_book]

  (* Get the Level 3 orderbook for BtcUsd *)
  let get_lvl_3_orderbook () =
    let open Yojson.Basic.Util in
    let json_to_entry ~seq json =
      let entry_price = json |> List.hd |> to_string |> float_of_string in
      let entry_size = List.nth json 1 |> to_string |> float_of_string in
      let entry_order_id = List.nth json 2 |> to_string in
      {price = entry_price; size = entry_size; order_id = entry_order_id; sequence = seq}
    in
    lwt json_response = get_it "/products/BTC-USD/book?level=3" in
    let sequence = member "sequence" json_response |> to_int in
    let bid_book =
      member "bids" json_response |> to_list |> List.map to_list
      |> List.map (json_to_entry ~seq:sequence)
    in
    let ask_book =
      member "asks" json_response |> to_list |> List.map to_list
      |> List.map (json_to_entry ~seq:sequence)
    in
    Lwt.return (sequence, [bid_book; ask_book])

  (* Get Accounts *)
  let get_accounts () =
    let open Yojson.Basic.Util in
    lwt json_response = get_it ~pvt:true "accounts" >>= fun json -> Lwt.return @@ to_list json in
    let btc_acct_json = json_response |> List.hd in
    let usd_acct_json = json_response |> List.rev |> List.hd in
    let s x json = member x json |> to_string in
    let f x json = member x json |> to_string |> float_of_string in
    let c x json = member x json |> to_string |> currency_of_string in
    let btc_account =
      {id = s "id" btc_acct_json;
       currency = c "currency" btc_acct_json;
       balance = f "balance" btc_acct_json;
       hold = f "hold" btc_acct_json;
       available = f "available" btc_acct_json;
       profile_id = s "profile_id" btc_acct_json}
    in
    let usd_account =
      {id = s "id" usd_acct_json;
       currency = c "currency" usd_acct_json;
       balance = f "balance" usd_acct_json;
       hold = f "hold" usd_acct_json;
       available = f "available" usd_acct_json;
       profile_id = s "profile_id" usd_acct_json}
    in
    Lwt.return [btc_account; usd_account]

  (* TODO: Get Account History *)
  (* TODO: Get Holds *)

  (* New Order *)
  let new_order (o : order) =
    let price = (string_of_float o.price) ^ if (mod_float o.price 1.0) = 0.0 then "0" else "" in
    let size = (string_of_float o.size) ^ if (mod_float o.size 1.0) = 0.0 then "0" else "" in
    let json_request =
      "{\"size\":" ^ size ^ "," ^
      "\"price\":" ^ price ^ "," ^
      "\"side\":\"" ^ (string_of_order_kind o.side) ^ "\"," ^
      "\"product_id\":\"BTC-USD\"}"
    in
    lwt json_response = post_it ~body:json_request "orders" in
    let s x = member x json_response |> to_string in
    let f x = member x json_response |> to_string |> float_of_string in
    let ok x = member x json_response |> to_string |> order_kind_of_string in
    let sym x = member x json_response |> to_string |> symbol_of_string in
    let stp_ x = member x json_response |> to_string |> stp_of_string in
    try_lwt
      Lwt.return @@ Success {id = s "id"; price = f "price"; size = f "size";
                             product_id = sym "product_id"; side = ok "side"; stp = stp_ "stp"}
    with
    | _ ->
        let fail_message = member "message" json_response |> to_string in
        Lwt.return @@ Failure fail_message

  (* Cancel Order *)
  let cancel_order order_id =
    delete_it ("orders/" ^ order_id) >>= fun s -> Lwt.return ()

  (* Get List of Orders *)
  let get_list_orders () =
    let open Yojson.Basic.Util in
    lwt json_response = get_it ~pvt:true "orders" >>= fun json -> Lwt.return @@ to_list json in
    let s x json = member x json |> to_string in
    let so x json = member x json |> to_string_option in
    let f x json = member x json |> to_string |> float_of_string in
    let ok x json = member x json |> to_string |> order_kind_of_string in
    let sym x json = member x json |> to_string |> symbol_of_string in
    let b x json = member x json |> to_bool in 
    Lwt_list.map_s
    (fun j -> Lwt.return
      {id = s "id" j; price = f "price" j; size = f "size" j; product_id = sym "product_id" j;
       side = ok "side" j; created_at = s "created_at" j; done_at = so "done_at" j;
       done_reason = so "done_reason" j; filled_size = f "filled_size" j;
       fill_fees = f "fill_fees" j; status = s "status" j; settled = b "settled" j}
    )
    json_response

  (* Get Account History *)
  let account_history ~ccy ~fetch () =
    let elem =
      match fetch with
      | `Amount -> "amount"
      | `Balance -> "balance"
    in
    lwt account_id =
      get_accounts ()
      >>= fun acct_list -> List.filter (fun (a : account) -> a.currency = ccy) acct_list |> List.hd
      |> fun (acct : account) -> Lwt.return acct.id 
    in
    lwt json_response = get_it ~pvt:true ("accounts/" ^ account_id ^ "/ledger") in
    json_response |> to_list |> List.map (member elem) |> List.map to_string |>
    List.map float_of_string |> Lwt.return

  (* TODO: Get Order *)

  end
