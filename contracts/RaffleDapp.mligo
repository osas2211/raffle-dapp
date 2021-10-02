type openRaffleParameter = tez * timestamp * string option * bytes

type buyTicketParameter = unit

type closeRaffleParameter = nat

type raffleEntrypoints = 
| OpenRaffle of openRaffleParameter
| BuyTicket of buyTicketParameter
| CloseRaffle of closeRaffleParameter

type storage = {
    admin: address;
    close_date: timestamp;
    jackpot: tez;
    description: string;
    raffle_is_open: bool;
    players: (address) set;
    sold_tickets: (nat, address) map;
    winning_ticket_number_hash : bytes;
}

type returnType = operation list * storage

let open_raffle(jackpot_amount, close_date, description, winning_ticket_number_hash, store : tez * timestamp * string option * bytes * storage) : returnType =
    if Tezos.source <> store.admin 
    then
      (failwith "Administrator not recognized.": returnType)
    else
        if not store.raffle_is_open
        then
            if Tezos.amount < jackpot_amount 
            then
            (failwith "The administrator does not own enought tz.": returnType)
            else
            let today = Tezos.now in
            let seven_day = 7 * 86400 in
            let in_7_day = today + seven_day in
            let is_close_date_not_valid = close_date < in_7_day in

            if is_close_date_not_valid 
            then
                (failwith "The raffle must remain open for at least 7 days.": returnType)
            else
                let desc : string =
                    match description with
                    | Some d -> d
                    | None -> store.description // leaves the description unchanged
                in
                
                let store = {store with jackpot = jackpot_amount; close_date = close_date; raffle_is_open = True; winning_ticket_number_hash = winning_ticket_number_hash ; description = desc ; } in
                (([] : operation list), store)
        else
            (failwith "A raffle is already open": returnType)

let buy_ticket (_param, store : buyTicketParameter * storage ) : returnType =
    if store.raffle_is_open 
    then 
        let ticket_price : tez = 1tez in
        let current_player : address = Tezos.sender in
        
        if Tezos.amount <= ticket_price 
        then
            (failwith "The sender does not own enough tz to buy a ticket.": returnType)
        else
            if Set.mem current_player store.players  
            then
                (failwith "Each player can participate only once.": returnType)
            else
                let ticket_id = Set.size store.players in

                let update_players : address set = Set.add current_player store.players in

                let new_sold_tickets : (nat, address) map =
                    Map.update 
                        ticket_id (Some current_player) store.sold_tickets 
                in 

                let store = { store with players = update_players ; sold_tickets = new_sold_tickets ; } in
                (([] : operation list), store)
    else
        (failwith "The raffle is closed." : returnType)

let close_raffle (winning_ticket_number, store : closeRaffleParameter * storage ) : returnType =
    let _operations : operation list = [] in
    
    if Tezos.source <> store.admin
    then
        (failwith "Administrator not recognized" : returnType)
    else
        if store.raffle_is_open
        then
            if Tezos.now < store.close_date 
            then
                (failwith "The raffle must remain open for at least 7 days": returnType)
            else
                let winning_ticket_number_bytes : bytes = Bytes.pack winning_ticket_number in
                let winning_ticket_number_hash : bytes = Crypto.sha256 winning_ticket_number_bytes in

                if winning_ticket_number_hash <> store.winning_ticket_number_hash
                then
                    (failwith "The hash does not match the hash of the winning ticket." : returnType)
                else
                    let number_of_players = Set.size store.players in
                    let winning_ticket_id = winning_ticket_number mod number_of_players in

                    let winner : address =
                        match Map.find_opt winning_ticket_id store.sold_tickets with
                            Some (a) -> a
                        | None -> (failwith "Winner address not found" : address)
                    in

                    let receiver : unit contract = 
                        match (Tezos.get_contract_opt (winner) : unit contract option) with
                          Some contract -> contract
                        | None -> (failwith "Winner contract not found" : unit contract) 
                    in

                    let _op : operation = Tezos.transaction unit store.jackpot receiver in 

                    let store = { store with jackpot = 0tez ; close_date = (0 : timestamp) ; description = ("raffle is currently closed" : string) ; raffle_is_open = False ; players = (Set.empty : address set) ; sold_tickets = (Map.empty : (nat, address) map) ; } in
                    (([] : operation list), store)  
        else
            (failwith "The raffle is closed." : returnType)
            
let main (action, store : raffleEntrypoints * storage): returnType =
    let return: returnType = 
      match action with
      | OpenRaffle param -> open_raffle (param.0, param.1, param.2, param.3, store)
      | BuyTicket param -> buy_ticket (param, store)
      | CloseRaffle param -> close_raffle (param, store)

    in return
