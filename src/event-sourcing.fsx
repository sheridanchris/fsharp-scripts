open System

type TransactionType =
  | Deposit
  | Withdrawl

type Transaction = { Amount: decimal; Type: TransactionType }
type State = { Balance: decimal; Transactions: Transaction list }

type Event =
  | Withdrew of amount: decimal
  | Deposited of amount: decimal

type Command =
  | Deposit of amount: decimal
  | Withdraw of amount: decimal

let initialState = { Balance = 0.00m; Transactions = [] }

// Apply an event (a fact, something that happened in the past) to the state
let evolve state event =
  match event with
  | Withdrew amount ->
    let balance = state.Balance - amount
    let transaction = { Amount = amount; Type = TransactionType.Withdrawl }
    { state with Balance = balance; Transactions = transaction :: state.Transactions }
  | Deposited amount ->
    let balance = state.Balance + amount
    let transaction = { Amount = amount; Type = TransactionType.Deposit }
    { state with Balance = balance; Transactions = transaction :: state.Transactions }

// Decide what happend (events) based on a command
let decide command state =
  match command, state with
  | Deposit amount, _ -> [ Deposited amount ]
  | Withdraw amount, state when state.Balance - amount >= 0.00m -> [ Withdrew amount ]
  | _ -> []

let build = List.fold evolve
let rebuild = build initialState