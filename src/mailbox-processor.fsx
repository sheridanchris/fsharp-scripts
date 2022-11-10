open System
open System.Collections.Generic

type User = { Id: Guid; Username: string }

type ChatMessage =
  { From: Guid
    Username: string
    Message: string }

type Msg =
  | UserJoined of User
  | UserDisconnected of User
  | NewChatMessage of ChatMessage
  | Exit

type State =
  { Participants: User list
    ChatMessages: ChatMessage list }

let chatProcessor =
  MailboxProcessor.Start(fun inbox ->
    let rec messageLoop (state: State) =
      async {
        let! eventMsg = inbox.Receive()

        match eventMsg with
        | Exit -> return ()
        | UserJoined user -> return! messageLoop { state with Participants = user :: state.Participants }
        | NewChatMessage message -> return! messageLoop { state with ChatMessages = message :: state.ChatMessages }
        | UserDisconnected user ->
          let userOption =
            state.Participants
            |> List.indexed
            |> List.tryFind (fun (_, u) -> user.Id = u.Id)

          match userOption with
          | None -> return! messageLoop state
          | Some (index, _) -> return! messageLoop { state with Participants = List.removeAt index state.Participants }
      }

    let initialState = { Participants = []; ChatMessages = [] }
    messageLoop initialState)

let userJoined =
  UserJoined
    { Id = Guid.NewGuid()
      Username = "christian" }

chatProcessor.Post userJoined