#r "nuget: FPrimitive, 3.2.0"

open FPrimitive

type NonEmptyString =
  private NonEmptyString of string with
    static member create x : Result<NonEmptyString, Map<string, string list>> =
      Spec.def<string>
      |> Spec.notNull "should not be null"
      |> Spec.notEmpty "should not be empty"
      |> Spec.createModel NonEmptyString x

let nonEmptyString = NonEmptyString.create "hello"
printfn "%A" nonEmptyString

let accessOnceThenRevoke =
  Access.func (fun () -> printfn "Hello, World!")
  |> Access.once
  |> Access.revokable

Access.eval () accessOnceThenRevoke
Access.eval () accessOnceThenRevoke
