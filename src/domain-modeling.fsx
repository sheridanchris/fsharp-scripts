#r "nuget: FPrimitive, 3.2.0"
#r "nuget: FSharp.UMX, 1.1.0"

open System
open FPrimitive
open FSharp.UMX

[<Measure>]
type UserId

type Username = private Username of string
type ValidEmailAddress = private ValidEmailAddress of string
type VerifiedEmailAddress = private VerifiedEmailAddress of ValidEmailAddress
type UnverifiedEmailAddress = private UnverifiedEmailAddress of ValidEmailAddress

type EmailAddress =
  | Verified of VerifiedEmailAddress
  | Unverified of UnverifiedEmailAddress

type User =
  { Id: Guid<UserId>
    Username: Username
    EmailAddress: EmailAddress }

type CheckUsernameExists = Username -> bool
type CheckEmailAddressExists = ValidEmailAddress -> bool

type CreateUserError =
  | UsernameExists
  | EmailAddressExists
  | UsernameAndEmailAddressExists

type CreateUser = Username -> ValidEmailAddress -> Result<User, CreateUserError>

module Username =
  let create value =
    Spec.def<string>
    |> Spec.notNull "Username can't be null."
    |> Spec.notEmpty "Username can't be empty."
    |> Spec.lengthBetween 1 20 "Username length must be between 1 and 20 characters."
    |> Spec.alphanum "Username must be alphanumeric."
    |> Spec.createModel Username value

module ValidEmailAddress =
  let create value =
    Spec.def<string>
    |> Spec.notNull "Email can't be null."
    |> Spec.notEmpty "Email can't be empty."
    |> Spec.matches @"[^@]+@[^\.]+\..+" "Email must be a valid email address."

module User =
  let createUser
    (checkUsernameExists: CheckUsernameExists)
    (checkEmailAddressExists: CheckEmailAddressExists)
    : CreateUser =
    fun username validEmailAddress ->
      let doesUsernameExist = checkUsernameExists username
      let doesEmailAddressExist = checkEmailAddressExists validEmailAddress

      if doesUsernameExist && doesEmailAddressExist then
        Error UsernameAndEmailAddressExists
      elif doesUsernameExist then
        Error UsernameExists
      elif doesEmailAddressExist then
        Error EmailAddressExists
      else
        Ok
          { Id = % Guid.NewGuid()
            Username = username
            EmailAddress = Unverified(UnverifiedEmailAddress validEmailAddress) }
