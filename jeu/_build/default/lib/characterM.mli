
module type CharacterMonad = sig
  type  'a t 
end

module CharacterMonad : CharacterMonad


val create_character : float * float -> string -> float -> float -> Character.t

val vel : float -> float -> unit CharacterMonad.t

val deplacer : unit CharacterMonad.t

val airb : bool -> unit CharacterMonad.t

