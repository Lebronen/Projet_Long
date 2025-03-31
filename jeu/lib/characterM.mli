module type CharacterM = sig
    type 'a t = Character.t -> 'a * Character.t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
  