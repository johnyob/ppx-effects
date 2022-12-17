(** These functions are exported for use by the [ppx_effects] PPX. They are not
    intended to be called directly by users. *)

let raise = Stdlib.raise

include Stdlib.Effect