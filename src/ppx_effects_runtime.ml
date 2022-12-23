(** These functions are exported for use by the [ppx_effects] PPX. They are not
    intended to be called directly by users. *)

let raise = Stdlib.raise

include Stdlib.Effect

module Enclose_impl = struct
  let perform = perform
end

module Deep = struct
  include Deep

  module Open_on_rhs = struct
    let continue = continue
    let discontinue = discontinue
    let discontinue_with_backtrace = discontinue_with_backtrace
  end
end

module Shallow = struct
  include Shallow
  module Open_on_rhs = struct end
end
