type amsTracker

val start : (int -> int -> int -> unit) -> amsTracker

val stop : amsTracker -> unit

val print_callback : int -> int -> int  -> unit
