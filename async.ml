let run_in_background f =
  Thread.create f ()

let (!->) = run_in_background
let (!!!) = Thread.kill
let (???) = Thread.join
