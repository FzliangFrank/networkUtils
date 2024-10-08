
dev_msg = function(..., dev=T) {
  args = as.list(match.call())[-1]
  args = args |> discard_at("dev")
  do.call(cli::cli_inform,args)
}
