# paste normal object
nL = list(
  id = 1,
  object = "Apple",
  category = "Fruit",
  size = 24.22
)
x_html = pasteDetails(nL)
x_paste = pasteDetails(nL, sep.attr = "\n")
cat(x_paste)

# paste a graph attributes
