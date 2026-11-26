# Verifies the modular ESCAP code is identical to the original app.R
log <- file("D:/API_R/dev/verify_split_log.txt", open = "wt"); sink(log); sink(log, type = "message")
on.exit({ sink(type = "message"); sink(); close(log) })
setwd("D:/API_R/ESCAP")
if (!dir.exists("www")) dir.create("www")
src <- list.files("D:/API_R/www", full.names = TRUE)
file.copy(src, "www", overwrite = FALSE)
cat("www files:", length(list.files("www")), "\n")
orig <- parse("D:/API_R/app.R", encoding = "UTF-8")
get_assign <- function(exprs, nm) for (e in exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nm))) return(e[[3]])
# 1. parse every new file
for (f in c("global.R", "ui.R", "server.R", list.files(c("ui", "server"), full.names = TRUE))) {
  r <- tryCatch({ parse(f, encoding = "UTF-8"); "ok" }, error = function(e) conditionMessage(e))
  if (r != "ok") cat("PARSE ERROR", f, r, "\n")
}
# 2. server body equality
srv_old <- get_assign(orig, "server")
old_body <- as.list(srv_old[[3]])[-1]
new_body <- unlist(lapply(sort(list.files("server", full.names = TRUE)), function(f) as.list(parse(f, encoding = "UTF-8"))), recursive = FALSE)
cat("server exprs old/new:", length(old_body), length(new_body), "\n")
same <- mapply(function(a, b) identical(deparse(a), deparse(b)), old_body, new_body)
cat("server identical:", all(same), " first mismatch:", which(!same)[1], "\n")
# 3. UI equality
suppressPackageStartupMessages(source("global.R", encoding = "UTF-8"))
norm <- function(x) gsub("[0-9]{3,}", "N", as.character(x))
ui_old <- eval(get_assign(orig, "ui"))
source("ui.R", encoding = "UTF-8")
cat("ui identical:", identical(norm(ui_old), norm(ui)), "\n")
cat("VERIFY DONE\n")
