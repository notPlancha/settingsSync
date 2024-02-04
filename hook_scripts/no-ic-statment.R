#!/usr/bin/env Rscript

# pre-commit hook to prevent the use of `ic()` statements in R code.
# https://github.com/lorenzwalthert/precommit/blob/main/inst/hooks/exported/no-browser-statement.R
# Copyright (C) 2019 Lorenz Watlhert
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

files <- list.files("R", recursive = TRUE, full.names = TRUE, pattern = "\\.R$")

will_stop <- F
files_with_ic <- character(0)
no_ic_statement <- function(path) {
  pd <- getParseData(parse(path, keep.source = TRUE))
  if (any(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"] == "ic")) {
    will_stop <<- T
    files_with_ic <<- c(files_with_ic, path)
  }
}

for (file in files) {
  no_ic_statement(file)
}
if (will_stop) {
  paste("The following files have an ic() call: \n", paste(files_with_ic, collapse="\n ")) |> stop(call. = FALSE)
}
