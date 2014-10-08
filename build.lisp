(ql:quickload "file-to-c-array")
(sb-ext:save-lisp-and-die "file-to-c-array.exe" :toplevel #'file-to-c-array::main :executable t :purify t)
