(in-package #:smt)


(push "/home/amir/Work/Lisp/smt/lang/cwmn.lisp" smt-engine::*rulepaths*)
(dolist (path smt-engine::*rulepaths*)
  (load path))


