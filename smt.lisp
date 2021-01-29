(in-package #:smt)


(push "/home/amir/Work/Lisp/smt/cwmn.lisp" smt-engine::*rulepaths*)
(dolist (path smt-engine::*rulepaths*)
  (load path))

(smt-engine::ruledocs)
