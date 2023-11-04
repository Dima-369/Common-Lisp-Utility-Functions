(asdf:defsystem "dima"
  :description "Copies of some functions I use in Emacs Lisp with modifications. Always :use this."
  :version "0.0.1"
  :depends-on (
               #:str
               ;; for `thread-first' and `thread-last'
               #:arrow-macros
               ;; JSON library
               #:shasht
               ;; for `sha1'
               #:ironclad)
  :components ((:file "dima")))
