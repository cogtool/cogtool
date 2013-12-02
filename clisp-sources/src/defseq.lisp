;; Definitions for the standard sequence types
;; (in conjunction with SEQUENCE.D)
;; Bruno Haible 9.7.1989, 1.8.1989, 2.8.1989

(in-package "SYSTEM")

(%defseq
  (vector
    'LIST
    #'identity
    #'list-upd
    #'list-endtest
    #'list-fe-init
    #'list-upd
    #'list-endtest
    #'list-access
    #'list-access-set
    #'identity
    #'ext::list-length-proper
    #'make-list
    #'list-elt
    #'list-set-elt
    #'list-init-start
    #'list-fe-init-end
) )

(%defseq ; VECTOR stands for GENERAL-VECTOR
  (vector
    'VECTOR
    #'vector-init
    #'vector-upd
    #'vector-endtest
    #'vector-fe-init
    #'vector-fe-upd
    #'vector-fe-endtest
    #'aref
    #'sys::store
    #'identity
    #'vector-length
    #'make-array
    #'aref
    #'sys::store
    #'vector-init-start
    #'vector-fe-init-end
) )

(%defseq
  (vector
    'STRING
    #'vector-init
    #'vector-upd
    #'vector-endtest
    #'vector-fe-init
    #'vector-fe-upd
    #'vector-fe-endtest
    #'char
    #'sys::store
    #'identity
    #'vector-length
    #'make-string
    #'char
    #'sys::store
    #'vector-init-start
    #'vector-fe-init-end
) )

(mapc
  #'(lambda (n &aux (eltype (list 'UNSIGNED-BYTE n)))
      (%defseq
        (vector
          n ; n stands for `(VECTOR (UNSIGNED-BYTE ,n))
          #'vector-init
          #'vector-upd
          #'vector-endtest
          #'vector-fe-init
          #'vector-fe-upd
          #'vector-fe-endtest
          (if (= n 1) #'bit #'aref)
          #'sys::store
          #'identity
          #'vector-length
          (if (= n 1)
            #'make-bit-vector
            #'(lambda (length) (make-array length :element-type eltype)))
          (if (= n 1) #'bit #'aref)
          #'sys::store
          #'vector-init-start
          #'vector-fe-init-end
    ) ) )
  '(1 2 4 8 16 32)
)

(%defseq ; (VECTOR NIL)
  (vector
    0
    #'vector-init
    #'vector-upd
    #'vector-endtest
    #'vector-fe-init
    #'vector-fe-upd
    #'vector-fe-endtest
    #'aref
    #'sys::store
    #'identity
    #'vector-length
    #'(lambda (length) (make-array length :element-type nil))
    #'aref
    #'sys::store
    #'vector-init-start
    #'vector-fe-init-end))
