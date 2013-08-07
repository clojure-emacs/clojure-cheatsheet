;;; helm-clojure-cheatsheet.el --- A helm version of the Clojure cheatsheet.
;; Copyright 2013 Kris Jenkins

;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: clojure nrepl cheatsheet helm
;; URL: https://github.com/krisajenkins/helm-clojure-cheatsheet
;; Created: 7th August 2013
;; Version: 0.1.0
;; Package-Requires: ((helm "1.5.3") (nrepl "0.1.8"))

;;; Commentary:

(require 'helm)
(require 'nrepl)

;;; Code:

(defun helm-clojure-cheatsheet-join
  (separator &rest items)
  (mapconcat 'identity items separator))

(helm-clojure-cheatsheet-join "/" "this" "that")


(defun helm-clojure-cheatsheet-raw-item-to-helm-item
  (raw-data)
  "Convert a simple-to-write data structure into the helm-source format."
  `((name . ,(apply 'helm-clojure-cheatsheet-join " : " (car raw-data)))
    (candidates ,@(cdr raw-data))
    (match . ((lambda (candidate)
    		(helm-mp-3-match (helm-clojure-cheatsheet-join " " candidate ,@(car raw-data))))))
    (action . (("Lookup Docs" . nrepl-doc-handler)
	       ("Lookup Source" . nrepl-src-handler)))))

(defvar helm-source-clojure-cheatsheet
  (mapcar 'helm-clojure-cheatsheet-raw-item-to-helm-item
	  '((("Primitives" "Numbers" "Arithmetic")
	     clojure.core/+
	     clojure.core/-
	     clojure.core/*
	     clojure.core//
	     clojure.core/quot
	     clojure.core/rem
	     clojure.core/mod
	     clojure.core/dec
	     clojure.core/inc
	     clojure.core/max
	     clojure.core/min)
	    (("Primitives" "Numbers" "Compare")
	     clojure.core/=
	     clojure.core/==
	     clojure.core/not=
	     clojure.core/<
	     clojure.core/>
	     clojure.core/<=
	     clojure.core/>=
	     clojure.core/compare)
	    (("Primitives" "Numbers" "Bitwise")
	     clojure.core/bit-and
	     clojure.core/bit-and-not
	     clojure.core/bit-clear
	     clojure.core/bit-flip
	     clojure.core/bit-not
	     clojure.core/bit-or
	     clojure.core/bit-set
	     clojure.core/bit-shift-left
	     clojure.core/bit-shift-right
	     clojure.core/bit-test
	     clojure.core/bit-xor)
	    (("Primitives" "Numbers" "Cast")
	     clojure.core/byte
	     clojure.core/short
	     clojure.core/long
	     clojure.core/int
	     clojure.core/float
	     clojure.core/double
	     clojure.core/bigdec
	     clojure.core/bigint
	     clojure.core/biginteger
	     clojure.core/num
	     clojure.core/rationalize)
	    (("Primitives" "Numbers" "Test")
	     clojure.core/nil?
	     clojure.core/identical?
	     clojure.core/zero?
	     clojure.core/pos?
	     clojure.core/neg?
	     clojure.core/even?
	     clojure.core/odd?)
	    (("Primitives" "Numbers" "Random")
	     clojure.core/rand
	     clojure.core/rand-int)
	    (("Primitives" "Numbers" "BigDecimal")
	     clojure.core/with-precision)
	    (("Primitives" "Numbers" "Unchecked")
	     clojure.core/*unchecked-math*
	     clojure.core/unchecked-add
	     clojure.core/unchecked-add-int
	     clojure.core/unchecked-byte
	     clojure.core/unchecked-char
	     clojure.core/unchecked-dec
	     clojure.core/unchecked-dec-int
	     clojure.core/unchecked-divide-int
	     clojure.core/unchecked-double
	     clojure.core/unchecked-float
	     clojure.core/unchecked-inc
	     clojure.core/unchecked-inc-int
	     clojure.core/unchecked-int
	     clojure.core/unchecked-long
	     clojure.core/unchecked-multiply
	     clojure.core/unchecked-multiply-int
	     clojure.core/unchecked-negate
	     clojure.core/unchecked-negate-int
	     clojure.core/unchecked-remainder-int
	     clojure.core/unchecked-short
	     clojure.core/unchecked-subtract
	     clojure.core/unchecked-subtract-int)
	    (("Primitives" "Strings" "Create")
	     clojure.core/str
	     clojure.core/format)
	    (("Primitives" "Strings" "Use")
	     clojure.core/count
	     clojure.core/get
	     clojure.core/subs
	     clojure.core/compare
	     clojure.string/join
	     clojure.string/escape
	     clojure.string/split
	     clojure.string/split-lines
	     clojure.string/replace
	     clojure.string/replace-first
	     clojure.string/reverse)
	    (("Primitives" "Strings" "Regex")
	     clojure.core/re-find
	     clojure.core/re-groups
	     clojure.core/re-matcher
	     clojure.core/re-matches
	     clojure.core/re-pattern
	     clojure.core/re-seq
	     clojure.string/replace
	     clojure.string/replace-first)
	    (("Primitives" "Strings" "Letters")
	     clojure.string/capitalize
	     clojure.string/lower-case
	     clojure.string/upper-case)
	    (("Primitives" "Strings" "Trim")
	     clojure.string/trim
	     clojure.string/trim-newline
	     clojure.string/triml
	     clojure.string/trimr)
	    (("Primitives" "Strings" "Test")
	     clojure.core/char
	     clojure.core/char?
	     clojure.core/string?
	     clojure.string/blank?)
	    (("Primitives" "Other" "Characters")
	     clojure.core/char
	     clojure.core/char-name-string
	     clojure.core/char-escape-string)
	    (("Primitives" "Other" "Keywords")
	     clojure.core/keyword
	     clojure.core/keyword?
	     clojure.core/find-keyword)
	    (("Primitives" "Other" "Symbols")
	     clojure.core/symbol
	     clojure.core/symbol?
	     clojure.core/gensym)
	    (("Primitives" "Other" "Data Readers")
	     clojure.core/*data-readers*
	     clojure.core/*default-data-reader-fn*)
	    (("Collections" "Generic Ops")
	     clojure.core/count
	     clojure.core/empty
	     clojure.core/not-empty
	     clojure.core/into
	     clojure.core/conj)
	    (("Collections" "Walking")
	     clojure.walk/walk
	     clojure.walk/prewalk
	     clojure.walk/prewalk-demo
	     clojure.walk/prewalk-replace
	     clojure.walk/postwalk
	     clojure.walk/postwalk-demo
	     clojure.walk/postwalk-replace)
	    (("Collections" "Content tests")
	     clojure.core/distinct?
	     clojure.core/empty?
	     clojure.core/every?
	     clojure.core/not-every?
	     clojure.core/some
	     clojure.core/not-any?)
	    (("Collections" "Capabilities")
	     clojure.core/sequential?
	     clojure.core/associative?
	     clojure.core/sorted?
	     clojure.core/counted?
	     clojure.core/reversible?)
	    (("Collections" "Type tests")
	     clojure.core/coll?
	     clojure.core/list?
	     clojure.core/vector?
	     clojure.core/set?
	     clojure.core/map?
	     clojure.core/seq?)
	    (("Collections" "Lists" "Create")
	     clojure.core/list
	     clojure.core/list*)
	    (("Collections" "Lists" "Examine")
	     clojure.core/first
	     clojure.core/nth
	     clojure.core/peek)
	    (("Collections" "Lists" "'Change'")
	     clojure.core/cons
	     clojure.core/conj 
	     clojure.core/rest
	     clojure.core/pop)	    
	    (("Collections" "Vectors" "Create")
	     clojure.core/vec
	     clojure.core/vector
	     clojure.core/vector-of)
	    (("Collections" "Vectors" "Examine")
	     clojure.core/get
	     clojure.core/peek)
	    (("Collections" "Vectors" "'Change'")
	     clojure.core/assoc
	     clojure.core/pop
	     clojure.core/subvec
	     clojure.core/replace
	     clojure.core/conj
	     clojure.core/rseq)
	    (("Collections" "Vectors" "Ops")
	     clojure.core/mapv
	     clojure.core/filterv
	     clojure.core/reduce-kv)	    

	    (("Other" "XML")
	     clojure.core/xml-seq
	     clojure.xml/parse)
	    (("Other" "REPL")
	     clojure.core/*1
	     clojure.core/*2
	     clojure.core/*3
	     clojure.core/*e
	     clojure.core/*print-dup*
	     clojure.core/*print-length*
	     clojure.core/*print-level*
	     clojure.core/*print-meta*
	     clojure.core/*print-readably*)

	    (("Unfiled") 

	     clojure.core/*agent*
	     clojure.core/*allow-unresolved-vars*
	     clojure.core/*assert*
	     clojure.core/*clojure-version*
	     clojure.core/*command-line-args*
	     clojure.core/*compile-files*
	     clojure.core/*compile-path*
	     clojure.core/*compiler-options*
	     clojure.core/*err*
	     clojure.core/*file*
	     clojure.core/*flush-on-newline*
	     clojure.core/*fn-loader*
	     clojure.core/*in*
	     clojure.core/*math-context*
	     clojure.core/*ns*
	     clojure.core/*out*
	     clojure.core/*read-eval*
	     clojure.core/*source-path*
	     clojure.core/*use-context-classloader*
	     clojure.core/*verbose-defrecords*
	     clojure.core/*warn-on-reflection*
	     clojure.core/->
	     clojure.core/->>
	     clojure.core/->ArrayChunk
	     clojure.core/->Vec
	     clojure.core/->VecNode
	     clojure.core/->VecSeq
	     clojure.core/-cache-protocol-fn
	     clojure.core/-reset-methods
	     clojure.core/..
	     clojure.core/EMPTY-NODE
	     clojure.core/accessor
	     clojure.core/aclone
	     clojure.core/add-classpath
	     clojure.core/add-watch
	     clojure.core/agent
	     clojure.core/agent-error
	     clojure.core/agent-errors
	     clojure.core/aget
	     clojure.core/alength
	     clojure.core/alias
	     clojure.core/all-ns
	     clojure.core/alter
	     clojure.core/alter-meta!
	     clojure.core/alter-var-root
	     clojure.core/amap
	     clojure.core/ancestors
	     clojure.core/and
	     clojure.core/apply
	     clojure.core/areduce
	     clojure.core/array-map
	     clojure.core/as->
	     clojure.core/aset
	     clojure.core/aset-boolean
	     clojure.core/aset-byte
	     clojure.core/aset-char
	     clojure.core/aset-double
	     clojure.core/aset-float
	     clojure.core/aset-int
	     clojure.core/aset-long
	     clojure.core/aset-short
	     clojure.core/assert
	     clojure.core/assoc!
	     clojure.core/assoc-in
	     clojure.core/atom
	     clojure.core/await
	     clojure.core/await-for
	     clojure.core/await1
	     clojure.core/bases
	     clojure.core/bean
	     clojure.core/binding
	     clojure.core/boolean
	     clojure.core/boolean-array
	     clojure.core/booleans
	     clojure.core/bound-fn
	     clojure.core/bound-fn*
	     clojure.core/bound?
	     clojure.core/butlast
	     clojure.core/byte-array
	     clojure.core/bytes
	     clojure.core/case
	     clojure.core/cast
	     clojure.core/char-array
	     clojure.core/chars
	     clojure.core/chunk
	     clojure.core/chunk-append
	     clojure.core/chunk-buffer
	     clojure.core/chunk-cons
	     clojure.core/chunk-first
	     clojure.core/chunk-next
	     clojure.core/chunk-rest
	     clojure.core/chunked-seq?
	     clojure.core/class
	     clojure.core/class?
	     clojure.core/clear-agent-errors
	     clojure.core/clojure-version
	     clojure.core/comment
	     clojure.core/commute
	     clojure.core/comp
	     clojure.core/comparator
	     clojure.core/compare-and-set!
	     clojure.core/compile
	     clojure.core/complement
	     clojure.core/concat
	     clojure.core/cond
	     clojure.core/cond->
	     clojure.core/cond->>
	     clojure.core/condp
	     clojure.core/conj!
	     clojure.core/constantly
	     clojure.core/construct-proxy
	     clojure.core/contains?
	     clojure.core/create-ns
	     clojure.core/create-struct
	     clojure.core/cycle
	     clojure.core/decimal?
	     clojure.core/declare
	     clojure.core/default-data-readers
	     clojure.core/definline
	     clojure.core/definterface
	     clojure.core/defmacro
	     clojure.core/defmethod
	     clojure.core/defmulti
	     clojure.core/defn
	     clojure.core/defn-
	     clojure.core/defonce
	     clojure.core/defprotocol
	     clojure.core/defrecord
	     clojure.core/defstruct
	     clojure.core/deftype
	     clojure.core/delay
	     clojure.core/delay?
	     clojure.core/deliver
	     clojure.core/denominator
	     clojure.core/deref
	     clojure.core/derive
	     clojure.core/descendants
	     clojure.core/destructure
	     clojure.core/disj
	     clojure.core/disj!
	     clojure.core/dissoc
	     clojure.core/dissoc!
	     clojure.core/distinct
	     clojure.core/doall
	     clojure.core/dorun
	     clojure.core/doseq
	     clojure.core/dosync
	     clojure.core/dotimes
	     clojure.core/doto
	     clojure.core/double-array
	     clojure.core/doubles
	     clojure.core/drop
	     clojure.core/drop-last
	     clojure.core/drop-while
	     clojure.core/ensure
	     clojure.core/enumeration-seq
	     clojure.core/error-handler
	     clojure.core/error-mode
	     clojure.core/eval
	     clojure.core/every-pred
	     clojure.core/ex-data
	     clojure.core/ex-info
	     clojure.core/extend
	     clojure.core/extend-protocol
	     clojure.core/extend-type
	     clojure.core/extenders
	     clojure.core/extends?
	     clojure.core/false?
	     clojure.core/ffirst
	     clojure.core/file-seq
	     clojure.core/filter
	     clojure.core/find
	     clojure.core/find-ns
	     clojure.core/find-protocol-impl
	     clojure.core/find-protocol-method
	     clojure.core/find-var
	     clojure.core/flatten
	     clojure.core/float-array
	     clojure.core/float?
	     clojure.core/floats
	     clojure.core/flush
	     clojure.core/fn
	     clojure.core/fn?
	     clojure.core/fnext
	     clojure.core/fnil
	     clojure.core/for
	     clojure.core/force
	     clojure.core/frequencies
	     clojure.core/future
	     clojure.core/future-call
	     clojure.core/future-cancel
	     clojure.core/future-cancelled?
	     clojure.core/future-done?
	     clojure.core/future?
	     clojure.core/gen-class
	     clojure.core/gen-interface
	     clojure.core/get-in
	     clojure.core/get-method
	     clojure.core/get-proxy-class
	     clojure.core/get-thread-bindings
	     clojure.core/get-validator
	     clojure.core/group-by
	     clojure.core/hash
	     clojure.core/hash-combine
	     clojure.core/hash-map
	     clojure.core/hash-set
	     clojure.core/identity
	     clojure.core/if-let
	     clojure.core/if-not
	     clojure.core/ifn?
	     clojure.core/import
	     clojure.core/in-ns
	     clojure.core/init-proxy
	     clojure.core/instance?
	     clojure.core/int-array
	     clojure.core/integer?
	     clojure.core/interleave
	     clojure.core/intern
	     clojure.core/interpose
	     clojure.core/into-array
	     clojure.core/ints
	     clojure.core/io!
	     clojure.core/isa?
	     clojure.core/iterate
	     clojure.core/iterator-seq
	     clojure.core/juxt
	     clojure.core/keep
	     clojure.core/keep-indexed
	     clojure.core/key
	     clojure.core/keys
	     clojure.core/last
	     clojure.core/lazy-cat
	     clojure.core/lazy-seq
	     clojure.core/let
	     clojure.core/letfn
	     clojure.core/line-seq
	     clojure.core/load
	     clojure.core/load-file
	     clojure.core/load-reader
	     clojure.core/load-string
	     clojure.core/loaded-libs
	     clojure.core/locking
	     clojure.core/long-array
	     clojure.core/longs
	     clojure.core/loop
	     clojure.core/macroexpand
	     clojure.core/macroexpand-1
	     clojure.core/make-array
	     clojure.core/make-hierarchy
	     clojure.core/map
	     clojure.core/map-indexed
	     clojure.core/mapcat
	     clojure.core/max-key
	     clojure.core/memfn
	     clojure.core/memoize
	     clojure.core/merge
	     clojure.core/merge-with
	     clojure.core/meta 
	     clojure.core/method-sig
	     clojure.core/methods
	     clojure.core/min-key
	     clojure.core/munge
	     clojure.core/name
	     clojure.core/namespace
	     clojure.core/namespace-munge
	     clojure.core/newline
	     clojure.core/next
	     clojure.core/nfirst
	     clojure.core/nnext
	     clojure.core/not
	     clojure.core/ns
	     clojure.core/ns-aliases
	     clojure.core/ns-imports
	     clojure.core/ns-interns
	     clojure.core/ns-map
	     clojure.core/ns-name
	     clojure.core/ns-publics
	     clojure.core/ns-refers
	     clojure.core/ns-resolve
	     clojure.core/ns-unalias
	     clojure.core/ns-unmap
	     clojure.core/nthnext
	     clojure.core/nthrest
	     clojure.core/number?
	     clojure.core/numerator
	     clojure.core/object-array
	     clojure.core/or
	     clojure.core/parents
	     clojure.core/partial
	     clojure.core/partition
	     clojure.core/partition-all
	     clojure.core/partition-by
	     clojure.core/pcalls
	     clojure.core/persistent!
	     clojure.core/pmap
	     clojure.core/pop!
	     clojure.core/pop-thread-bindings
	     clojure.core/pr
	     clojure.core/pr-str
	     clojure.core/prefer-method
	     clojure.core/prefers
	     clojure.core/primitives-classnames
	     clojure.core/print
	     clojure.core/print-ctor
	     clojure.core/print-dup
	     clojure.core/print-method
	     clojure.core/print-simple
	     clojure.core/print-str
	     clojure.core/printf
	     clojure.core/println
	     clojure.core/println-str
	     clojure.core/prn
	     clojure.core/prn-str
	     clojure.core/promise
	     clojure.core/proxy
	     clojure.core/proxy-call-with-super
	     clojure.core/proxy-mappings
	     clojure.core/proxy-name
	     clojure.core/proxy-super
	     clojure.core/push-thread-bindings
	     clojure.core/pvalues
	     clojure.core/rand-nth
	     clojure.core/range
	     clojure.core/ratio?
	     clojure.core/rational?
	     clojure.core/read
	     clojure.core/read-line
	     clojure.core/read-string
	     clojure.core/realized?
	     clojure.core/reduce
	     clojure.core/reduced
	     clojure.core/reduced?
	     clojure.core/reductions
	     clojure.core/ref
	     clojure.core/ref-history-count
	     clojure.core/ref-max-history
	     clojure.core/ref-min-history
	     clojure.core/ref-set
	     clojure.core/refer
	     clojure.core/refer-clojure
	     clojure.core/reify
	     clojure.core/release-pending-sends
	     clojure.core/remove
	     clojure.core/remove-all-methods
	     clojure.core/remove-method
	     clojure.core/remove-ns
	     clojure.core/remove-watch
	     clojure.core/repeat
	     clojure.core/repeatedly
	     clojure.core/replicate
	     clojure.core/require
	     clojure.core/reset!
	     clojure.core/reset-meta!
	     clojure.core/resolve
	     clojure.core/rest
	     clojure.core/restart-agent
	     clojure.core/resultset-seq
	     clojure.core/reverse
	     clojure.core/rseq
	     clojure.core/rsubseq
	     clojure.core/satisfies?
	     clojure.core/second
	     clojure.core/select-keys
	     clojure.core/send
	     clojure.core/send-off
	     clojure.core/send-via
	     clojure.core/seq
	     clojure.core/seque
	     clojure.core/sequence
	     clojure.core/set
	     clojure.core/set-agent-send-executor!
	     clojure.core/set-agent-send-off-executor!
	     clojure.core/set-error-handler!
	     clojure.core/set-error-mode!
	     clojure.core/set-validator!
	     clojure.core/short-array
	     clojure.core/shorts
	     clojure.core/shuffle
	     clojure.core/shutdown-agents
	     clojure.core/slurp
	     clojure.core/some->
	     clojure.core/some->>
	     clojure.core/some-fn
	     clojure.core/sort
	     clojure.core/sort-by
	     clojure.core/sorted-map
	     clojure.core/sorted-map-by
	     clojure.core/sorted-set
	     clojure.core/sorted-set-by
	     clojure.core/special-symbol?
	     clojure.core/spit
	     clojure.core/split-at
	     clojure.core/split-with
	     clojure.core/struct
	     clojure.core/struct-map
	     clojure.core/subseq
	     clojure.core/supers
	     clojure.core/swap!
	     clojure.core/sync
	     clojure.core/take
	     clojure.core/take-last
	     clojure.core/take-nth
	     clojure.core/take-while
	     clojure.core/test
	     clojure.core/the-ns
	     clojure.core/thread-bound?
	     clojure.core/time
	     clojure.core/to-array
	     clojure.core/to-array-2d
	     clojure.core/trampoline
	     clojure.core/transient
	     clojure.core/tree-seq
	     clojure.core/true?
	     clojure.core/type
	     clojure.core/underive
	     clojure.core/unquote
	     clojure.core/unquote-splicing
	     clojure.core/update-in
	     clojure.core/update-proxy
	     clojure.core/use
	     clojure.core/val
	     clojure.core/vals
	     clojure.core/var-get
	     clojure.core/var-set
	     clojure.core/var?
	     clojure.core/vary-meta
	     clojure.core/vec
	     clojure.core/vector
	     clojure.core/vector-of
	     clojure.core/when
	     clojure.core/when-first
	     clojure.core/when-let
	     clojure.core/when-not
	     clojure.core/while
	     clojure.core/with-bindings
	     clojure.core/with-bindings*
	     clojure.core/with-in-str
	     clojure.core/with-loading-context
	     clojure.core/with-local-vars
	     clojure.core/with-meta
	     clojure.core/with-open
	     clojure.core/with-out-str
	     clojure.core/with-redefs
	     clojure.core/with-redefs-fn
	     clojure.core/xml-seq
	     clojure.core/zipmap
	     clojure.walk/keywordize-keys
	     clojure.walk/macroexpand-all
	     clojure.walk/stringify-keys

	     ))
))

;;;###autoload
(defun helm-clojure-cheatsheet ()
  "Use helm to show a Clojure cheatsheet."
  (interactive)
  (helm :sources helm-source-clojure-cheatsheet))

(provide 'helm-clojure-cheatsheet)

;;; helm-clojure-cheatsheet.el ends here

(evil-define-key 'normal emacs-lisp-mode-map
  "H" 'helm-clojure-cheatsheet)
