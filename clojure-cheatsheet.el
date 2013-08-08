;;; -*- lexical-binding: t -*-
;;; clojure-cheatsheet.el --- A helm version of the Clojure cheatsheet.
;; Copyright 2013 Kris Jenkins

;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: clojure nrepl cheatsheet helm
;; URL: https://github.com/krisajenkins/clojure-cheatsheet
;; Created: 7th August 2013
;; Version: 0.1.0
;; Package-Requires: ((helm "1.5.3") (nrepl "0.1.8"))

;;; Commentary: 

(require 'helm)
(require 'helm-match-plugin)
(require 'nrepl)

;;; Code:

(defun clojure-cheatsheet/flatten
  (node)
  (cond
   ((not (listp node)) node)
   ((listp (car node)) (apply 'append (mapcar 'clojure-cheatsheet/flatten node)))
   (t (list (mapcar 'clojure-cheatsheet/flatten node)))))

(defun clojure-cheatsheet/symbol-qualifier
  (namespace)
  (lambda (symbol)
    (intern (format "%s/%s" namespace symbol))))

(defun clojure-cheatsheet/handle-subnode
  (head)
  (lambda (subnode)
    (cond
     ((symbolp (car subnode)) (cons head subnode))
     ((stringp (car subnode)) (cons (format "%s : %s" head (car subnode))
				    (cdr subnode)))
     (t (mapcar (clojure-cheatsheet/handle-subnode head) subnode)))))

(defun clojure-cheatsheet/propagate-headings
  (node)
  (if (not (listp node))
      node
    (let* ((postwalk (mapcar 'clojure-cheatsheet/propagate-headings node))
	   (head (car postwalk))
	   (tail (cdr postwalk)))
      (cond
       ((symbolp head) (mapcar (clojure-cheatsheet/symbol-qualifier head) tail))
       ((stringp head) (mapcar (clojure-cheatsheet/handle-subnode head) tail))
       ((listp head) postwalk)
       (t (error "Unhandled case!"))))))

(defun clojure-cheatsheet/group-by-head
  (coll)
  (let ((result (make-hash-table :test 'equal)))
    (loop for item in coll
	  collect
	  (progn
	    (let* ((head (car item))
		   (tail (cdr item))
		   (current (gethash head result))
		   (new (if current
			    (append current tail)
			  tail)))
	      (puthash head new result))))
    result))

(defun clojure-cheatsheet-join
  (separator &rest items)
  (mapconcat 'identity items separator))

(defun clojure-cheatsheet-lookup-doc
  (symbol)
  (if (nrepl-current-connection-buffer)
      (nrepl-doc-handler symbol)
    (error "nREPL not connected!")))

(defun clojure-cheatsheet-lookup-src
  (symbol)
  (if (nrepl-current-connection-buffer)
      (nrepl-src-handler symbol)
    (error "nREPL not connected!")))

(defun clojure-cheatsheet/hash-to-helm-source
  (ahash)
  (let ((result '()))
    (maphash (lambda (heading symbols)
	       (setq result
		     (cons
		      `((name . ,heading)
			(candidates ,@symbols)
			(match . ((lambda (candidate)
				    (helm-mp-3-match (clojure-cheatsheet-join " " candidate ,heading)))))
			(action . (("Lookup Docs" . clojure-cheatsheet-lookup-doc)
				   ("Lookup Source" . clojure-cheatsheet-lookup-src))))
		      result)))
	     ahash)
    result))

(defvar helm-source-clojure-cheatsheet
  (clojure-cheatsheet/hash-to-helm-source
   (clojure-cheatsheet/group-by-head
    (clojure-cheatsheet/flatten
     (clojure-cheatsheet/propagate-headings
      '(("Primitives"
	 ("Numbers"
	  ("Arithmetic"
	   (clojure.core + - * / quot rem mod dec inc max min))
	  ("Compare"
	   (clojure.core = == not= < > <= >= compare)) 
	  ("Bitwise"
	   (clojure.core bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set bit-shift-left bit-shift-right bit-test bit-xor))
	  ("Cast"
	   (clojure.core byte short long int float double bigdec bigint biginteger num rationalize))
	  ("Test"
	   (clojure.core nil? identical? zero? pos? neg? even? odd?))
	  ("Random" 
	   (clojure.core rand rand-int))
	  ("BigDecimal"
	   (clojure.core with-precision))
	  ("Unchecked"
	   (clojure.core *unchecked-math*
			 unchecked-add
			 unchecked-add-int
			 unchecked-byte
			 unchecked-char
			 unchecked-dec
			 unchecked-dec-int
			 unchecked-divide-int
			 unchecked-double
			 unchecked-float
			 unchecked-inc
			 unchecked-inc-int
			 unchecked-int
			 unchecked-long
			 unchecked-multiply
			 unchecked-multiply-int
			 unchecked-negate
			 unchecked-negate-int
			 unchecked-remainder-int
			 unchecked-short
			 unchecked-subtract
			 unchecked-subtract-int)))

	 ("Strings"
	  ("Create"
	   (clojure.core str format))
	  ("Use"
	   (clojure.core count get subs compare)
	   (clojure.string join escape split split-lines replace replace-first reverse))
	  ("Regex"
	   (clojure.core re-find re-pattern)
	   (clojure.string replace replace-first))
	  ("Letters"
	   (clojure.string capitalize lower-case upper-case))
	  ("Trim"
	   (clojure.string trim trim-newline triml trimr))
	  ("Test"
	   (clojure.core char char? string?)
	   (clojure.string blank?)))

	 ("Other"
	  ("Characters"
	   (clojure.core char char-name-string char-escape-string))
	  ("Keywords"
	   (clojure.core keyword keyword? find-keyword))
	  ("Symbols"
	   (clojure.core symbol symbol? gensym))
	  ("Data Readers"
	   (clojure.core *data-readers* *default-data-reader-fn*))))

	("Collections"
	 ("Generic Ops"
	  (clojure.core count empty not-empty into clojure.core conj))
	 ("Walking" 
	  (clojure.walk walk prewalk prewalk-demo prewalk-replace postwalk postwalk-demo postwalk-replace))
	 ("Content tests"
	  (clojure.core distinct? empty? every? not-every? some not-any?))
	 ("Capabilities"
	  (clojure.core/sequential? associative? sorted? counted? reversible?))
	 ("Type tests"
	  (clojure.core coll? list? vector? set? map? seq?))

	 ("Lists"
	  ("Create"
	   (clojure.core list list*))
	  ("Examine"
	   (clojure.core/first nth peek))
	  ("'Change'"
	   (clojure.core cons conj rest pop)))	    

	 ("Vectors"
	  ("Create"
	   (clojure.core vec vector vector-of))
	  ("Examine"
	   (clojure.core get peek))
	  
	  ("'Change'"
	   (clojure.core assoc pop subvec replace conj rseq))
	  ("Ops"
	   (clojure.core mapv filterv reduce-kv)
	   ))
	 ("Sets"
	  ("Create"
	   (clojure.core set hash-set sorted-set sorted-set-by))
	  ("Examine"
	   (clojure.core get contains?))
	  ("'Change'"
	   (clojure.core conj disj))
	  ("Relational Algebra"
	   (clojure.set join select project union difference intersection))
	  ("Get map"
	   (clojure.set index rename-keys rename map-invert))
	  ("Test"
	   (clojure.set subset? superset?)))

	 ("Maps"
	  ("Create"
	   (clojure.core hash-map array-map zipmap sorted-map sorted-map-by bean frequencies group-by))
	  ("Examine" 
	   (clojure.core get get-in contains? find keys vals))
	  ("'Change'" 
	   (clojure.core assoc assoc-in dissoc merge merge-with select-keys update-in))
	  ("Entry" 
	   (clojure.core key val))
	  ("Sorted Maps" 
	   (clojure.core rseq subseq rsubseq)
	   )))

	("Functions"
	 ("Create"
	  (clojure.core fn defn defn- definline identity constantly comp complement partial juxt memfn memoize fnil every-pred some-fn))
	 ("Call"
	  (clojure.core -> ->> some-> some->> as-> cond-> cond->>))
	 ("Test"
	  (clojure.core fn? ifn?)))

	("Other"
	 ("XML"
	  (clojure.core xml-seq)
	  (clojure.xml parse))
	 ("REPL"
	  (clojure.core *1 *2 *3 *e *print-dup* *print-length* *print-level* *print-meta* *print-readably*)))
	("Namespace"
	 ("Current"
	  (clojure.core *ns*))
	 ("Create Switch"
	  (clojure.core ns in-ns create-ns))
	 ("Add"
	  (clojure.core alias import intern refer refer-clojure))
	 ("Find"
	  (clojure.core all-ns find-ns))
	 ("Examine"
	  (clojure.core ns-aliases ns-imports ns-interns ns-map ns-name ns-publics ns-refers))
	 ("From symbol"
	  (clojure.core resolve namespace ns-resolve))
	 ("Remove"
	  (clojure.core ns-unalias ns-unmap remove-ns)))
	("Loading"
	 ("Load libs"
	  (clojure.core require use import refer))
	 ("List Loaded"
	  (clojure.core loaded-libs))
	 ("Load Misc"
	  (clojure.core load load-file load-reader load-string)))

	("Concurrency"
	 ("Atoms"
	  (clojure.core/atom swap! reset! compare-and-set!))
	 ("Futures"
	  (clojure.core future future-call future-cancel future-cancelled? future-done? future?))
	 ("Threads"
	  (clojure.core bound-fn bound-fn* get-thread-bindings pop-thread-bindings push-thread-bindings))

	 ("Misc"
	  (clojure.core locking pcalls pvalues pmap seque promise deliver))

	 ("Refs & Transactions"
	  ("Create"
	   (clojure.core ref))
	  ("Examine"
	   (clojure.core deref))
	  ("Transaction"
	   (clojure.core sync dosync io!))
	  ("In Transaction"
	   (clojure.core ensure ref-set alter commute))
	  ("Validators"
	   (clojure.core get-validator set-validator!))
	  ("History"
	   (clojure.core ref-history-count ref-max-history ref-min-history)))

	 ("Agents & Asynchronous Actions"
	  ("Create"
	   (clojure.core agent))
	  ("Examine"
	   (clojure.core agent-error))
	  ("Change State"
	   (clojure.core send send-off restart-agent))
	  ("Block Waiting"
	   (clojure.core await await-for))
	  ("Ref Validators"
	   (clojure.core get-validator set-validator!))
	  ("Watchers"
	   (clojure.core add-watch remove-watch))
	  ("Thread Handling"
	   (clojure.core shutdown-agents))
	  ("Error"
	   (clojure.core error-handler set-error-handler! error-mode set-error-mode!))
	  ("Misc"
	   (clojure.core *agent* release-pending-sends))))

	("Sequences"
	 ("Creating a Lazy Seq"
	  ("From Collection"
	   (clojure.core seq keys vals rseq subseq rsubseq))
	  ("From Producer Fn"
	   (clojure.core lazy-seq repeatedly iterate))
	  ("From Constant"
	   (clojure.core repeat range))
	  ("From Other"
	   (clojure.core file-seq line-seq resultset-seq re-seq tree-seq xml-seq iterator-seq enumeration-seq))
	  ("From Seq"
	   (clojure.core keep keep-indexed))))

	

	))))))


;; (("Unfiled") 

;;  clojure.core/*allow-unresolved-vars*
;;  clojure.core/*assert*
;;  clojure.core/*clojure-version*
;;  clojure.core/*command-line-args*
;;  clojure.core/*compile-files*
;;  clojure.core/*compile-path*
;;  clojure.core/*compiler-options*
;;  clojure.core/*err*
;;  clojure.core/*file*
;;  clojure.core/*flush-on-newline*
;;  clojure.core/*fn-loader*
;;  clojure.core/*in*
;;  clojure.core/*math-context*
;;  clojure.core/*out*
;;  clojure.core/*read-eval*
;;  clojure.core/*source-path*
;;  clojure.core/*use-context-classloader*
;;  clojure.core/*verbose-defrecords*
;;  clojure.core/*warn-on-reflection*
;;  clojure.core/->ArrayChunk
;;  clojure.core/->Vec
;;  clojure.core/->VecNode
;;  clojure.core/->VecSeq
;;  clojure.core/-cache-protocol-fn
;;  clojure.core/-reset-methods
;;  clojure.core/..
;;  clojure.core/EMPTY-NODE
;;  clojure.core/accessor
;;  clojure.core/aclone
;;  clojure.core/add-classpath
;;  clojure.core/agent-errors
;;  clojure.core/aget
;;  clojure.core/alength
;;  clojure.core/alter-meta!
;;  clojure.core/alter-var-root
;;  clojure.core/amap
;;  clojure.core/ancestors
;;  clojure.core/and
;;  clojure.core/apply
;;  clojure.core/areduce
;;  clojure.core/aset
;;  clojure.core/aset-boolean
;;  clojure.core/aset-byte
;;  clojure.core/aset-char
;;  clojure.core/aset-double
;;  clojure.core/aset-float
;;  clojure.core/aset-int
;;  clojure.core/aset-long
;;  clojure.core/aset-short
;;  clojure.core/assert
;;  clojure.core/assoc!
;;  clojure.core/await1
;;  clojure.core/bases
;;  clojure.core/binding
;;  clojure.core/boolean
;;  clojure.core/boolean-array
;;  clojure.core/booleans
;;  clojure.core/bound?
;;  clojure.core/butlast
;;  clojure.core/byte-array
;;  clojure.core/bytes
;;  clojure.core/case
;;  clojure.core/cast
;;  clojure.core/char-array
;;  clojure.core/chars
;;  clojure.core/chunk
;;  clojure.core/chunk-append
;;  clojure.core/chunk-buffer
;;  clojure.core/chunk-cons
;;  clojure.core/chunk-first
;;  clojure.core/chunk-next
;;  clojure.core/chunk-rest
;;  clojure.core/chunked-seq?
;;  clojure.core/class
;;  clojure.core/class?
;;  clojure.core/clear-agent-errors
;;  clojure.core/clojure-version
;;  clojure.core/comment
;;  clojure.core/comparator
;;  clojure.core/compile
;;  clojure.core/concat
;;  clojure.core/cond
;;  clojure.core/condp
;;  clojure.core/conj!
;;  clojure.core/construct-proxy
;;  clojure.core/create-struct
;;  clojure.core/cycle
;;  clojure.core/decimal?
;;  clojure.core/declare
;;  clojure.core/default-data-readers
;;  clojure.core/definterface
;;  clojure.core/defmacro
;;  clojure.core/defmethod
;;  clojure.core/defmulti
;;  clojure.core/defonce
;;  clojure.core/defprotocol
;;  clojure.core/defrecord
;;  clojure.core/defstruct
;;  clojure.core/deftype
;;  clojure.core/delay
;;  clojure.core/delay?
;;  clojure.core/denominator
;;  clojure.core/derive
;;  clojure.core/descendants
;;  clojure.core/destructure
;;  clojure.core/disj
;;  clojure.core/disj!
;;  clojure.core/dissoc
;;  clojure.core/dissoc!
;;  clojure.core/distinct
;;  clojure.core/doall
;;  clojure.core/dorun
;;  clojure.core/doseq
;;  clojure.core/dotimes
;;  clojure.core/doto
;;  clojure.core/double-array
;;  clojure.core/doubles
;;  clojure.core/drop
;;  clojure.core/drop-last
;;  clojure.core/drop-while
;;  clojure.core/eval
;;  clojure.core/ex-data
;;  clojure.core/ex-info
;;  clojure.core/extend
;;  clojure.core/extend-protocol
;;  clojure.core/extend-type
;;  clojure.core/extenders
;;  clojure.core/extends?
;;  clojure.core/false?
;;  clojure.core/ffirst
;;  clojure.core/filter
;;  clojure.core/find-protocol-impl
;;  clojure.core/find-protocol-method
;;  clojure.core/find-var
;;  clojure.core/flatten
;;  clojure.core/float-array
;;  clojure.core/float?
;;  clojure.core/floats
;;  clojure.core/flush
;;  clojure.core/fnext
;;  clojure.core/for
;;  clojure.core/force
;;  clojure.core/gen-class
;;  clojure.core/gen-interface
;;  clojure.core/get-method
;;  clojure.core/get-proxy-class
;;  clojure.core/hash
;;  clojure.core/hash-combine
;;  clojure.core/if-let
;;  clojure.core/if-not
;;  clojure.core/init-proxy
;;  clojure.core/instance?
;;  clojure.core/int-array
;;  clojure.core/integer?
;;  clojure.core/interleave
;;  clojure.core/interpose
;;  clojure.core/into-array
;;  clojure.core/ints
;;  clojure.core/isa?
;;  clojure.core/last
;;  clojure.core/lazy-cat
;;  clojure.core/let
;;  clojure.core/letfn
;;  clojure.core/long-array
;;  clojure.core/longs
;;  clojure.core/loop
;;  clojure.core/macroexpand
;;  clojure.core/macroexpand-1
;;  clojure.core/make-array
;;  clojure.core/make-hierarchy
;;  clojure.core/map
;;  clojure.core/map-indexed
;;  clojure.core/mapcat
;;  clojure.core/max-key
;;  clojure.core/meta 
;;  clojure.core/method-sig
;;  clojure.core/methods
;;  clojure.core/min-key
;;  clojure.core/munge
;;  clojure.core/name
;;  clojure.core/namespace-munge
;;  clojure.core/newline
;;  clojure.core/next
;;  clojure.core/nfirst
;;  clojure.core/nnext
;;  clojure.core/not
;;  clojure.core/nthnext
;;  clojure.core/nthrest
;;  clojure.core/number?
;;  clojure.core/numerator
;;  clojure.core/object-array
;;  clojure.core/or
;;  clojure.core/parents
;;  clojure.core/partition
;;  clojure.core/partition-all
;;  clojure.core/partition-by
;;  clojure.core/persistent!
;;  clojure.core/pop!
;;  clojure.core/pr
;;  clojure.core/pr-str
;;  clojure.core/prefer-method
;;  clojure.core/prefers
;;  clojure.core/primitives-classnames
;;  clojure.core/print
;;  clojure.core/print-ctor
;;  clojure.core/print-dup
;;  clojure.core/print-method
;;  clojure.core/print-simple
;;  clojure.core/print-str
;;  clojure.core/printf
;;  clojure.core/println
;;  clojure.core/println-str
;;  clojure.core/prn
;;  clojure.core/prn-str
;;  clojure.core/proxy
;;  clojure.core/proxy-call-with-super
;;  clojure.core/proxy-mappings
;;  clojure.core/proxy-name
;;  clojure.core/proxy-super
;;  clojure.core/rand-nth
;;  clojure.core/ratio?
;;  clojure.core/rational?
;;  clojure.core/read
;;  clojure.core/read-line
;;  clojure.core/read-string
;;  clojure.core/realized?
;;  clojure.core/reduce
;;  clojure.core/reduced
;;  clojure.core/reduced?
;;  clojure.core/reductions
;;  clojure.core/reify
;;  clojure.core/remove
;;  clojure.core/remove-
;;  clojure.core/remove-method
;;  clojure.core/replicate
;;  clojure.core/reset-meta!
;;  clojure.core/rest
;;  clojure.core/reverse
;;  clojure.core/satisfies?
;;  clojure.core/second
;;  clojure.core/send-via
;;  clojure.core/sequence
;;  clojure.core/set-agent-send-executor!
;;  clojure.core/set-agent-send-off-executor!
;;  clojure.core/short-array
;;  clojure.core/shorts
;;  clojure.core/shuffle
;;  clojure.core/slurp
;;  clojure.core/sort
;;  clojure.core/sort-by
;;  clojure.core/special-symbol?
;;  clojure.core/spit
;;  clojure.core/split-at
;;  clojure.core/split-with
;;  clojure.core/struct
;;  clojure.core/struct-map
;;  clojure.core/supers
;;  clojure.core/take
;;  clojure.core/take-last
;;  clojure.core/take-nth
;;  clojure.core/take-while
;;  clojure.core/test
;;  clojure.core/the-ns
;;  clojure.core/thread-bound?
;;  clojure.core/time
;;  clojure.core/to-array
;;  clojure.core/to-array-2d
;;  clojure.core/trampoline
;;  clojure.core/transient
;;  clojure.core/true?
;;  clojure.core/type
;;  clojure.core/underive
;;  clojure.core/unquote
;;  clojure.core/unquote-splicing
;;  clojure.core/update-proxy
;;  clojure.core/var-get
;;  clojure.core/var-set
;;  clojure.core/var?
;;  clojure.core/vary-meta
;;  clojure.core/vec
;;  clojure.core/vector
;;  clojure.core/vector-of
;;  clojure.core/when
;;  clojure.core/when-first
;;  clojure.core/when-let
;;  clojure.core/when-not
;;  clojure.core/while
;;  clojure.core/with-bindings
;;  clojure.core/with-bindings*
;;  clojure.core/with-in-str
;;  clojure.core/with-loading-context
;;  clojure.core/with-local-vars
;;  clojure.core/with-meta
;;  clojure.core/with-open
;;  clojure.core/with-out-str
;;  clojure.core/with-redefs
;;  clojure.core/with-redefs-fn
;;  clojure.walk/keywordize-keys
;;  clojure.walk/macroexpand-all
;;  clojure.walk/stringify-keys)

;;;###autoload
(defun clojure-cheatsheet ()
  "Use helm to show a Clojure cheatsheet."
  (interactive)
  (helm :sources helm-source-clojure-cheatsheet))

(provide 'clojure-cheatsheet)

;;; clojure-cheatsheet.el ends here
