;;; clojure-cheatsheet.el --- The Clojure Cheatsheet in Emacs
;; Copyright 2013 Kris Jenkins

;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: clojure nrepl cheatsheet helm
;; URL: https://github.com/krisajenkins/clojure-cheatsheet
;; Created: 7th August 2013
;; Version: 0.1.0
;; Package-Requires: ((helm "1.5.3") (nrepl "0.1.8"))

;;; Commentary:
;;
;; The Clojure Cheatsheet in Emacs.

(require 'helm)
(require 'helm-match-plugin)
(require 'nrepl)

;;; Code

(defvar clojure-cheatsheet-hierarchy
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
      (clojure.core sequential? associative? sorted? counted? reversible?))
     ("Type tests"
      (clojure.core coll? list? vector? set? map? seq?))

     ("Lists"
      ("Create"
       (clojure.core list list*))
      ("Examine"
       (clojure.core first nth peek))
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
      (clojure.core *1 *2 *3 *e *print-dup* *print-length* *print-level* *print-meta* *print-readably*))
     ("Code"
      (clojure.core *compile-files* *compile-path* *file* *warn-on-reflection* compile gen-class gen-interface loaded-libs test))
     ("Misc"
      (clojure.core eval force hash name *clojure-version* clojure-version *command-line-args*))
     ("Browser / Shell"
      (clojure.java.browse browse-url)
      (clojure.java.shell  sh with-sh-dir with-sh-env)))

    ("Vars & Global Environment"
     ("Def Variants"
      (clojure.core def defn defn- definline defmacro defmethod defmulti defonce defrecord))
     ("Interned Vars"
      (clojure.core declare intern binding find-var var))
     ("Var Objects"
      (clojure.core with-local-vars var-get var-set alter-var-root var?))
     ("Var Validators"
      (clojure.core set-validator! get-validator)))

    ("Abstractions"
     ("Protocols"
      (clojure.core defprotocol extend-type reify))
     ("Records"
      (clojure.core defrecord))
     ("Types"
      (clojure.core deftype))
     ("Multimethods"
      ("Define"
       (clojure.core defmulti defmethod))
      ("Dispatch"
       (clojure.core get-method methods))
      ("Remove"
       (clojure.core remove-method remove-all-methods))
      ("Prefer"
       (clojure.core prefer-method prefers))
      ("Relation"
       (clojure.core derive isa? parents ancestors descendants make-hierarchy))))

    ("Macros"
     ("Create"
      (clojure.core defmacro definline))
     ("Debug"
      (clojure.coremacroexpand-1 macroexpand)
      (clojure.walk macroexpand-all))
     ("Branch"
      (clojure.core and or when when-not when-let when-first if-not if-let cond condp case))
     ("Loop"
      (clojure.core for doseq dotimes while))
     ("Arrange"
      (clojure.core .. doto ->))
     ("Scope"
      (clojure.core binding locking time)
      (clojure.core with-in-str with-local-vars with-open with-out-str with-precision with-redefs with-redefs-fn))
     ("Lazy"
      (clojure.core lazy-cat lazy-seq delay))
     ("Doc."
      (clojure.core assert comment doc)))

    ("Java Interop"
     ("General"
      (clojure.core .. doto new bean comparator enumeration-seq import iterator-seq memfn set!))
     ("Cast"
      (clojure.core boolean byte short char int long float double bigdec bigint num cast biginteger))
     ("Exceptions"
      (clojure.core throw try catch finally pst ex-info ex-data)))

    
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
      (clojure.core atom swap! reset! compare-and-set!))
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
       (clojure.core keep keep-indexed)))

     ("Seq in, Seq out"
      ("Get shorter"
       (clojure.core distinct filter remove for))
      ("Get longer"
       (clojure.core cons conj concat lazy-cat mapcat cycle interleave interpose)))
     ("Tail-items"
      (clojure.core rest nthrest fnext nnext drop drop-while take-last for))
     ("Head-items"
      (clojure.core take take-nth take-while butlast drop-last for))
     ("'Change'"
      (clojure.core conj concat distinct flatten group-by partition partition-all partition-by split-at split-with filter remove replace shuffle))
     ("Rearrange"
      (clojure.core reverse sort sort-by compare))
     ("Process items"
      (clojure.core map pmap map-indexed mapcat for replace seque))

     ("Using a Seq"
      ("Extract item"
       (clojure.core first second last rest next ffirst nfirst fnext nnext nth nthnext rand-nth when-first max-key min-key))
      ("Construct coll"
       (clojure.core zipmap into reduce reductions set vec into-array to-array-2d))
      ("Pass to fn"
       (clojure.core apply))
      ("Search"
       (clojure.core some filter))
      ("Force evaluation"
       (clojure.core doseq dorun doall))
      ("Check for forced"
       (clojure.core realized?))))

    ("Zippers"
     ("Create"
      (clojure.zip zipper seq-zip vector-zip xml-zip))
     ("Get loc"
      (clojure.zip up down left right leftmost rightmost))
     ("Get seq"
      (clojure.zip lefts rights path children))
     ("'Change'"
      (clojure.zip make-node replace edit insert-child insert-left insert-right append-child remove))
     ("Move"
      (clojure.zip next prev))
     ("Misc"
      (clojure.zip root node branch? end?)))

    ("Documentation"
     ("REPL"
      (clojure.repl doc find-doc apropos source pst javadoc )))

    ("Transients"
     ("Create")
     (clojure.core transient persistent!)
     ("Change")
     (clojure.core conj! pop! assoc! dissoc! disj!))
    ("Misc"
     ("Compare"
      (clojure.core = == identical? not= not compare)
      (clojure.data diff))
     ("Test"
      (clojure.core true? false? nil? instance?)))

    ("IO"
     ("To/from ..."
      (clojure.core spit slurp))
     ("To *out*"
      (clojure.core pr prn print printf println newline)
      (clojure.pprint print-table))
     ("To writer"
      (clojure.pprint pprint cl-format))
     ("To string"
      (clojure.core format with-out-str pr-str prn-str print-str println-str))
     ("From *in*"
      (clojure.core read-line read))
     ("From reader"
      (clojure.core line-seq read))
     ("From string"
      (clojure.core read-string with-in-str))
     ("Open"
      (clojure.core with-open)
      (clojure.java.io  reader writer input-stream output-stream))
     ("Misc"
      (clojure.core flush file-seq *in* *out* *err*)
      (clojure.java.io file copy delete-file resource as-file as-url as-relative-path)))

    ("Special Forms"
     (clojure.core def if do let quote var fn loop recur throw try monitor-enter monitor-exit)
     ("Binding / Destructuring"
      (clojure.core let fn defn defmacro loop for doseq if-let when-let)))
    ))

(defun clojure-cheatsheet/flatten
  (node)
  (cond
   ((not (listp node)) node)
   ((listp (car node)) (apply 'append (mapcar 'clojure-cheatsheet/flatten node)))
   (t (list (mapcar 'clojure-cheatsheet/flatten node)))))

(defun clojure-cheatsheet/symbol-qualifier
  (namespace symbol)
  "Given a (Clojure) namespace and a symbol, fully-qualify that symbol."
  (intern (format "%s/%s" namespace symbol)))

(defun clojure-cheatsheet/handle-subnode
  (head subnode)
  (cond
   ((symbolp (car subnode)) (cons head subnode))
   ((stringp (car subnode)) (cons (format "%s : %s" head (car subnode))
				  (cdr subnode)))
   (t (mapcar (apply-partially 'clojure-cheatsheet/handle-subnode head) subnode))))

(defun clojure-cheatsheet/propagate-headings
  (node)
  (if (not (listp node))
      node
    (let* ((postwalk (mapcar 'clojure-cheatsheet/propagate-headings node))
	   (head (car postwalk))
	   (tail (cdr postwalk)))
      (cond
       ((symbolp head) (mapcar (apply-partially 'clojure-cheatsheet/symbol-qualifier head) tail))
       ((stringp head) (mapcar (apply-partially 'clojure-cheatsheet/handle-subnode head) tail))
       ((listp head) postwalk)
       (t (error "Unhandled case!"))))))

(defun clojure-cheatsheet/group-by-head
  (data)
  (let ((result '()))
    (mapcar (lambda (item)
  	      (let* ((head (car item))
  		     (tail (cdr item))
  		     (current (cdr (assoc head result))))
  		(if current
  		    (setf (cdr (assoc head result))
			  (append current tail))
  		  (setq result (append result (list item))))))
	    data)
    result))

(defun clojure-cheatsheet/lookup-doc
  (symbol)
  (if (nrepl-current-connection-buffer)
      (nrepl-doc-handler symbol)
    (error "nREPL not connected!")))

(defun clojure-cheatsheet/lookup-src
  (symbol)
  (if (nrepl-current-connection-buffer)
      (nrepl-src-handler symbol)
    (error "nREPL not connected!")))

(defun clojure-cheatsheet/item-to-helm-source
  (item)
  (let ((heading (car item))
	(symbols (cdr item)))
    `((name . ,heading)
      (candidates ,@symbols)
      (match . ((lambda (candidate)
		  (helm-mp-3-match (format "%s %s" candidate ,heading)))))
      (action . (("Lookup Docs" . clojure-cheatsheet/lookup-doc)
		 ("Lookup Source" . clojure-cheatsheet/lookup-src))))))

(defvar helm-source-clojure-cheatsheet
 (mapcar 'clojure-cheatsheet/item-to-helm-source
	 (clojure-cheatsheet/group-by-head
	  (clojure-cheatsheet/flatten
	   (clojure-cheatsheet/propagate-headings clojure-cheatsheet-hierarchy)))))

;;;###autoload
(defun clojure-cheatsheet ()
  "Use helm to show a Clojure cheatsheet."
  (interactive)
  (helm :sources helm-source-clojure-cheatsheet))

(provide 'clojure-cheatsheet)

;;; clojure-cheatsheet.el ends here
