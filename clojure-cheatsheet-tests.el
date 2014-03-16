;;; clojure-cheatsheet-tests.el --- The Clojure Cheatsheet for Emacs tests.

;;; Commentary:
;;
;; The Clojure Cheatsheet for Emacs tests. Note that to run this test
;; suite, you'll need to be running a CIDER session, connected to a
;; Clojure project with all the Cheatsheet's namespaces available.

(require 'ert)
(require 'cl-lib)
(require 'cider-interaction)
(require 'nrepl-client)
(require 'dash)
(require 'clojure-cheatsheet)

;;; Code:

(defmacro should-propagate-to
  (form result)
  `(should (equal (clojure-cheatsheet/propagate-headings (quote ,form))
                  (quote ,result))))

(ert-deftest clojure-cheatsheet/propagate-headings-tests ()
  (should-propagate-to (kaj +)
                       (kaj/+))
  (should-propagate-to (clojure.core + -)
                       (clojure.core/+ clojure.core/-))
  (should-propagate-to ("Level 1" (clojure.core + -))
                       (("Level 1" clojure.core/+ clojure.core/-)))
  (should-propagate-to ("Level 1" (:special def var))
                       (("Level 1" def var)))
  (should-propagate-to ("Level 1"
                        ("Level 2"
                         (clojure.core + -)))
                       ((("Level 1 : Level 2" clojure.core/+ clojure.core/-))))
  (should-propagate-to ("Level 1"
                        (clojure.set union diff)
                        ("Level 2"
                         (clojure.core + -)))
                       (("Level 1" clojure.set/union clojure.set/diff)
                        (("Level 1 : Level 2" clojure.core/+ clojure.core/-)))))

(ert-deftest flatten-hierarchy ()
  (should (equal (clojure-cheatsheet/->> '("Top"
                                           (clojure.core -> ->>)
                                           ("Sub 1"
                                            (:url "Explanation" "http://clojure.org/protocols")
                                            (:special def var)
                                            (clojure.xml data zip)
                                            (clojure.thing this)))
                                         clojure-cheatsheet/propagate-headings
                                         clojure-cheatsheet/flatten
                                         clojure-cheatsheet/group-by-head)
                 '(("Top" clojure.core/-> clojure.core/->>)
                   ("Top : Sub 1"
                    (:url "Explanation" "http://clojure.org/protocols")
                    def
                    var
                    clojure.xml/data clojure.xml/zip clojure.thing/this)))))

(defun strip-all-but-namespace-lists (node)
  (cond ((not (listp node)) node)
        ((keywordp (car node)) nil)
        ((stringp (car node)) (cdr node))
        (t node)))

(defun extract-namespaces (node)
  (cond ((not (listp node)) node)
        ((keywordp (car node)) nil)
        ((stringp (car node)) (cdr node))
        ((symbolp (car node)) (list (car node)))
        (t node)))

(defun qualify-all-symbols (node)
  (if (and (listp node)
           (car node)
           (symbolp (car node)))
	(mapcar (apply-partially #'clojure-cheatsheet/symbol-qualifier (car node))
			(cdr node))
    node))

(defun all-cheatsheet-symbols-qualified ()
  (clojure-cheatsheet/->> clojure-cheatsheet-hierarchy
                          (clojure-cheatsheet/treewalk #'strip-all-but-namespace-lists
                                                       #'qualify-all-symbols)
                          clojure-cheatsheet/flatten
                          (apply #'append)))

(defun all-namespaces ()
  (clojure-cheatsheet/->> clojure-cheatsheet-hierarchy
                          (clojure-cheatsheet/treewalk #'extract-namespaces
                                                       #'identity)
                          clojure-cheatsheet/flatten
                          (apply #'append)
                          -uniq))

(defun require-namespace
  (namespace)
  (let ((result (cider-eval-sync (format "(require '%s)" namespace))))
    (should     (plist-get result :value))
    (should     (plist-get result :done))
    (should-not (plist-get result :stderr))))

(defun check-symbol-bound
  (symbol)
  (let ((var (cider-eval-sync (format "(var %s)" symbol))))
    (should     (plist-get var :value))
    (should     (plist-get var :done))
    (should-not (plist-get var :stderr))) )

(ert-deftest test-clojure-function-references ()
  "Ensure that every function we've defined can actually be found at the REPL."

  (should (nrepl-current-connection-buffer))

  (mapc #'require-namespace
        (all-namespaces))

  (mapc #'check-symbol-bound
        (all-cheatsheet-symbols-qualified)))


(defun all-cider-symbols-qualified ()
  (apply 'append
         (mapcar (lambda (namespace)
                   (let* ((dir (cider-eval-sync (format "(clojure.repl/dir %s)" namespace)))
                          (symbols (plist-get dir :stdout))
                          (qualified-symbols (mapcar (lambda (symbol)
                                                       (clojure-cheatsheet/symbol-qualifier namespace symbol))
                                                     (split-string symbols "\n" t))))
                     qualified-symbols))
                 (all-namespaces))))

(ert-deftest test-all-symbols-accounted-for ()
  "Checks that, for every namespace the cheatsheet covers, it covers every var in that namespace.
eg. If the cheatsheet covers clojure.repl, it should have an entry for everything in clojure.repl."

  (let ((ignorable '(clojure.java.io/IOFactory
                     clojure.core/*allow-unresolved-vars*
                     clojure.core/*assert*
                     clojure.core/*compiler-options*
                     clojure.core/*flush-on-newline*
                     clojure.core/*fn-loader*
                     clojure.core/*math-context*
                     clojure.core/*read-eval*
                     clojure.core/*source-path*
                     clojure.core/*use-context-classloader*
                     clojure.core/*verbose-defrecords*
                     clojure.core/->ArrayChunk
                     clojure.core/->Vec
                     clojure.core/->VecNode
                     clojure.core/->VecSeq
                     clojure.core/-cache-protocol-fn
                     clojure.core/-reset-methods
                     clojure.core/EMPTY-NODE
                     clojure.core/accessor
                     clojure.core/add-classpath
                     clojure.core/defstruct
                     clojure.core/agent-errors
                     clojure.core/clear-agent-errors
                     clojure.data/diff-similar
                     clojure.data/equality-partition
                     clojure.data/Diff
                     clojure.data/EqualityPartition
                     clojure.core/chunk
                     clojure.core/chunk-append
                     clojure.core/chunk-buffer
                     clojure.core/chunk-cons
                     clojure.core/chunk-first
                     clojure.core/chunk-next
                     clojure.core/chunk-rest
                     clojure.core/replicate
                     clojure.core/destructure

                     clojure.core.async/Mix
                     clojure.core.async/Mult
                     clojure.core.async/Mux
                     clojure.core.async/Pub
                     clojure.core.async/admix*
                     clojure.core.async/do-alt
                     clojure.core.async/fn-handler
                     clojure.core.async/ioc-alts!
                     clojure.core.async/muxch*
                     clojure.core.async/solo-mode*
                     clojure.core.async/sub*
                     clojure.core.async/tap*
                     clojure.core.async/toggle*
                     clojure.core.async/unmix*
                     clojure.core.async/unmix-all*
                     clojure.core.async/unsub*
                     clojure.core.async/unsub-all*
                     clojure.core.async/untap*
                     clojure.core.async/untap-all*

                     clojure.test/*initial-report-counters*
                     clojure.test/*load-tests*
                     clojure.test/*report-counters*
                     clojure.test/*stack-trace-depth*
                     clojure.test/*test-out*
                     clojure.test/*testing-contexts*
                     clojure.test/*testing-vars*
                     clojure.test/assert-any
                     clojure.test/assert-expr
                     clojure.test/assert-predicate
                     clojure.test/do-report
                     clojure.test/file-position
                     clojure.test/function?
                     clojure.test/get-possibly-unbound-var
                     clojure.test/inc-report-counter
                     clojure.test/report
                     clojure.test/set-test
                     clojure.test/successful?
                     clojure.test/test-all-vars
                     clojure.test/test-ns
                     clojure.test/test-var
                     clojure.test/testing
                     clojure.test/testing-contexts-str
                     clojure.test/testing-vars-str
                     clojure.test/try-expr
                     clojure.test/with-test
                     clojure.test/with-test-out))

        (unfiled '(clojure.core/await1
                   clojure.repl/set-break-handler!
                   clojure.core/bound?

                   clojure.core/construct-proxy
                   clojure.core/create-struct
                   clojure.core/find-protocol-impl
                   clojure.core/find-protocol-method
                   clojure.core/get-proxy-class
                   clojure.core/hash-combine
                   clojure.core/init-proxy
                   clojure.core/method-sig
                   clojure.core/munge
                   clojure.core/namespace-munge
                   clojure.core/primitives-classnames
                   clojure.core/print-ctor
                   clojure.core/print-dup
                   clojure.core/print-method
                   clojure.core/print-simple
                   clojure.core/proxy
                   clojure.core/proxy-call-with-super
                   clojure.core/proxy-mappings
                   clojure.core/proxy-name
                   clojure.core/proxy-super
                   clojure.core/reduced
                   clojure.core/reset-meta
                   clojure.core/struct
                   clojure.core/struct-map
                   clojure.core/thread-bound?
                   clojure.core/underive
                   clojure.core/unquote
                   clojure.core/unquote-splicing
                   clojure.core/update-proxy
                   clojure.core/with-bindings
                   clojure.core/with-bindings*
                   clojure.core/with-loading-context
                   clojure.xml/*current*
                   clojure.xml/*sb*
                   clojure.xml/*stack*
                   clojure.xml/*state*
                   clojure.xml/attrs
                   clojure.xml/content
                   clojure.xml/content-handler
                   clojure.xml/element
                   clojure.xml/emit
                   clojure.xml/emit-element
                   clojure.xml/startparse-sax
                   clojure.xml/tag
                   clojure.pprint/*print-base*
                   clojure.pprint/*print-miser-width*
                   clojure.pprint/*print-pprint-dispatch*
                   clojure.pprint/*print-pretty*
                   clojure.pprint/*print-radix*
                   clojure.pprint/*print-suppress-namespaces*
                   clojure.pprint/code-dispatch
                   clojure.pprint/formatter
                   clojure.pprint/formatter-out
                   clojure.pprint/fresh-line
                   clojure.pprint/get-pretty-writer
                   clojure.pprint/pprint-indent
                   clojure.pprint/pprint-logical-block
                   clojure.pprint/pprint-newline
                   clojure.pprint/pprint-tab
                   clojure.pprint/print-length-loop
                   clojure.pprint/set-pprint-dispatch
                   clojure.pprint/simple-dispatch
                   clojure.pprint/with-pprint-dispatch
                   clojure.pprint/write
                   clojure.pprint/write-out
                   clojure.java.browse/*open-url-script*
                   clojure.java.shell/*sh-dir*
                   clojure.java.shell/*sh-env*
                   clojure.repl/demunge
                   clojure.repl/root-cause
                   clojure.repl/set-break-handler
                   clojure.repl/stack-element-str
                   clojure.repl/thread-stopper
                   clojure.java.javadoc/*core-java-api*
                   clojure.java.javadoc/*feeling-lucky*
                   clojure.java.javadoc/*feeling-lucky-url*
                   clojure.java.javadoc/*local-javadocs*
                   clojure.java.javadoc/*remote-javadocs*
                   clojure.java.javadoc/add-local-javadoc
                   clojure.java.javadoc/add-remote-javadoc
                   clojure.java.io/Coercions
                   clojure.java.io/default-streams-impl
                   )))
    (should
     (not (set-difference
           (set-difference
            (all-cider-symbols-qualified)
            (all-cheatsheet-symbols-qualified))
           (append ignorable unfiled))))))

(provide 'clojure-cheatsheet-tests)

;;; clojure-cheatsheet-tests.el ends here
