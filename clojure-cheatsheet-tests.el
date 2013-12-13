;;; clojure-cheatsheet-tests.el --- The Clojure Cheatsheet for Emacs tests.

;;; Commentary:
;;
;; The Clojure Cheatsheet for Emacs tests.

(require 'ert)
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

(defun all-symbols-qualified ()
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
  (cider-eval-sync (format "(require '%s)" namespace)))

(defun check-symbol-bound
  (symbol)
  (let ((var (cider-eval-sync (format "(var %s)" symbol))))
	(should     (plist-get var :value))
	(should-not (plist-get var :stderr))) )

(ert-deftest test-clojure-function-references ()
  (should (nrepl-current-connection-buffer))

  (mapc #'require-namespace
		(all-namespaces))

  (mapc #'check-symbol-bound
		(all-symbols-qualified)))

(provide 'clojure-cheatsheet-tests)

;;; clojure-cheatsheet-tests.el ends here
