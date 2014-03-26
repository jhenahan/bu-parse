;; Main interface

;; We start with an empty grammar
(defvar *grammar* nil)

;; This function returns all complete parses of the
;; given word list.
(defun parser (wordlist)
  (mapcar #'parse-tree (complete (parse wordlist))))

;; We define a function to switch grammars.
;; This returns the length since spitting
;; out the entire list of rules is pointless.
;; We need to set it nil first or we get lots
;; of crosstalk if we switch grammars in a
;; single interactive session.
(defun use (grammar) 
  (setf *grammar* nil) 
  (length (setf *grammar* grammar)))

;;; Grammars

(defparameter *grammarA*
  '((S -> (NP VP))
    (NP -> (Pronoun))
    (NP -> (Article Noun))
    (VP -> (VP PP))
    (VP -> (VP Adverb Adverb))
    (VP -> (Verb))
    (PP -> (Preposition NP))
    (NP -> (Noun))
    (Pronoun -> I) (Pronoun -> me) 
    (Pronoun -> you) (Pronoun -> someone)
    (Adverb -> slowly) (Adverb -> quickly)
    (Adverb -> happily) (Adverb -> sadly)
    (Article -> the) (Article -> a) (Article -> an)
    (Verb -> ate) (Verb -> drove) (Verb -> walked)
    (Verb -> saw) (Verb -> went)
    (Preposition -> to) (Preposition -> from)
    (Preposition -> with)
    (Noun -> supermarket) (Noun -> office)
    (Noun -> apple) (Noun -> dog) (Noun -> man)))

(defparameter *grammarB*
  '((S -> (NP VP))
    (NP -> (Pronoun))
    (NP -> (Noun))
    (NP -> (Article NP))
    (VP -> (Verb Vmod))
    (Vmod -> (Adverb Vmod))
    (Vmod -> (Adverb))
    (Adverb -> (PP))
    (PP -> (Preposition NP))
    (Pronoun -> I) (Pronoun -> me) 
    (Pronoun -> you) (Pronoun -> someone)
    (Adverb -> slowly) (Adverb -> quickly)
    (Adverb -> happily) (Adverb -> sadly)
    (Article -> the) (Article -> a) (Article -> an)
    (Verb -> ate) (Verb -> drove) (Verb -> walked)
    (Verb -> saw) (Verb -> went)
    (Preposition -> to) (Preposition -> from)
    (Preposition -> with)
    (Noun -> supermarket) (Noun -> office)
    (Noun -> apple) (Noun -> dog) (Noun -> man)))

(defparameter *grammarC*
  '((S -> (NP VP))
    (NP -> (Pronoun))
    (NP -> (Article NP))
    (VP -> (Verb Adverb))
    (Adverb -> (Adverb Adverb))
    (Adverb -> (PP))
    (PP -> (Preposition NP))
    (NP -> (Noun))
    (Pronoun -> I) (Pronoun -> me) 
    (Pronoun -> you) (Pronoun -> someone)
    (Adverb -> slowly) (Adverb -> quickly)
    (Adverb -> happily) (Adverb -> sadly)
    (Article -> the) (Article -> a) (Article -> an)
    (Verb -> ate) (Verb -> drove) (Verb -> walked)
    (Verb -> saw) (Verb -> went)
    (Preposition -> to) (Preposition -> from)
    (Preposition -> with)
    (Noun -> supermarket) (Noun -> office)
    (Noun -> apple) (Noun -> dog) (Noun -> man)))

;; Plumbing

;; We define a structure for lexical rules so
;; that we can use the familiar notation from
;; the book more easily.
(defstruct (rule (:type list)) left -> right)

;; We define a parse tree by the already parsed
;; part of the tree and the part remaining to be
;; parsed. A successful parse will result in a
;; nil remainder.
(defstruct (parse) tree remainder)

;; We need a few helper functions to construct
;; our parse trees and to refer to the left
;; and right subtrees.
(defun build-tree (left right) (cons left right))
(defun left (tree) (first tree))
(defun right (tree) (rest tree))

;; In the event that we try to parse a word that's
;; not in our lexicon, we'll make a guess from the
;; context.
(defparameter *unknown* '(Noun Verb Adverb))

;; rules takes a word and returns a list of rules
;; with the word on the right hand side. We first
;; check to see if the word has an associated rule
;; in our grammar, and if it does not we guess
;; within *unknown*
(defun rules (word)
  (or (traverse word *grammar* 
                :key #'rule-right
                :test #'equal)
      (mapcar #'(lambda (sym) 
                  `(,sym -> ,word)) 
              *unknown*)))

;; Another helper function to pull out any rule where
;; the given symbol is the first in the right hand side
;; of a rule.
(defun initial (sym)
  (traverse sym *grammar* 
            :key #'(lambda (rule) 
                     (head (rule-right rule)))))

;; Returns nil unless the parse succeeds with
;; no remainder. Returns a parse tree on a complete
;; parse.
(defun complete (parses) 
  (remove-if-not #'null parses :key #'parse-remainder))

;; This is a standard bottom-up parse. The real work happens
;; in the helper function. In essence, we take the first word,
;; get the rule for it, and then check if the parse is done.
;; If it is, we just return the parse tree so far. If it is
;; not (i.e., there are words remaining to be parsed), then
;; we repeat the process with the remaining words. The function
;; will return every parse tree that it can find for a given grammar.
(defun parse (words)
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extension (rule-left rule) 
                           (list (first words))
                           (rest words) nil))
            (rules (first words)))))

;; If we have all the necessary rules in the left hand side
;; to account for the words in the right hand side, we stack
;; parses onto our tree until we hit a nil and then return
;; the entire parse tree.
;; Otherwise, we try to extend to the right until we have a
;; valid parse.
(defun extension (l r remainder necessary)
  (if (null necessary)
      (let 
          ((parse 
            (make-parse 
             :tree (build-tree l r) 
             :remainder remainder)))
        (cons parse
              (mapcan
                #'(lambda (rule)
                    (extension (rule-left rule)
                                  (list (parse-tree parse))
                                  remainder (rest (rule-right rule))))
                (initial l))))
      (mapcan
        #'(lambda (p)
            (if 
             (eq (left (parse-tree p)) (first necessary))
             (extension l 
                        (flip-append r (parse-tree p))
                        (parse-remainder p) 
                        (rest necessary))))
        (parse remainder))))

;; Helper functions

;; Just append with the args flipped
(defun flip-append (items item)
  (append items (list item)))

;; Steps through the haystack and finds any needles according
;; to the key.
(defun traverse (needle haystack 
                 &rest key
                 &key (test #'equal) test-not &allow-other-keys)
  (if test-not
      (apply #'remove needle haystack
             :test-not (complement test-not) key)
      (apply #'remove needle haystack
             :test (complement test) key)))

;; A "safe" version of first so we don't choke on type errors
(defun head (x)
  (if (consp x) (first x) nil))
