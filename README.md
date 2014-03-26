Jack Henahan

CS 251

HW3

Lisp: SBCL 1.1.12

Comments included in source file

Interact with the program using `(use grammar)` and `(parser words)`.

Example runs:

```lisp
CL-USER> (use *grammarA*)
32
CL-USER> (parser '(I drove to the office slowly))
NIL
CL-USER> (parser '(Someone went to the supermarket))
((S (NP (PRONOUN SOMEONE))
  (VP (VP (VERB WENT))
   (PP (PREPOSITION TO) (NP (ARTICLE THE) (NOUN SUPERMARKET))))))

CL-USER> (use *grammarB*)
33
CL-USER> (parser '(I drove to the office slowly))
((S (NP (PRONOUN I))
  (VP (VERB DROVE)
   (VMOD (ADVERB (PP (PREPOSITION TO) (NP (ARTICLE THE) (NP (NOUN OFFICE)))))
    (VMOD (ADVERB SLOWLY))))))
CL-USER> (parser '(I drove slowly to the office))
((S (NP (PRONOUN I))
  (VP (VERB DROVE)
   (VMOD (ADVERB SLOWLY)
    (VMOD
     (ADVERB (PP (PREPOSITION TO) (NP (ARTICLE THE) (NP (NOUN OFFICE))))))))))
CL-USER> (parser '(Someone saw the dog happily eat the apple))
NIL

CL-USER> (use *grammarC*)
32
CL-USER> (parser '(I slowly drove to the office))
NIL
CL-USER> (parser '(I drove slowly to the office))
((S (NP (PRONOUN I))
  (VP (VERB DROVE)
   (ADVERB (ADVERB SLOWLY)
    (ADVERB (PP (PREPOSITION TO) (NP (ARTICLE THE) (NP (NOUN OFFICE)))))))))
CL-USER> (parser '(someone went to the supermarket slowly))
((S (NP (PRONOUN SOMEONE))
  (VP (VERB WENT)
   (ADVERB
    (ADVERB (PP (PREPOSITION TO) (NP (ARTICLE THE) (NP (NOUN SUPERMARKET)))))
    (ADVERB SLOWLY)))))
CL-USER> (parser '(the dog ate happily))
((S (NP (ARTICLE THE) (NP (NOUN DOG))) (VP (VERB ATE) (ADVERB HAPPILY))))
```