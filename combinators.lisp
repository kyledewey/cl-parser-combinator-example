;; simple grammar (from http://www.cs.bgu.ac.il/~yaeln/AI06/class2.html)
;; sentence ::= noun-phrase + verb-phrase
;; noun-phrase ::= article + noun
;; verb-phrase ::= verb + noun-phrase
;; article ::= 'a' | 'the'
;; noun ::= 'man' | 'ball' | 'woman' | 'table'
;; verb ::= 'hit' | 'took' | 'saw' | 'liked'

;; parser: Stream => ParseResult

;; ParseResult:
;; -SuccessfulParseResult: (built-up success, rest)
;; -FailureResult: failure message

(defun make-success (result remaining)
  (list result remaining))

(defun success-rest (result)
  (second result))

(defun success-result (result)
  (first result))

(defun make-fail (msg) msg)

(defun is-fail (res)
  (stringp res))

(defun is-success (res)
  (not (is-fail res)))

(defun make-err-msg (expected saw)
  (concatenate 'string "Expected " expected
	       " but saw " saw))

(defun lit-parser (lit)
  (lambda (stream)
    (cond
     ((null stream) 
      (make-fail
       (make-err-msg (symbol-name lit) "end of stream.")))
     ((not (eq (car stream) lit))
      (make-fail
       (make-err-msg (symbol-name lit) (symbol-name (car stream)))))
     (T (make-success lit (cdr stream))))))

(defun and-parser (p1 p2)
  (lambda (stream)
    (let ((res1 (funcall p1 stream)))
      (if (is-fail res1) res1
	(let ((res2 (funcall p2 (success-rest res1))))
	  (if (is-fail res2) res2
	    (make-success (list (success-result res1)
				(success-result res2))
			  (success-rest res2))))))))

(defun or-parser (p1 p2)
  (lambda (stream)
    (let ((res1 (funcall p1 stream)))
      (if (is-success res1) res1
	(funcall p2 stream)))))

(defun res-parser (p make-res)
  (lambda (stream)
    (let ((res (funcall p stream)))
      (if (is-success res)
	  (make-success
	   (funcall make-res (success-result res))
	   (success-rest res))
	res))))

(defmacro parser* (kind first &rest remaining)
  (if (null remaining) first
    `(funcall ,kind ,first (parser* ,kind ,(car remaining) ,@(cdr remaining)))))

(defmacro or-parser* (first &rest remaining)
  `(parser* ,#'or-parser ,first ,@remaining))

(defmacro and-parser* (first &rest remaining)
  `(parser* ,#'and-parser ,first ,@remaining))
	       
(defun make-sentence (np vp)
  (list 'sentence np vp))

(defun make-noun-phrase (article noun)
  (list 'noun-phrase article noun))

(defun make-verb-phrase (verb np)
  (list 'verb-phrase verb np))

(defun make-article (article)
  (list 'article article))

(defun make-noun (noun)
  (list 'noun noun))

(defun make-verb (verb)
  (list 'verb verb))

(defun noun-parser ()
  (res-parser
   (or-parser* (lit-parser 'man)
	       (lit-parser 'ball)
	       (lit-parser 'woman)
	       (lit-parser 'table))
   #'make-noun))

(defun verb-parser ()
  (res-parser
   (or-parser* (lit-parser 'hit)
	       (lit-parser 'took)
	       (lit-parser 'saw)
	       (lit-parser 'liked))
   #'make-verb))

(defun article-parser ()
  (res-parser
   (or-parser (lit-parser 'a)
	      (lit-parser 'the))
   #'make-article))

(defun verb-phrase-parser ()
  (res-parser
   (and-parser (verb-parser) (noun-phrase-parser))
   #'(lambda (from-and)
       (make-verb-phrase
	(first from-and)
	(second from-and)))))

(defun noun-phrase-parser ()
  (res-parser
   (and-parser (article-parser) (noun-parser))
   #'(lambda (from-and)
       (make-noun-phrase
	(first from-and)
	(second from-and)))))

(defun sentence-parser ()
  (res-parser
   (and-parser (noun-phrase-parser) (verb-phrase-parser))
   #'(lambda (from-and)
       (make-sentence
	(first from-and)
	(second from-and)))))

