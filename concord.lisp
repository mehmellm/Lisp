;;;;
;;;; Lucas Mehmel
;;;; CSC351 Lab 9
;;;;


;;; Concord from McConnel's directory
;; reads lines from a text file and calls insertword with each word

(defun concord ()
   (princ "Enter the file name:")
   (let ((linenumber 1) (wordlist '()) (port (open (read-line))))
	(do ((nextword '() nextword) (nextchar 'a' nextchar)) 
	    ((equal 'eof nextword))
		(cond 
		      ((char= (peek-char nil port nil 'eof) (code-char 10)) 
				(setf linenumber (+ linenumber 1)))
		)
		(setf nextword (read port nil 'eof))
		(cond 
		      ((not (equal 'eof nextword))
			(setf wordlist 
			      (insertword (list nextword linenumber) wordlist)))
		)
	)
	(printlist wordlist)
	'"** end of list **"
   )
)

;;; insertword
;;  this creates a two dimensnional list of words and there line numbers that they appear on

(defun insertword (theList words)
		(cond ((null words) (list theList))
			((eq (car theList) (caar words)) (cons theList words))
			(t(insert-it (car words) (insertword theList (cdr words))))
		)
)

;;; insert-it 
;;  returns the sorted list with new element

(defun insert-it (x words)
		(cond ((null words) (cons x '()))
			((string= (car x) (caar words)) (checkline x words))
			((string< (car x) (caar words)) (cons x words))
			(t(cons (car words) (insert-it x (cdr words))))
		)
)


;;; checkline
;;  this compares the line numbers for the words and returns the sorted list based on the line number

(defun checkline(e wordlist)
  	(cond
		((null wordlist) (cons e '()))
	        ((> (cadr e) (cadar wordlist)) (cons (car wordlist) (checkline e (cdr wordlist))))
	        ((<= (cadr e) (cadar wordlist))(cons e wordlist))
	 )
)

;;; printlist
;;  prints each word and the first line number it appeared on

(defun printlist (wordlist)
 	 (cond ((null wordlist) nil)
		((string= (caar wordlist) (caadr wordlist)) (print (caar wordlist))
		 (prin1 (cadar wordlist)) (printlist (mulprint (caar wordlist) (cdr wordlist))))
                 (t(print (caar wordlist)) (prin1 (cadar wordlist)) (printlist (cdr wordlist)))
         )
)


;;; mulprint
;;  this prints the line number of words that appeared multiple times

(defun mulprint (key wordlist)
	  (cond
     		((null (cdr wordlist))
   			 (cond
           	          ((string= (caar wordlist) key) (format t ", ~D" (cadar wordlist)) nil)    
      	                          (t wordlist))
                         )
                ((not (string= (caar wordlist) key))  wordlist)
                ((not (= (cadar wordlist) (cadadr wordlist)))
                     (format t ", ~D" (cadar wordlist)) (mulprint (caar wordlist) (cdr wordlist))
                )
               ((= (cadar wordlist) (cadadr wordlist))
                    (format t ", ~D"(cadar wordlist)) (mulprint key (mulchecker 0 key wordlist))
               )
             ((string= (caar wordlist) key) (format t ", ~D"(cadar wordlist)))
	)
)

;;; mulchecker
;;  this functions prints how many times it appears on the same line

(defun mulchecker (num key wordlist)
  	(cond
      		((not (= (cadar wordlist) (cadadr wordlist))) (format t "(~D)" (+ 1 num)) (cdr wordlist))
              		((= (cadar wordlist) (cadadr wordlist)) (mulchecker (+ 1 num) key (cdr wordlist)))
        )
 )

 

