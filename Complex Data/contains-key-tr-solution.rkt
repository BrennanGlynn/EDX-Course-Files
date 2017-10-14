;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname contains-key-tr-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; bt-contains-tr-starter.rkt

; Problem:
; 
; Starting with the following data definition for a binary tree (not a binary search tree) 
; design a tail-recursive function called contains? that consumes a key and a binary tree 
; and produces true if the tree contains the key.
; 


(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))

#;
(define (fn-for-node bt)
  (if (false? bt) (...)
      (... (node-k bt)
           (node-v bt)
           (fn-for-node (node-l bt))
           (fn-for-node (node-r bt)))))

;; Integer BT -> Boolean
;; Looks for a given key in a binary tree and returns true if found
(check-expect (contains? 1 BT1) false)
(check-expect (contains? 7 BT2) true)
(check-expect (contains? 6 BT2) true)
(check-expect (contains? 2 BT2) false)

(define (contains? key bt)
  (local [(define (fn-for-bt bt todo)
            (if (false? bt) (fn-for-lobt todo)
                (if (= key (node-k bt)) true
                    (fn-for-bt (node-l bt) (cons (node-r bt) todo)))))
          
          (define (fn-for-lobt todo)
            (if (empty? todo) false
                (fn-for-bt (first todo) (rest todo))))]
    (fn-for-bt bt empty)))












