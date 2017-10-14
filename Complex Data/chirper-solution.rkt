;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chirper-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;  PROBLEM:
;  
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people. 
;  
;  Design a data definition for Chirper, including a template that is tail recursive and avoids 
;  cycles. 
;  
;  Then design a function called most-followers which determines which user in a Chirper Network is 
;  followed by the most people.
;  


(define-struct user (name verified following))
;; User is (make-user (String Boolean (listOf User)
;; interp. a user with that has a name a verified status and a list of users they follow
(define BRENNAN (make-user "Brennan" true empty))
(define SYD (make-user "Sydnee" false empty))
(define JOSH (make-user "Joshua" false (list SYD)))
(define JAKE (make-user "Jacob" true (list JOSH SYD BRENNAN)))
(define LYNSEE (make-user "Lynsee" false (list JAKE BRENNAN)))

#;
(define (fn-for-user u)
  (local [(define (fn-for-user u visited todo)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-following u) todo)
                            (cons (user-name u) visited))))

          (define (fn-for-lou todo visited)
            (if (empty? todo) (... visited)
                (... (fn-for-user (first todo) (... visited))
                     (fn-for-lou (rest todo) (... visited)))))]
    (fn-for-user u empty empty)))



;; User -> User
;; Returns the user that is followed by the most people in that network
(check-expect (most-followers BRENNAN) BRENNAN)
(check-expect (most-followers JOSH) SYD)
(check-expect (most-followers LYNSEE) BRENNAN)

(define (most-followers u)
  (local [(define-struct entry (user followers))
          ;; entry is (make-entry (User Integer))
          ;; interp. an entry with a user and the number of users that follow them
          
          (define (fn-for-user u todo visited rsf)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited (merge u rsf))
                (fn-for-lou (append (user-following u) todo)
                            (cons (user-name u) visited)
                            (merge u rsf))))

          (define (fn-for-lou todo visited rsf)
            (if (empty? todo) (top-entry rsf)
                (fn-for-user (first todo) (rest todo) visited rsf)))

          (define (merge u loe)
            (if (empty? loe) (cons (make-entry u 1) loe)
                (if (string=? (user-name u) (user-name (entry-user (first loe))))
                    (cons (make-entry (entry-user (first loe)) (add1 (entry-followers (first loe)))) (rest loe))
                    (merge u (rest loe)))))

          (define (top-entry loe)
            (local [(define (top-entry loe rsf)
                      (if (empty? loe) (entry-user rsf)
                          (if (> (entry-followers (first loe)) (entry-followers rsf))
                              (top-entry (rest loe) (first loe))
                              (top-entry (rest loe) rsf))))]
              (top-entry loe (make-entry (make-user "" false empty) 0))))]
    
    (fn-for-user u empty empty empty)))

  


