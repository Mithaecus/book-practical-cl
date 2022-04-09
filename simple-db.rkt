#lang racket

(define cd%
    (class object%
        (init-field title artist rating ripped)
    (super-new)))

(define (make-cd title artist rating ripped)
    (new cd% [title title] [artist artist] [rating rating] [ripped ripped]))

(define db%
    (class object%
        (init-field [records '()])
        (define/public (add-record cd)
            (begin 
                (if (empty? records)
                    (set! records cd)
                    (set! records (cons cd records)))))
        (define/public (dump-all)
            (open-output-file "./db" #:exists 'replace)
            (for/last ([cd (flatten (list records))])
                (write (
                    list (get-field title cd) 
                         (get-field artist cd) 
                         (get-field rating cd) 
                         (get-field ripped cd)) 
                    (open-output-file "./db" #:exists 'append))
            (close-output-port (open-output-file "./db" #:exists 'append))))
        (define/public (load)
            (let ([input (file->list "./db")])
                (for ([cd input])
                    (add-record 
                        (make-cd (first cd)        ; Title
                                 (second cd)       ; Artist
                                 (third cd)        ; Rating
                                 (fourth cd))))))  ; Ripped
    (super-new)))

(define *DB* (make-object db%))

(let loop ()
    (display "Load? ")
    (define load? (read-line (current-input-port) 'any))

    (if (or (equal? load? "Y")
            (equal? load? "Yes")
            (equal? load? "y")
            (equal? load? "yes"))
        (send *DB* load)
        (void))

    (display "Let's add a CD!\n")
    (display "Title: ")
    (define title (read-line (current-input-port) 'any))

    (display "Artist: ")
    (define artist (read-line (current-input-port) 'any))

    (display "Rating: ")
    (define rating (read-line (current-input-port) 'any))

    (display "Ripped: ")
    (define ripped (read-line (current-input-port) 'any))

    (define cd (make-cd title artist rating ripped))

    (send *DB* add-record cd)
    (send *DB* dump-all)
    
    (display "Quit? ")
    (define quit (read-line (current-input-port) 'any))

    (if (or (equal? quit "N")
            (equal? quit "No")
            (equal? quit "n")
            (equal? quit "no"))
        (void)
        (exit))

    (loop))