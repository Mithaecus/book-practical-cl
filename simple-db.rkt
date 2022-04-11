#lang racket

(define cd%
    (class object%
        (init-field title artist rating ripped)
    (super-new)))

(define db%
    (class object%
        (init-field [records '()])
        (define/public (add-record cd)
            (begin 
                (if (empty? records)
                    (set! records cd)
                    (set! records (cons cd records)))))
        (define/public (dump-all)
            (open-output-file "./data/db" #:exists 'replace)
            (for/last ([cd (flatten (list records))])
                (write (
                    list (get-field title cd) 
                         (get-field artist cd) 
                         (get-field rating cd) 
                         (get-field ripped cd)) 
                    (open-output-file "./data/db" #:exists 'append))
            (close-output-port (open-output-file "./data/db" #:exists 'append))))
        (define/public (load)
            (let ([input (file->list "./data/db")])
                (for ([cd input])
                    (add-record 
                        (new cd% [title (first cd)]
                                 [artist (second cd)]
                                 [rating (third cd)]
                                 [ripped (fourth cd)]))))) 
        (define/public (select-by-artist artist)
            (let ([results '()])
                (for ([cd (flatten (list records))])
                    (if (equal? (get-field artist cd) artist)
                        (set! results (cons cd results))
                        (void)))
                results))
    (super-new)))

(define *DB* (make-object db%))

(let loop ()
    
    (send *DB* load)

    (display "Let's add a CD!\n")
    (display "Title: ")
    (define title (read-line (current-input-port) 'any))

    (display "Artist: ")
    (define artist (read-line (current-input-port) 'any))

    (display "Rating: ")
    (define rating (read-line (current-input-port) 'any))

    (display "Ripped: ")
    (define ripped (read-line (current-input-port) 'any))

    (send *DB* add-record 
        (new cd% [title title] 
                 [artist artist] 
                 [rating rating] 
                 [ripped ripped]))

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
