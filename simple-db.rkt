#lang racket

(define db%
    (class object%
        (init-field [records '()])
        (define/public (add-record cd)
            (begin 
                (if (empty? records)
                    (set! records cd)
                    (set! records (cons cd records)))))
        (define/public (display-all)
            (for/last ([cd (flatten (list records))])
                (printf "Title:  ~a\nArtist: ~a\nRating: ~a\nRipped: ~a\n\n" 
                    (get-field title cd) 
                    (get-field artist cd) 
                    (get-field rating cd) 
                    (get-field ripped cd))))
    (super-new)))

(define cd%
    (class object%
        (init-field title artist rating ripped)
    (super-new)))

(define (make-cd title artist rating ripped)
    (new cd% [title title] [artist artist] [rating rating] [ripped ripped]))

(define *DB* (make-object db%))


(let loop ()
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
    (send *DB* display-all)

    (loop))