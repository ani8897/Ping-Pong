#lang racket
(require 2htdp/image)
(provide step
         delta-t
         boxb-up
         boxb-dn
         boxb-l
         boxb-r
         RADIUS
         LENGTH
         WIDTH
         bar1
         bar2
         background-image
         background-image2
         menu-image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; these are the various constants used in the game;;;;
(define step 8)                                           ;
(define delta-t 5)
(define boxb-up 10)
(define boxb-dn 600)
(define boxb-l 10)
(define boxb-r 990)
(define RADIUS 20)
(define LENGTH 100)
(define WIDTH 50)
(define bar1 (rectangle WIDTH LENGTH "solid" "green"))
(define bar2 (rectangle WIDTH LENGTH "solid" "green"))
(define background-image2 (bitmap "ping.jpeg"))
(define background-image (bitmap "ping1.jpg"))
(define menu-image (bitmap "menu-pong.jpg"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;