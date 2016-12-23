#lang racket

(require "language.css")

language.css
(printf "Imported a stylesheet? ~a~n" (css-stylesheet? language.css))
