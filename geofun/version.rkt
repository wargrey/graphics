#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [cairo_version cairo-version]
                     [pango_version pango-version]

                     [cairo_version_string cairo-version-string]
                     [pango_version_string pango-version-string]))

(require "digitama/unsafe/more.rkt")
