# Bitmap: Functional Pictures

wargrey

```racket
 (require bitmap/base) package: [graphics](https://pkgs.racket-lang.org/package/graphics)
 (require bitmap)     
```

The `bitmap` is a typed functional picture library which employs
\[Cairo\] and \[Pango\] via `ffi/unsafe` directly. The initial
motivation is to provide efficient, professional, yet handy graphics
APIs that lay the founddation for my `css` engine as well as to be used
standalone for developers and artists.

​_WARNING: This library is still experimental and everything is subject
to change._​

​_WARNING: To keep compatible with other Racket picture libraries is not
the goal._​


