# **Tamer's Handbook**

> This handbook shows my _Programming Methodology_ via testing
> [makefile.rkt](/Users/wargrey/DigitalWorld/makefile.rkt) itself (implies
> the                                                            [minimal
> common code
> base](/Users/wargrey/DigitalWorld/DigiGnome/digitama/runtime.rkt))
> that the entire project should follow.
> _**Principal** This sample should be (rather than must be)  followed due
> to the complexity of the real world problems.  In fact it's all right to
> forget it after reading._

_Translating is the most complex human activity in the Universe._

`**Story:** Make a _Tamer's Handbook_ to Show the Sample`
`**In order to** standardize my development process and to make life
joyful`   `**As an** indenpendent developer`   `**I want to** write the
_handbook_ with _Literate Programming_ skill`
`**Scenario 1:** The project should provide "tamer/handbook.scrbl"`
`**Given** the project have been launched and ready for developing`
`**And** I currently have not found any appropriate specification
templates`   `**When** I do some researching and architect a template`
`**Then** I should see the "handbook.scrbl" in "tamer"`
`Hmm... the _Gherkin language_, semiformal and elegant, or maybe stupid.
Nevertheless, it's not my cup of tea...`

Since I'm an indenpendent developer and communicating costs almost
nothing, I make decision at my risk to model software system in [Formal
Methods](http://en.wikipedia.org/wiki/Formal\_methods) and document it
with the [Literate
Programming](http://en.wikipedia.org/wiki/Literate\_programming)
approach. After all this _handbook_ plays the role of the [Test
Report](http://en.wikipedia.org/wiki/Behavior-driven\_development) along
with the [Design
Documentation](http://en.wikipedia.org/wiki/Design\_by\_contract) so
that the production code could keep elegant.

_Harness Summary of DigiGnome/tamer_
`makefile.rkt....................................................#?` `⧴
FAILURE » EXAMPLE of FAILURE` `⧴ FATAL » (⧴ exn:fail:user)` `⧴ FAILURE »
Rule 5: DigiGnome/stone/index.scrbl` `⧴ FAILURE » Rule 6: readme.scrbl`
`⧴ FAILURE » Rule 6: readme.scrbl`  `76.19% tests successful.`
`Testsuite = 1, Testcases = 21, Failures = 4, Error = 1.` `0.021
wallclock seconds (0.021 task + 0.000 gc = 0.021 CPU).`

    1 Story: Hello, Hacker Hero!                          
      1.1 Scenario: Ready? Let's have a try!              
      1.2 Scenario: The rules serve you!                  
        1.2.1 Rules on project organization               
        1.2.2 Rules on project documentation              
      1.3 Scenario: What if the _handbook_ is unavaliable?

## 1. Story: Hello, Hacker Hero!

Every hacker needs a _makefile.rkt_ to make life simple. However testing
building routines always makes nonsense but costs high, thus besides the
simplest examples, I will check whether the subprojects satisfy the
rules.

**_<makefile>_ ::=**

```racket
{module story racket     
  <makefile taming start>
                         
  <ready? help!>         
  <hello rules!>}        
                         
<tamer battle>           
```

where **_<makefile taming start>_ ::=**

```racket
(require "tamer.rkt")                                      
                                                           
(tamer-story (tamer-story->libpath "makefile.rkt"))        
(tamer-partner (tamer-partner->filepath "../makefile.rkt"))
```

_DigiGnome/tamer/makefile.rkt_
`Behavioral Specification Examples...............................#?` `⧴
FAILURE » EXAMPLE of FAILURE` `⧴ FATAL » (⧴ exn:fail:user)` `Rules:
info.rkt settings........................................#t` `Rules:
readme.md readers........................................#f` `⧴ FAILURE
» Rule 5: DigiGnome/stone/index.scrbl` `⧴ FAILURE » Rule 6:
readme.scrbl` `⧴ FAILURE » Rule 6: readme.scrbl`  `76.19% tests
successful.` `Testsuites = 3, Testcases = 21, Failures = 4, Error = 1.`
`0.002 wallclock seconds (0.002 task + 0.000 gc = 0.002 CPU).`

### 1.1. Scenario: Ready? Let's have a try!

**_<ready? help!>_ ::=**

```racket
(current-directory (let ([story (cadadr (tamer-story))])             
                     (path-only (build-path (digimon-world) story))))
                                                                     
(define-values {make out err $? setup teardown}                      
  (values (dynamic-require (tamer-partner) 'main {λ _ #false})       
          (open-output-bytes 'stdout)                                
          (open-output-bytes 'stderr)                                
          (make-parameter +nan.0)                                    
          {λ argv {λ _ (parameterize ([current-output-port out]      
                                      [current-error-port err]       
                                      [exit-handler $?])             
                         (apply make argv))}}                        
          {λ _ (void (get-output-bytes out #true)                    
                     (get-output-bytes err #true)                    
                     ($? +nan.0))}))                                 
                                                                     
(define-tamer-suite spec-examples "Behavioral Specification Examples"
  (list <testsuite: should pass and do pass.>                        
        <testsuite: should pass but do fail!>                        
        <testsuite: fatal should never happen!>))                    
```

Although normal **Racket** script doesn't require the so-called `main`
routine, I still prefer to start with

```racket
(main argument ...) -> void?
  argument : string?        
```

You may have already familiar with the [GNU
Make](http://en.wikipedia.org/wiki/Make\_(software)), nonetheless you
are still free to check the options first. Normal **Racket** program
always knows `‑h` or `‑‑help` option:

```racket
> ((dynamic-require/expose (tamer-story) 'make) "--help")            
makefile.rkt [ <option> ... ] [<phony-target|file-path>] ...         
  Carefully options are not exactly the same as those of GNU Make.   
                                                                     
 where <option> is one of                                            
  -B, --always-make : Unconditionally make all targets.              
  -n, --dry-run : Just make without updating targets. [Except *.rkt] 
  -s, --silent : Just run commands but output nothing if no errors.  
  -t, --touch : Touch targets instead of remaking them if it exists. 
  -v, --verbose : Build with verbose messages.                       
* +o <digimon>, ++only <digimon> : Only build <digimon>s.            
  --help, -h : Show this help                                        
                                                                     
 where <phony-target> is one of                                      
  all : Build the entire software without documentation. [default]   
  mostlyclean : Delete all except when remaking costs high.          
  clean : Delete all except those record the configuration.          
  distclean : Delete all that are excluded in the distribution.      
  maintainer-clean : Delete all that can be remade. [For Maintainers]
  check : Validate and generate test report along with documentation.
make: [error] /DigiGnome/tamer/handbook.scrbl needs a proper         
`exit-handler`!                                                      
; See, _makefile_ complains that **Scribble** is killed by accident. 
```

Now it's time to look deep into the specification examples of

> `λ Behavioral Specification Examples` `1 make [option]` `1.1 make
> --silent --help` `#t 1 2ms should exit normally` `#t 2 0ms should have
> to quiet` `1.2 make --silent --unknown` `#t 1 1ms should abort`
> `#t 2 0ms should report errors` `2 make [phony goal]` `#f 1 - EXAMPLE of
> FAILURE` `_»» make: I don't know how to make `love`!_` `3 EXAMPLE DO
> NEED A NAME` `#? 1 - (⧴ exn:fail:user)` `_»» Testcase should have a
> name_` `1 failure 1 error`

**_<testsuite: should pass and do pass.>_ ::=**

```racket
(test-suite "make [option]"                                          
            (test-suite "make --silent --help"                       
                        #:before (setup "--silent" "--help")         
                        #:after teardown                             
                        (test-pred "should exit normally" zero? ($?))
                        (test-pred "should have to quiet"            
                                   zero? (file-position out)))       
            (test-suite "make --silent --unknown"                    
                        #:before (setup "--silent" "--unknown")      
                        #:after teardown                             
                        (test-false "should abort" (zero? ($?)))     
                        (test-pred "should report errors"            
                                   positive? (file-position err))))  
```

**_<testsuite: should pass but do fail!>_ ::=**

```racket
(test-suite "make [phony goal]"                                     
            (test-not-exn "EXAMPLE of FAILURE" {λ _ (make "love")}))
```

**_<testsuite: fatal should never happen!>_ ::=**

```racket
(test-suite "EXAMPLE DO NEED A NAME"                    
            (test-begin (fail "None of my bussiness!")))
```

### 1.2. Scenario: The rules serve you!

**_<hello rules!>_ ::=**

```racket
(define digidirs (filter {λ [sub] (and (directory-exists? sub)                
                                       (regexp-match? #px"/[^.][^/.]+$" sub))}
                         (directory-list (digimon-world) #:build? #true)))    
                                                                              
<rule: info.rkt>                                                              
<rule: readme.md>                                                             
```

Since _Behavior Driven Development_ is the evolution of _Test Driven
Development_ which does not define what exactly should be tested and how
would the tests be performed correct. The term _Architecture_ is all
about designing rules, and this story is all about building system. So
apart from conventions, we need a sort of rules that the _makefile.rkt_
(and systems it builds) should satisfy.

> Meanwhile _parallel building_ is not supported.

#### 1.2.1. Rules on project organization

**_<rule: info.rkt>_ ::=**

```racket
(require setup/getinfo)                                      
                                                             
(define info-root (get-info/full (digimon-world)))           
                                                             
(define-tamer-suite rules:info.rkt "Rules: info.rkt settings"
  (cons (test-suite "with /info.rkt"                         
                    <rules: ROOT/info.rkt>)                  
        (for/list ([digidir (in-list digidirs)])             
          (define digimon (file-name-from-path digidir))     
          (define info-ref (get-info/full digidir))          
          (test-suite (format "with /~a/info.rkt" digimon)   
                      <rules: DIGIMON/info.rkt>))))          
```

> `λ Rules: info.rkt settings` `1 with /info.rkt` `#t 1 0ms Rule 1: multi`
> `#t 2 0ms Subprojects should have their own info.rkt` `2 with
> /DigiGnome/info.rkt` `#t 1 0ms Rule 2: collection` `#t 2 0ms Rule 3:
> compile-omit-paths` `#t 3 0ms Rule 4: test-omit-paths` `3 with
> /nanomon/info.rkt` `#t 1 0ms Rule 2: collection` `#t 2 0ms Rule 3:
> compile-omit-paths` `#t 3 0ms Rule 4: test-omit-paths` `4 with
> /sakuyamon/info.rkt` `#t 1 0ms Rule 2: collection` `#t 2 0ms Rule 3:
> compile-omit-paths` `#t 3 0ms Rule 4: test-omit-paths`

* **Rule 1** The entire project is a multi-collection package,
  non-hidden directories within it are considered as the subprojects.

* **Rule 2** Each subproject should have an explicit name,  even if the
  name is the same as its directory.

* **Rule 3** `compile-collection-zos` and friends should never touch
  these files or directories:  `"makefile.rkt"`, `"submake.rkt"`,
  `"info.rkt"`,  `"stone"` and  `"tamer"`.

* **Rule 4** `raco test` should do nothing since we would do testing  in
  a more controllable way.

**_<rules: ROOT/info.rkt>_ ::=**

```racket
(test-case "Rule 1: multi"                                             
           (check-not-false info-root "/info.rkt not found!")          
           (check-equal? (info-root 'collection) 'multi                
                         "'collection should be 'multi")               
           (check-equal? (info-root 'compile-omit-paths) 'all          
                         "'compile-omit-paths should be 'all")         
           (check-equal? (info-root 'test-omit-paths) 'all             
                         "'test-omit-paths should be 'all"))           
(test-case "Subprojects should have their own info.rkt"                
           (check-pred positive? (length digidirs) "No project found!")
           (for ([digidir (in-list digidirs)])                         
             (check-pred file-exists? (build-path digidir "info.rkt")  
                         (format "/~a/info.rkt not found!"             
                                 (file-name-from-path digidir)))))     
```

**_<rules: DIGIMON/info.rkt>_ ::=**

```racket
(test-case "Rule 2: collection"                                            
           (with-check-info                                                
            {{'info.rkt (build-path digimon "info.rkt")}}                  
            (check-pred string? (info-ref 'collection)                     
                        "'collection should be string!")))                 
(test-case "Rule 3: compile-omit-paths"                                    
           (with-check-info                                                
            {{'info.rkt (build-path digimon "info.rkt")}}                  
            (let ([compile-omit-paths (info-ref 'compile-omit-paths)])     
              (check-not-false compile-omit-paths                          
                               "'compile-omit-paths not defined!")         
              (if (equal? compile-omit-paths 'all)                         
                  (check-true #true)                                       
                  (for ([omit (in-list (list "makefile.rkt" "submake.rkt"  
                                             "info.rkt" "stone" "tamer"))])
                    (check-not-false (member omit compile-omit-paths)      
                                     (format "~a should in                 
compile-omit-paths"                                                        
                                             omit)))))))                   
(test-case "Rule 4: test-omit-paths"                                       
           (with-check-info                                                
            {{'info.rkt (build-path digimon "info.rkt")}}                  
            (let ([test-omit-paths (info-ref 'test-omit-paths)])           
              (check-not-false test-omit-paths                             
                               "'test-omit-paths not defined!")            
              (check-equal? test-omit-paths 'all                           
                            "'test-omit-paths should be 'all!"))))         
```

> Documentation are deployed in [my website](gyoudmon/org) with
> **Scribble**,                               while sources are hosted in
> [Github](github.com/digital-world) with **Markdown**.

#### 1.2.2. Rules on project documentation

**_<rule: readme.md>_ ::=**

```racket
(define /stone (find-relative-path (digimon-zone) (digimon-stone)))
                                                                   
(define-tamer-suite rules:readme.md "Rules: readme.md readers"     
  (cons (test-suite "/index.scrbl"                                 
                    <rules: ROOT/readme.md>)                       
        (for/list ([digidir (in-list digidirs)])                   
          (define digimon (file-name-from-path digidir))           
          (define stonedir (build-path digimon /stone))            
          (test-suite (format "with ~a" stonedir)                  
                      <rules: DIGIMON/readme.md>))))               
```

> `λ Rules: readme.md readers` `1 /index.scrbl` `#f 1 - Rule 5:
> DigiGnome/stone/index.scrbl` `_»» index.scrbl should exists!_` `2 with
> DigiGnome/stone` `#t 1 0ms Rule 6: readme.scrbl` `3 with nanomon/stone`
> `#f 1 - Rule 6: readme.scrbl` `_»» readme.scrbl should exists!_` `4 with
> sakuyamon/stone` `#f 1 - Rule 6: readme.scrbl` `_»» readme.scrbl should
> exists!_` `3 failures 0 error`

* **Rule 5** The project’s toplevel _README.md_ is designated as the
  _main-toc_ of **Scribble**.

* **Rule 6** Each subproject’s _README.md_ is designated as its own
  content table.

**_<rules: ROOT/readme.md>_ ::=**

```racket
(test-case (format "Rule 5: ~a/~a/index.scrbl" (digimon-gnome) /stone)
           (check-pred file-exists?                                   
                       (build-path (digimon-stone) "index.scrbl")     
                       "index.scrbl should exists!"))                 
```

**_<rules: DIGIMON/readme.md>_ ::=**

```racket
(test-case "Rule 6: readme.scrbl"                                 
           (with-check-info                                       
            {{'stonedir stonedir}}                                
            (check-pred file-exists?                              
                        (build-path digidir /stone "readme.scrbl")
                        "readme.scrbl should exists!")))          
```

### 1.3. Scenario: What if the _handbook_ is unavaliable?

Furthermore, the _handbook_ itself is the standard test report, but it's
still reasonable to check the system in some more convenient ways. Hence
we have **_<tamer battle>_ ::=**

```racket
{module main racket      
  <makefile taming start>
                         
  (exit (tamer-spec))}   
```

Run `racket «`tamer files`»` we will get _hspec-like_ report.

Technically speaking, `raco test --submodule main` is always there,
although that way is not recommended, and is omitted by `"info.rkt"`.
