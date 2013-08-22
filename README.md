# cl-web-dev
###### Because I don't want to write this shit every time

#### What this is

A collection of functions and macros I've found useful while doing web development in Common Lisp. These particular ones make enough sense to me that I always either define them up-front when starting a new system, or I wish I had them. There's some non-web related stuff that I've nevertheless found useful in enough related projects that they're in. *You* may not find them useful, in which case you should continue doing what you're doing.

#### What this isn't

- A framework. I guess you could call it a tool-kit, but that makes it sound heavier than it is.
- Platform Independent. It uses [cl-who](http://weitz.de/cl-who/), [hunchentoot](http://weitz.de/hunchentoot/) and [parenscript](http://common-lisp.net/project/parenscript/), and isn't built to support anything else.
- A way to avoid learning `cl-who`, `parenscript` or `hunchentoot`. It lets you use each of them a bit more easily, but that's all.

# Usage

You just need to `:use` the packages `:cl-web-dev` and `:parenscript` in whatever project you'd like. Load the others mentioned, but `:cl-web-dev` re-exports all the relevant symbols from `:cl-who` and `:hunchentoot`, so you don't need to worry about those. 

The "Hello World" is...

    (ql:quickload :cl-web-dev)
    (defpackage :your-package (:use :cl :cl-web-dev :parenscript))
    (in-package :your-package)
    
    (define-handler test ()
      (html-str
        (:html
          (:body
           (:h1 "Hello World!")
           (:p "From" (:code "cl-web-dev") "!")
           (:script (str (ps (alert "And also, parenscript"))))))))
    
    (defparameter server (easy-start 4242))

# Exported Symbols

## General purpose stuff

These are just simple, general-purpose utilities. You can find them in [alexandria](http://common-lisp.net/project/alexandria/) and/or [anaphora](http://common-lisp.net/project/anaphora/), but the goal of `cl-web-dev` is to be self-contained.

#### with-gensyms

Thank you, [Peter Seibel](http://www.gigamonkeys.com/book/macros-defining-your-own.html). It's a macro used to assign multiple `gensym` bindings for defining macros.

#### aif

Thank you, [Paul Graham](http://dunsmor.com/lisp/onlisp/onlisp_18.html). The anaphoric `if` statement; you can use the symbol `it` in its clauses to refer to the result of the test expression.

#### awhen

Thank you again, Paul. The `when`-equivalent of `aif`.

#### with-overwrite

Basic macro that provides a particular series of default values to `with-open-file`. In particular, it ensures that the specified directories exist, automatically creates a nonexistant file, and automatically over-writes an existing one.

Takes a `stream`, a `filename` and a `body`. Executes `body` with `stream` outputting to `filename`.

#### to-file

Minimal interface layer to `with-overwrite`. 

Takes a file-name and a string. Writes the string to the named file.

## Hunchentoot-related

There are really only four things I ever do with Hunchentoot. Define handlers, define a static directory, poke at `session` and start the server. Very occasionally, I also stop the server. `cl-web-dev` re-exports `stop`, `session-start`, `session-value`, `delete-session-value` and `remove-session`, and defines the following to help with the rest

#### easy-start

Takes a port number, and optionally, a pathname to a static directory. Starts a `hunchentoot:easy-acceptor` listening on the port number, and serves up the static directory if specified.

#### define-handler

A thin wrapper over `define-easy-handler`. It assigns the handlers' `uri` based on its name to simplify definition slightly.

## cl-who-related

#### html-str

A shortcut providing reasonable defaults (and a shorter name) for `with-html-output-to-string`.

#### html

Same as `html-str`, but for `with-html-output`.

#### scripts

Takes a bunch of file names, and generates the `script` tags to serve them from your static directory.

#### styles

As `scripts`, but for stylesheets rather than javascript.

## parenscript

Mostly, these are tiny utility macros that provide a simple interface to [jQuery](http://jquery.com/) through `parenscript`. You're still expected to include `parenscript`, and serve up a `jquery.js` yourself. Maybe that could be made a bit simpler...

#### obj->string

Takes a JSON object and stringifies it.

#### string->obj

Takes a string and tries to parse it into a JSON object.

#### fn

Shorthand for the anonymous function of zero arguments.

#### $

Basic emulation of the jQuery selector. It doesn't do standalone jQuery functions. For instance, it won't do what you think if you try `($ (map (list 1 2 3) (lambda (a) (+ 1 a))))`. But it will do what you think if you try `($ "#test" (val))`.

#### $int

Tries to parse the value of the target element as an integer.

#### $float

Tries to parse the value of the target element as a float.

#### doc-ready

Shorthand for `($ document (ready (lambda () ...)))`.

#### $map

Interface to the jQuery `$.map`. Takes a `body` argument rather than a function. That body will be evaluated with the symbols `elem` and `i` bound to the appropriae values. 

Example:

    ($map (list 3 2 1) (list i elem))
    
will return `[[0 3] [1 2] [2 1]]`

#### $grep

Interface to the jQuery `$.grep`. Takes a body rather than a function just like `$map`.

#### $post

Interface to the `$.post` function. Takes a `uri`, an object of parameters and a `body`. There are four bound symbols in body:

- `data`, which refers to the raw return from the handler (doesn't seem to work in all browsers)
- `status` the HTTP response code
- `jqXHR`, the jquery response JSON object
- `res`, a parsed JSON object of the response text when possible

#### $highlight

Shorthand for `($ "#foo" (stop t t) (effect :highlight nil 500))`.

#### $droppable

Interface to `.droppable`.

Example:

    ($droppable board-selector
	(:card-in-hand 
	 (play ($ dropped (attr :id)) :up (@ event client-x) (@ event client-y) 0 0)))

This will run the code `(play ...` when a `draggable` with the class `card-in-hand` is dropped on it.

#### $draggable

Interface to `draggable`.

Example:

    ($draggable ".foo" () 
	(move (self id) (@ ui offset left) (@ ui offset top) 0 0))

This creates a draggable for the class `.foo`, and runs `(move...` when dragging stops.
