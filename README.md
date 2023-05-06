# quicklisp-apropos

Apropos across Quicklisp libraries.

WIP

Coming Soon.

Example of `apropos-function` with the query: "random string":

![apropos-random-string-example](apropos-random-string.png "Example result of apropos with 'random string' as query")

## quicklisp-apropos package functions

* **APROPOS**
  Function: Perform apropos QUERY across libraries in Quicklisp.
* **APROPOS-CLASS**
  Function: Perform apropos QUERY to match exported CLOS classes of Quicklisp libraries.
* **APROPOS-DOC**
  Function: Perform apropos QUERY to match in documentation of exported definitions of Quicklisp libraries.
* **APROPOS-FUNCTION**
  Function: Perform apropos QUERY to match exported functions of Quicklisp libraries.
* **APROPOS-GENERIC-FUNCTION**
  Function: Perform apropos QUERY to match exported CLOS generic functions of Quicklisp libraries.
* **APROPOS-MACRO**
  Function: Perform apropos QUERY to match exported macros of Quicklisp libraries.
* **APROPOS-NAME**
  Function: Perform apropos QUERY to match exported names of Quicklisp libraries.
* **APROPOS-PACKAGE**
  Function: Perform apropos QUERY on packages of Quicklisp libraries.
* **APROPOS-SYSTEM**
  Function: Perform apropos QUERY on ASDF systems of Quicklisp libraries.
* **APROPOS-VARIABLE**
  Function: Perform apropos QUERY to match exported variables of Quicklisp libraries.

## Emacs commands

* `quicklisp-apropos`
   Apropos quicklisp using a generic QUERY.
* `quicklisp-apropos-class`
   Search across CLOS classes exported in Quicklisp libraries that
   match the QUERY.
* `quicklisp-apropos-function`
   Search across Lisp functions exported in Quicklisp libraries that
   match the QUERY.
* `quicklisp-apropos-generic-function`
   Search across CLOS generic functions exported in Quicklisp
   libraries that match the QUERY.
* `quicklisp-apropos-macro`
   Search across Lisp macros exported in Quicklisp libraries that
   match the QUERY.
* `quicklisp-apropos-package`
   Search across Lisp packages in Quicklisp libraries that match the
   QUERY.
* `quicklisp-apropos-system`
   Search across ASDF systems in Quicklisp libraries that match the
   QUERY.
* `quicklisp-apropos-variable`
   Search across Lisp variables exported in Quicklisp libraries that
   match the QUERY.
* `quicklisp-apropos-update-index`
   Download and update quicklisp-apropos index.

