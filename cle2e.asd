;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage cle2e-system
  (:use :common-lisp :asdf))

(in-package :cle2e-system)


;; System Definition
(defsystem :cle2e

  :depends-on   ()

  :version      "2016.12"

  :author       "email@andreas-sommer.eu"

  :description  "E2E file reader for Heidelberg OCT files."

  :serial       t

  :components   ((:file "cle2e"))

  )




