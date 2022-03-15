# CL-FORMS #

CL-FORMS is a web forms handling library for Common Lisp.

Although it is potentially framework agnostic, it runs on top of Hunchentoot at the moment.

It features:

* Several form field types: String, boolean, integer, email, password fields. And more.
* Custom fields. CL-FORMS is extensible and it is possible to define new field types.
* Server and client side validation
* Rendering backends. Forms can be rendered via CL-WHO, or Djula, or something else; the backend is pluggable. The default renderer is CL-WHO.
* Themes (like Bootstrap)
* Control on rendering and layout.
* Handling of form errors.
* CSRF protection

## Basics ##

Use `defform` to define a form. Example:

```lisp
(defform fields-form (:action "/fields-post")
  ((name :string :value "")
   (ready :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (submit :submit :label "Create")))
```

On your web handler, grab the form via `find-form`, select a renderer with `with-form-renderer`and then render the form with `render-form`:

```lisp
(let ((form (forms::find-form 'fields-form)))
   (forms:with-form-renderer :who
      (forms:render-form form))
```

To handle the form, grab it via `find-form` and then call `handle-request` (you should probably also call `validate-form` after). 
Then bind form fields via either `with-form-field-values`, that binds the form field values; or `with-form-fields` that binds the form fields.

```lisp
(let ((form (forms:find-form 'fields-form)))
    (forms::handle-request form)
    (forms::with-form-field-values (name ready sex) form
       (who:with-html-output (forms.who::*html*)
          (:ul
            (:li (who:fmt "Name: ~A" name))
            (:li (who:fmt "Ready: ~A" ready))
            (:li (who:fmt "Sex: ~A" sex))))))
```

Plase have a look at the demo sources for more examples of how to use the library

## DEMO ##

There's a demo included. To run:

```lisp
(require :cl-forms.demo)
(forms.test:run-demo)
```
