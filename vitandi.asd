(asdf:defsystem #:vitandi
  :description "A smart house co-ordinator."
  :author "Duncan Bayne <duncan@bayne.id.au>"
  :license "GNU Lesser General Public License 3.0"
  :depends-on (#:drakma
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "vitandi")
               (:file "scenes")
               (:file "lifx-config")))

