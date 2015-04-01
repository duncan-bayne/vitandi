(require :sb-posix)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '("drakma"
                "cl-json"))

(load "lifx-config.lisp")

;;(setq drakma:*header-stream* nil)
(setq drakma:*text-content-types* (cons '("application" . "json")
                                        drakma:*text-content-types*))

(defun get-all-lights-status ()
  "Gets the status of all LIFX light bulbs."
  (json:decode-json-from-string
   (drakma:http-request "https://api.lifx.com/v1beta1/lights/all"
                        :method :get
                        :basic-authorization `(,lifx-token ,lifx-password))))

(defun set-light-power (power id duration-s)
  "Sets a specific light to power on or off over a specified duration"
  (drakma:http-request (concatenate 'string "https://api.lifx.com/v1beta1/lights/id:" id "/power")
                       :method :put
                       :content (concatenate 'string "state=" power ";duration=" (write-to-string duration-s))
                       :basic-authorization `(,lifx-token ,lifx-password)))

(defun light-on-p (status)
  (equalp "on"
          (cdr (assoc :power status))))

(defun light-off-p (status)
  (equalp "off"
          (cdr (assoc :power status))))

(defun light-id-from-light-status (status)
  "Extracts the ID of a light from the status of an individual light."
  (cdr (assoc :id status)))

(defun set-all-lights-off (duration-s)
  (mapcar (lambda (status)
            (if (light-on-p status)
                (set-light-power "off" (light-id-from-light-status status) duration-s)))
          (get-all-lights-status)))

(defun set-all-lights-on (duration-s)
  (mapcar (lambda (status)
            (if (light-off-p status)
                (set-light-power "on" (light-id-from-light-status status) duration-s)))
          (get-all-lights-status)))

(defun dawn-simulator ()
  (let ((hour (nth 2 (multiple-value-list (get-decoded-time)))))
    (if (= hour 5)
        (progn
          (print "dawn")
          (set-all-lights-on (* 60 10))))))

(defun dusk-simulator ()
  (let ((hour (nth 2 (multiple-value-list (get-decoded-time)))))
    (if (= hour 21)
        (progn
          (print "dusk")
          (set-all-lights-off 60)))))

(defun run-tickers ()
  "Executes each ticker in turn."
  (dawn-simulator)
  (dusk-simulator))

(defun main-loop ()
  (loop
     (run-tickers)
     (sleep 60)))
