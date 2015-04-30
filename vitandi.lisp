(in-package #:vitandi)

(defparameter current-scene nil)

(setq drakma:*header-stream* nil)
(setq drakma:*text-content-types* (cons '("application" . "json")
                                        drakma:*text-content-types*))

(defun lifx-parameters-from-scene (scene-name duration-s)
  "Gets a string containing PUT variables from a LIFX scene"
  (let* ((scene (getf scenes scene-name))
         (brightness (getf scene :brightness))
         (hue (getf scene :hue))
         (kelvin (getf scene :kelvin))
         (saturation (getf scene :saturation)))
    ;; TODO: there must be a better way of doing this ...
    (concatenate 'string
                 "power_on=true"
                 ";duration=" (write-to-string duration-s)
                 ";color=" (concatenate 'string
                                        "brightness:" (write-to-string brightness)
                                        " hue:" (write-to-string hue)
                                        " kelvin:" (write-to-string kelvin)
                                        " saturation:" (write-to-string saturation)))))

(defun get-all-lights-status ()
  "Gets the status of all LIFX light bulbs."
  (json:decode-json-from-string
   (drakma:http-request "https://api.lifx.com/v1beta1/lights/all"
                        :method :get
                        :basic-authorization `(,lifx-token ,lifx-password))))

(defun set-all-lights-to-scene (scene-name duration-s)
  "Sets all LIFX light bulbs to a particular scene, over a specified duration in seconds"
  (print (format t "set-all-lights-to-scene: setting to ~a over ~a seconds" scene-name duration-s))
  (drakma:http-request "https://api.lifx.com/v1beta1/lights/all/color"
                       :method :put
                       :content (lifx-parameters-from-scene scene-name duration-s)
                       :basic-authorization `(,lifx-token ,lifx-password)))

(defun lifx-scene-from-current-time ()
  (let ((hour (nth 2 (multiple-value-list (get-decoded-time)))))
    (case hour
      ((22 23 0 1 2 3 4) :night)
      ((5 6 7) :warm-daylight)
      (otherwise :off))))

(defun main-loop ()
  (loop
     (let ((desired-scene (lifx-scene-from-current-time)))
       (if (not (eq current-scene desired-scene))
           (progn
             (set-all-lights-to-scene desired-scene 3600)
             (setq current-scene desired-scene))))
     (sleep 60)))
