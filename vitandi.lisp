(in-package #:vitandi)

(defparameter current-scene nil)

(setq drakma:*header-stream* nil)
(setq drakma:*text-content-types* (cons '("application" . "json")
                                        drakma:*text-content-types*))

(defun weekday-p (day-of-week)
  (< day-of-week 5))

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

(defun check-all-lights-status ()
  "Gets the status of all LIFX light bulbs, and emails alerts if any are offline"
  (json:decode-json-from-string
   (drakma:http-request "https://api.lifx.com/v1beta1/lights/all"
                        :method :get
                        :basic-authorization `(,lifx-token ,lifx-password))))

(defun offline-p (result)
  "Returns T if a particular light is offline, NIL otherwise"
  (equalp "offline" (cdr (assoc :STATUS result))))

(defun send-alert (result)
  "Alerts the user that a LIFX API operation has failed"
  (format t "LIFX API request has failed: ~a~%" result))

(defun set-all-lights-to-scene (scene-name duration-s)
  "Sets all LIFX light bulbs to a particular scene, over a specified duration in seconds"
  (format t "set-all-lights-to-scene: setting to ~a over ~a seconds~%" scene-name duration-s)
  (json:decode-json-from-string
   (drakma:http-request "https://api.lifx.com/v1beta1/lights/all/color"
                        :method :put
                        :content (lifx-parameters-from-scene scene-name duration-s)
                        :basic-authorization `(,lifx-token ,lifx-password))))

(defun lifx-scene-from-current-time ()
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (if (weekday-p day-of-week)
        (case hour
          ((21 23 0 1 2 3) :night)
          ((4 5 6 7 17 18 19 20) :warm-daylight)
          (otherwise :off))
        (case hour
          ((22 23 0 1 2 3 4 5 6) :night)
          ((7 8 9 17 18 19 20 21) :warm-daylight)
          (otherwise :off)))))

(defun main-loop ()
  (loop
     (let ((desired-scene (lifx-scene-from-current-time)))
       (if (not (eq current-scene desired-scene))
           (let ((result (set-all-lights-to-scene desired-scene 3600)))
             (if (some #'offline-p result)
                 (send-alert result)
                 (setq current-scene desired-scene)))))
     (sleep 60)))
