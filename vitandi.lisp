(defvar tickers)

(defun dawn-simulator() )

(defun sunset-simulator() )

(setf tickers '(dawn-simulator sunset-simulator))

(defun run-tickers ()
  "Executes each ticker in turn."
  (mapcar (lambda (ticker) (funcall ticker)) tickers))

(defun main-loop ()
  (loop
   (run-tickers)
   (sleep 1)))
