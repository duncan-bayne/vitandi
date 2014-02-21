(require db)

(cond
 [(not sqlite3-available?) (error "Unable to find the native SQLite3 client.  Aborting.")])

 ;; (define database (sqlite3-connect #:database #:mode 'create))
