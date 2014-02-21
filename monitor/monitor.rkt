(require racket/include)
(require version/utils)
(include "database.rkt")

(cond
 [(version<? (version) "6.0") (error "Requires Racket 6.0 or later.")])
