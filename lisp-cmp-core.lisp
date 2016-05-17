(js-mac mac a `(js-mac ,@a))
(mac exe a `(js-exe ,@a))
(mac exen a `(exe ,@a nil))

(mac defn a `(exen (def ,@a)))
(mac macn a `(exen (mac ,@a)))

(js-mac smac a `(js-smac ,@a))

(smac nil 'js-nil)

(mac by (n nm op)
  `(mac ,nm #g
     `(do ,@(mapn `(,,op ,@_) (grp #g ,n)))))

(mac macby (nm ag . bd)
  `(do (mac #g ,ag ,@bd)
       (by ,(len ag) ,nm #g)))

(macby alias (new old)
  `(mac ,new #args `(,,old ,@#args)))

(mac jsali a
  `(alias ,@(mapnapp `(,_ ,(app 'js- _)) a)))

(jsali call
       var fn rfn def new
       if break cont
       ret throw nret
       mmac mblock smblock qt
       do cdo1 arr)

(mmac dtfn a (x . args)
  `((. ,x ,@a) ,@args))

(mac amblock a
  `(mblock (smblock (mmblock ,@a))))

