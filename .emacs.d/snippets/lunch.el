(defvar *locations*
  ["portillos"
   "chickfila"
   "chipotle"
   "Manderin Garden"
   "red robin"
   "noodles"
   "potbelly"
   "meat heads"
   "the rock"
   "Medici"
   "Moes"
   "Buffalo Wild wings"
   "Jasons Deli"
   "Braizes"
   "chilis"
   "jimmy johns"
   "great wall"
   "zen express"
   "tonys tacos"
   "Fort Jesse Cafe"])

(defun shuffle (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)."
   (let ((len (length vector)) j temp)
     (dotimes (i len vector)
       (setq j (+ i (random (- len i)))
             temp (aref vector i))
       (aset vector i (aref vector j))
       (aset vector j temp))))

(defun pick-3-locations ()
  "pick 3 lunch locations"
   (butlast
    (append (shuffle *locations*) nil)
    (- (length *locations*) 3)))

(defun create-lunch-poll ()
  "format choices into the slack poll, prints choices to buffer"
  (let ((choices-string (combine-and-quote-strings (pick-3-locations) ", ")))
    (message (format "chose: %S" choices-string))
    (format "/poll \"lunch\" %S" choices-string)))

(defun copy-lunch-poll ()
  "copy a lunch poll to the paste ring"
  (interactive)
  (kill-new (create-lunch-poll)))
