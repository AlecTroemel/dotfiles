(defvar locations [“portillos”
                “chickfila”
                “chipotle”
                “Manderin Garden”
                “red robin”
                “noodles”
                “potbelly”
                “meat heads”
                “the rock”
                “Medici”
                “Moes”
                “Buffalo Wild wings”
                “Jasons Deli”
                “Braizes”
                “chilis”
                “jimmy johns”
                “great wall”
                “zen express”
                “tonys tacos”
                 “Fort Jesse Cafe”])

(defun shuffle (vector)
  “Randomly permute the elements of VECTOR (all permutations equally likely).”
   (let ((len (length vector)) j temp)
     (dotimes (i len vector)
       (setq j (+ i (random (- len i)))
             temp (aref vector i))
       (aset vector i (aref vector j))
       (aset vector j temp))))

(defun pick-3 (vector)
  “pick 3 lunch locations”
   (butlast
    (append (shuffle vector) nil)
    (- (length vector) 3)))

(defun create-lunch-poll (vector)
 (format “/poll \“lunch\” %S”
          (combine-and-quote-strings (pick-3 vector) “, “)))

(create-lunch-poll locations)

(pick-3 locations)
