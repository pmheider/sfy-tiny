(defvar *lkb-directory* "/projects/pmheider/delphin/lkb/")
(defvar *grammar-directory* "/projects/pmheider/delphin/erg_567/")
(defvar *sfy-directory* "/projects/pmheider/snalps/")
(defvar *load-sneps-command* "/projects/snwiz/bin/sneps")

(pushnew :tty *features*)

(load (concatenate 'string *lkb-directory*
		   "src/general/loadup"))

(pushnew :lkb *features*)

(load-system :lkb)

(lkb::read-script-file-aux (concatenate 'string *grammar-directory*
					"lkb/script"))

(setf lkb::*show-parse-p* nil) 

(setf *sfy-parse-mode* "nested-lists")

(setf *sfy-quiet-parse* t)

(if (not (find-package 'snepslog))
    (load *load-sneps-command*))
