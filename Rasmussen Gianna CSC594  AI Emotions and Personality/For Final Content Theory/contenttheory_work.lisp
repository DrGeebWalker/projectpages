
;;;whattosay game play with bob sample
;;;needs to show profile to get next option
(setf *profiles* '(gia bob))

;;;completes the sequence 
(setf *objects* '(gum-wrapper dictionary))

;;;build off these later
(defparameter *map* '((whattosay (You see Bob hanging out all alone))))


(defparameter *object-locations* '((gum-wrapper whattosay)
                                   (dictionary whattosay)))

(defparameter *profile-locations* '((gia whattosay)
                                    (bob whattosay)))

(defparameter *location* 'whattosay)


;;;add to your accessible profiles
(defun add-to-profiles (object)
  (push (list object 'profile-body) *profile-locations*))

;;;add to accessible inventory
(defun add-to-inventory (object)
  (push (list object 'inventory-body) *object-locations*))

(defmacro defspel (&rest rest) `(defmacro ,@rest))

(defspel add-profile (object)
  `(add-to-profiles ',object))

(defspel add-item (object)
  `(add-to-inventory ',object))

;;;mainly usede for showing items profiles on you
(defun is-at (obj loc obj-loc)
  (eq (second (assoc obj obj-loc)) loc))

;;;displays profiles obtained
(defun collected-profiles ()
  (remove-if-not (lambda (x)
                   (is-at x 'profile-body *profile-locations*)) 
                 *profiles*)
  )

(defun show-profiles ()
  (princ "Which profile would you like to select?")
  (loop for prof in (collected-profiles)
        do (print prof))
  
  )

;;;shows inventory
(defun inventory ()
  (remove-if-not (lambda (x)
                   (is-at x 'inventory-body *object-locations*))
                 *objects*)
  )

;;;presents item
(defun present-item ()
  (princ "Which item would you like to present?")
  (loop for item in (inventory)
        do (print item))
  )

;;;have you completed whattosay
(defparameter *completed-whattosay* nil)

  ;;;Bob name 
  (setf *bob-name* `(????))
  
  ;;;have you spoken to bob yet?
  (defparameter *spoken-to* nil)
  
  ;;;have you shown bob your profile?
  (defparameter *shown-profile* nil)
  
  ;;;starter dialogue 
  (setf *current-dialogue* `(Hey! How are you doing?))
  
  ;;;Dialgue at the end of either talking choices
  (setf *bob-refusal* `(I can't let you through. My program is still downloading.))
  
  (setf *bob-introduction* `(I am Bob!))
  
  ;;;you show bob his own profile
  (setf *bob-affirmation* `(I am Bob!))
  
  ;;;you show bob your profile
  (setf *bob-doubt* `(I am Bob! You are Gia!))
  
  ;;;show bob your badge
  (setf *bob-agreement* `(Processing complete! Thank you for the help!))
  
  ;;;go back option.
  ;;;available in talk, show profile, and present item
  (setf *go-back* `(Back to options))
  
    ;;;show options
  (defun chat-options ()
    (print "talk")
    (if (not (null *spoken-to*))
        (print "show-profile"))
    (if (not (null *shown-profile*))
        (print "present-item"))
    (print "exit-conversation"))
  
  ;;;dialogue options for whattosay
  (setf *option1* `(I want to get through.))
  (setf *option2* `(Move. I need to go through here.))
  (setf *option3* `(Who are you?))
  
  ;;;presents options
  ;;;in the web interface the options are clickable
  (defun talk ()
    (princ "Option 1: ")
    (princ *option1*)
    
    (print "Option 2: ")
    (princ *option2*)
    
    (print "Option 3: ")
    (princ *option3*)
    
    (print *go-back*)
    
    (terpri)
    (setf dialogue-choice (read))
    
    (cond ((string-equal dialogue-choice "option1") (talk-response1 *option1*))
          ((string-equal dialogue-choice "option2") (talk-response1 *option2*))
          ((string-equal dialogue-choice "option3") (talk-response2))
          (t `(Okay.)))
    
    (run-whattosay)
    
    )
  
  (defun talk-response1 (option)
    (princ "Gia: ")
    (princ option)
    (terpri)
    (princ *bob-name*)
    (princ ": ")
    (princ *bob-refusal*)
    (setf *current-dialogue* *bob-refusal*)
    
    (terpri)
    (princ "Gia: I guess asking to see the body is not enough.")
    )
  
  
  (defun talk-response2 ()
    (princ "Gia: ")
    (princ *option3*)
    (setf *bob-name* `(Bob))
    (terpri)
    (princ *bob-name*)
    (princ ": ")
    (princ *bob-introduction*)
    (setf *current-dialogue* *bob-introduction*)
    
    (if (null *spoken-to*)
        (progn
          (setf *spoken-to* t)
          
          (add-profile gia)
          (add-profile bob-profile)
          
          (print "Bob added to your profiles")
          (print "Gia: (Maybe if I show him a profile I can convince him to let me through.)")
         )
      (print "I should try showing Bob one of my profiles to see if that convinces him to let me through.")
      )
    )
  
  ;;;when you choose to show profile
  (defun show-profiles-whattosay ()
    (show-profiles)
    (print "go back")
    (terpri)
    (setq profile-choice (read))
    (cond ((string-equal profile-choice "gia") (show-gia))
          ((string-equal profile-choice "bob") (show-bob))
          (t '(Okay))
          )
    
    (run-whattosay)
    )
  
  
  
  (defun show-gia ()
    (princ "Gia: I am Gia, please let me through.")
    (terpri)
    (princ *bob-name*)
    (princ ": ")
    (princ *bob-doubt*)
    (setf *current-dialogue* *bob-doubt*)
    (terpri)
    (princ "Gia: I should get him a dictionary!")
    
    (add-item gum-wrapper)
    (add-item dictionary)
    (setf *shown-profile* t)
    
    (print "You can now access your inventory")
    )
  
  (defun show-bob ()
    (princ "Gia: So you are a robot named Bob?")
    (terpri)
    (princ "Bob: ")
    (princ *bob-affirmation*)
        (setf *current-dialogue* *bob-affirmation*)
    (terpri)
    (princ "Gia: (Drats. Maybe he needs something to learn from?)")

    )
  
  
  
  ;;;when you choose to present an item
  (defun present-item-whattosay ()
    (present-item)
    (print "go back")
    (terpri)
    
    (setq item-choice (read))
    (cond ((string-equal item-choice "gum-wrapper") (give-wrapper))
          ((string-equal item-choice "dictionary") (present-dictionary))
          (t `(Okay.)))
    
    (run-whattosay)
    )
  
  
  ;;;give gum-wrapper
  (defun give-wrapper ()
    (princ "Detective Hughman: Check this out!")
    (terpri)
    (princ "Bob: A gum wrapper? I cannot eat.")
    (terpri)
    (princ "Gia: (He didnt even read the joke on the back! This guy is tough.")
    (terpri)

    )
  
  
  ;;;display your dictionary
  (defun present-dictionary ()
    (princ "Gia: I have a dictionary!")
    (terpri)
    (princ "Bob: ")
    (princ *bob-agreement*)
    (setf *current-dialogue* *bob-agreement*)
    (setf *completed-whattosay* t)
    (terpri)
    (princ "Gia: (Pretty sure I did say that...)")
    (terpri)
    )
  
     ;;;prints Bob's last sentance and player options
  (defun run-whattosay()
    (print *current-dialogue*)
    (chat-options)
    (terpri)
    (setq choice (read))
    (cond ((string-equal choice "talk") (talk))
          ((string-equal choice "show-profile") (show-profiles-whattosay))
          ((string-equal choice "present-item") (present-item-whattosay))
          (t `(I mean this will be improssible to choose later)))
    )


    
    
    
    
    
    