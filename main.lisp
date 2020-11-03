(ql:quickload 'trivia)
(ql:quickload 'drakma)
(ql:quickload 'zip)
(ql:quickload :str)

(use-package :trivia)

(require :sb-posix)

(defun echo-off ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun echo-on ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logior (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun to-assoc (l)
    (cons (read-from-string (car l)) (cadr l)))

(defun read-param (s)
    (to-assoc (mapcar #'str:trim (str:split "::" s :omit-nulls t))))

(defun get-params (filename)
    (let ((params (str:lines (file-get-contents filename))))
       (mapcar #'read-param params)))

(defun valid-params (p)
    (and (assoc 'DBPATH p) 
         (assoc 'FBPATH p) 
         (assoc 'PORT p)
         t))

(defun build-command (command-type &key user pass fbpath dbpath port output) 
    "returns a string that represents the command to be run
    to perform a validation"
    (let ((template (cond ((eq command-type 'VALIDATION) "~A/gfix.exe -user ~A -password ~A -v -full -n 127.0.0.1/~A:~A")  
                          ((eq command-type 'BACKUP)     "~A/gbak.exe -user ~A -password ~A 127.0.0.1/~A:~A ~A"))))
            (format t template    
                      fbpath             
                      user        
                      pass                  
                      dbpath 
                      port
                      output )))

(defun compress-file (file)
  'OK)

(defun send-file (file)
  'OK)

(defun run-command (command)
  (multiple-value-bind (out err exc) (uiop:run-program command :output :string)
   (list out err exc)))

(defun exec-bailing (commands)
  "executes a list of commands and stops at the first
  error it encounters, a command can be a string or a
  list containing a function and an argument"
  (if commands
    (cond ((stringp (car commands)) 
              (match (run-command (car commands))
                ((list _ _ 0) (exec-bailing (cdr commands)))
                ((list _ _ _) (list 'ERR (car commands)))))
          (t 
              (match (funcall (caar commands) (cdar commands))
                ('OK  (exec-bailing (cdr commands)))
                ('ERR (list 'ERR (car commands))))))
    'OK))

(defparameter *validate* "echo validating" )
(defparameter *backup*   "echo backuping"  )
(defparameter *compress* (list #'compress-file "filename") )
(defparameter *send*     (list #'send-file "filename"))


(defun main (args)
  (match args
         ((list user pass) (prin1 (exec-bailing (list
                                                      *validate*
                                                      *backup*
                                                      *compress*
                                                      *send*))))
         (_                (prin1 "debe ingresar un usuario y contraseña"))))

(defparameter params (get-params "params.conf"))

(defun get-input (prompt)
  (clear-input)
  (write-string prompt)
  (finish-output)
  (read-line))

(defvar username)
(defvar password)
(setq username (get-input "usuario:"))
(echo-off)
(setq password (get-input "contraseña:"))
(echo-on)
(print username)
