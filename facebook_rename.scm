(use-modules
  (srfi srfi-1))

(weechat:register "FacebookRename" "wes" "0.1" "None" "Rename facebook users" "" "")

(define (get-upto strs pred)
  (cond
    ((pred (string-take (car strs) 1)) (list (string-drop (car strs) 1)))
    (else
      (cons
        (format "-~a" (car strs))
        (get-upto (cdr strs) pred)))))


(define parse-nick (lambda (nick)
    (apply string-append
      (reverse
        (get-upto nick (lambda (x) (equal? ":" x)))))))


(define realnames (make-hash-table 100))

(define buffer
  (weechat:buffer_search
    "irc"
    "localhost.&bitlbee"))

(define print (lambda (x) (weechat:print buffer x)))

(define (update-nick! nick realname)
  (print
    (format "~a is ~a"
            nick
            realname))
  (weechat:command
    buffer
    (format "/msg ~a rename ~a ~a"
            "&bitlbee" nick realname)))

(define (whois-callback data signal signal-data)
  (let* ((splitted (string-split signal-data #\space))
         (nick (cadddr splitted))
         (reversed (reverse splitted))
         (realname (parse-nick reversed)))
    (hash-set! realnames nick realname)
    (if (not
          (equal? realname nick))
      (update-nick! nick realname))
    weechat:WEECHAT_RC_OK))

(define (join-callback data signal signal-data)
  (let* ((parsed (string-split signal-data #\,))
         (buf (car parsed))
         (nick (cadr parsed))
         (realname (hash-ref realnames nick #f)))
    (cond
      ((not (equal? buf buffer)) weechat:WEECHAT_RC_OK)
      (realname weechat:WEECHAT_RC_OK)
      (else
          (weechat:command buffer
                     (format "/whois ~a localhost" nick))
            weechat:WEECHAT_RC_OK))))


(weechat:hook_signal "localhost,irc_in_311" "whois-callback" "")

(weechat:hook_signal "nicklist_nick_added" "join-callback" "")
