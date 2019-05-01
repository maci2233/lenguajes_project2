#lang racket/gui

(require data-frame)
(require racket/gui/base)
(require plot)
(plot-new-window? #t)

(define (add_money_days list_days days)
  (add_money_days_exe list_days days 1)
)

(define (add_money_days_exe list_days days aux)
  (cond
    [(> aux days) list_days]
    [else (add_money_days_exe (cons (new text-field% [parent frame][label (string-append "Day " (~v aux))]) list_days) days (+ aux 1))]
  )
)

(define (build_x_axis list days)
  (cond
    [(= days 0) list]
    [else (build_x_axis (cons days list) (- days 1))]
  )
)

(define (build_money_spent_acum list text_field_list acum)
  (cond
    [(empty? text_field_list) (reverse list)]
    [(equal? "" (send (first text_field_list) get-value)) (build_money_spent_acum (cons -50 list) (rest text_field_list) acum)]
    [else (build_money_spent_acum (cons (+ acum (string->number (send (first text_field_list) get-value))) list) (rest text_field_list) (+ acum (string->number (send (first text_field_list) get-value))))]
  )
)

(define (get_info money_acum_list)
  (get_info_exe money_acum_list 0 0)
)

(define (get_info_exe money_acum_list days_count money_acum)
  (cond
    [(< (first money_acum_list) 0) (cons days_count (cons money_acum empty))]
    [else (get_info_exe (rest money_acum_list) (+ days_count 1) (first money_acum_list))]
  )
)

(define (open_csv filename)
  (df-read/csv filename)
)

(define (load_info text_list)
  (define df (open_csv "money.txt"))
  (define loaded_data (string-split(first (df-series-names df))))
  (load_info_exe loaded_data text_list)
)

(define (load_info_exe info_list text_list)
  (cond
    [(empty? info_list) text_list]
    [else (begin
            (send (first text_list) set-value (first info_list))
            (load_info_exe (rest info_list) (rest text_list))
          )]
  )
)

(define (get_money_days_string string_list text_field_list)
  (cond
    [(equal? "" (send (first text_field_list) get-value)) string_list]
    [else (get_money_days_string (cons (send (first text_field_list) get-value) string_list) (rest text_field_list))]
  )
)

(define (save_info text_list)
  (define new_df (make-data-frame))
  (define new_data_list (list (string-join (reverse (get_money_days_string '() text_list)) " ")))
  (define new_data_vector (list->vector new_data_list))
  (define new_data (make-series (first new_data_list) #:data new_data_vector))
  (df-add-series new_df new_data)
  (df-write/csv new_df "money.txt")
)

(define total_days 15)

; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Example"]
                   [width 500]
                   [height 800]
                   ))

(define money_total (new text-field%
                         [parent frame]
                         [label (string-append "Total money earned in " (~v total_days) " days")]
                         ;[horiz-margin 20]
                         ;[vert-margin 20]
                         ))

(define money_days (add_money_days '() total_days))
(define x_days (build_x_axis '() total_days))
;(define info_msg (new message% [parent frame] [label "prueba"]))

(define plot_btn(new button%
                     [parent frame]
                     [label "Plot"]
                     [callback (lambda (button event)
                                 (define money_total_value (string->number (send money_total get-value)))
                                 (define avg_money_day (/ money_total_value total_days))
                                 (define money_spent_acum (build_money_spent_acum '() (reverse money_days) 0))
                                 (define info_list (get_info money_spent_acum))
                                 (define passed_days (first info_list))
                                 (define curr_spent (first (rest info_list)))
                                 (define new_avg_money_day (floor (exact->inexact (/ (- money_total_value curr_spent) (- total_days passed_days)))))
                                 (plot (list
                                        (function (lambda (x) (* x avg_money_day)) #:label "Average")
                                        (points (map vector x_days money_spent_acum) #:label "Real")
                                       )
                                       #:x-min 0 #:x-max 15 #:y-min 0
                                 )
                                 ;(send info_msg set-label (string-append "\nRELEVANT INFORMATION\nYou have spent a total amout of $" (~v curr_spent)
                                 ;                                     " (pesos) in " (~v passed_days) " days.\nIf you want to survive, you can't spend more than\n$" (~v new_avg_money_day)
                                 ;                                     " (pesos) a day for the next " (~v (- total_days passed_days)) " days."))
                                 (new message% [parent frame] [label (string-append "\nRELEVANT INFORMATION\nYou have spent a total amout of $" (~v curr_spent)
                                                                      " (pesos) in " (~v passed_days) " day(s).\nIf you want to survive, you can't spend more than\n$" (~v new_avg_money_day)
                                                                      " (pesos) a day for the next " (~v (- total_days passed_days)) " day(s).")])
                               )]
                 ))




(define load_btn(new button%
                     [parent frame]
                     [label "Load"]
                     [callback (lambda (button event)
                                 (load_info (cons money_total (reverse money_days)))
                               )]
                 ))

(define save_btn(new button%
                     [parent frame]
                     [label "Save"]
                     [callback (lambda (button event)
                                 (save_info (cons money_total (reverse money_days)))
                               )]
                 ))


; Show the frame by calling its show method
(send frame show #t)