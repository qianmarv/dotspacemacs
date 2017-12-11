;;; ABAP-MODE
;; Copyright (C) 2016 Marvin Qian (qianmarv@gmail.om)
;; Inspired by abap-mode from VinceZK (Vincent Zhang)
;; Features
;; - Syntax highlight
;; - Indent
;;
;;==============================================================================
;; Special Thanks
;;
;; This program is developed on the basis of abap-mode developed by:
;;   hugo-dc (https://github.com/hugo-dc/abap-mode)
;;
;; And is inspired by:
;;   Vincent.Zhang <vincent.zhang@sap.com>
;;==============================================================================

;;; Code:

;; define keywords

(setq abap-keywords '("REPORT" "DATA" "DATA:" "TYPE" "TYPES" "TABLES" "AT" "BEGIN" "OF" "TIMES" "PERFORM" "APPEND" "CLEAR" "TO" "CALL" "FUNCTION" "EXPORTING" "EXCEPTIONS" "SELECT" "UP" "FROM" "INTO" "CORRESPONDING" "FIELDS" "TABLE" "GT" "LT" "EQ" "LE" "GE" "INSERT" "INTO" "MODIFY" "WHEN" "USING" "LIKE" "CHANGING" "TYPE-POOLS" "ROWS" "INITIAL" "SIZE" "WITH" "HEADER" "LINE" "LINES" "WRITE" "ASSIGNING" "READ" "IMPORT" "EXPORT"  "IMPORTING" "PUBLIC" "FINAL" "DEFINITION" "CREATE PUBLIC" "PUBLIC SECTION" "CLASS-METHODS" "PROTECTED SECTION" "PRIVATE SECTION" "METHODS" "CONSTANTS" "VALUE"))

(setq abap-keywords-open '("IF" "ELSE" "LOOP" "DO" "FORM" "CASE" "CLASS" "TRY" "CATCH" "METHOD"))

(setq abap-keywords-close '("ENDIF" "ENDCLASS" "ENDMETHOD" "ENDTRY" "END" "ENDLOOP" "ENDFORM" "ENDCASE" "ENDDO"))

(setq abap-keywords (append abap-keywords-open abap-keywords-close abap-keywords))


;; (setq abap-keywords '("REPORT" "DATA" "DATA:" "TYPE" "TYPES" "IF" "ELSE" "ENDIF" "LOOP" "TABLES" "AT" "BEGIN" "OF" "END" "ENDLOOP" "DO" "TIMES" "ENDDO" "PERFORM" "FORM" "APPEND" "CLEAR" "TO" "ENDFORM" "CALL" "FUNCTION" "EXPORTING" "EXCEPTIONS" "SELECT" "UP" "FROM" "INTO" "CORRESPONDING" "FIELDS" "TABLE" "GT" "LT" "EQ" "LE" "GE" "INSERT" "INTO" "MODIFY" "CASE" "WHEN" "USING" "LIKE" "CHANGING" "ENDCASE" "TYPE-POOLS" "ROWS" "INITIAL" "SIZE" "WITH" "HEADER" "LINE" "LINES" "WRITE" "ASSIGNING" "READ" "IMPORT" "EXPORT"  "IMPORTING" "CLASS" "PUBLIC" "FINAL" "DEFINITION" "CREATE PUBLIC" "PUBLIC SECTION" "CLASS-METHODS" "PROTECTED SECTION" "PRIVATE SECTION" "METHODS" "ENDCLASS" "ENDMETHOD" "TRY" "CATCH" "ENDTRY" "METHOD" "CONSTANTS" "VALUE"))

(setq abap-types    '("C" "I" "F" "STRING" "X" "XSTRING") )
(setq abap-constants '("SPACE" "SY-" ))
(setq abap-events    '("START-OF-SELECTION" "AT SELECTION-SCREEN"))
(setq abap-functions '("STRLEN" "CONCATENATE" "SPLIT"))

;; Generate regex string for each category
(setq abap-keywords-regexp  ( regexp-opt abap-keywords  'words))
(setq abap-type-regexp      ( regexp-opt abap-types     'words))
(setq abap-constants-regexp ( regexp-opt abap-constants 'words))
(setq abap-event-regexp     ( regexp-opt abap-events    'words))
(setq abap-functions-regexp ( regexp-opt abap-functions 'words))

;; create the list for font-lock
(setq abap-font-lock-keywords
      `(
        (,abap-constants-regexp . font-lock-constant-face)
        (,abap-event-regexp     . font-lock-builtin-face)
        (,abap-functions-regexp . font-lock-function-name-face)
        (,abap-keywords-regexp  . font-lock-keyword-face)
        (,abap-type-regexp      . font-lock-type-face)
        ;; Order above matters, in general longer words first
        ))

;; (setq abap-font-lock-keywords
;;       `(
;;         (,abap-constants . font-lock-constant-face)
;;         (,abap-event     . font-lock-builtin-face)
;;         (,abap-functions . font-lock-function-name-face)
;;         (,abap-keywords  . font-lock-keyword-face)
;;         (,abap-type      . font-lock-type-face)
;;         ;; Order above matters, in general longer words first
;;         ))

(defun delete-leading-space()
  " Delete leading SPACE / TAB"
  (let ((end (progn
               (back-to-indentation)
               (point)))
        (beg (progn
               (move-beginning-of-line nil)
               (point))))
    (delete-region beg end)
    )
  )

(defun is-line-empty()
  "Check space line"
  ;; (beginning-of-line)
  (back-to-indentation)
  (if (looking-at "$")
      t
    nil
    ))

(defun goto-previous-non-empty-line()
  "goto previous non empty line"
  (previous-line)
  (if (and (not (= 1 (point)))
           (is-line-empty))
      (goto-previous-non-empty-line)
    ))

(defun get-previous-line-width ()
  "Get width of previous non empty line"
  (save-excursion
    (goto-previous-non-empty-line)
    (current-column)))

(defun calc-indent ()
  "Get width of previous non empty line"
  (save-excursion
    (back-to-indentation)
    ;; (beginning-of-line)
    ;; Close
    (let ((offset (if (looking-at (regexp-opt abap-keywords-close 'words))
                      (* -1 tab-width)
                    0)))
      (goto-previous-non-empty-line)
      (if (looking-at (regexp-opt abap-keywords-open 'words))
      ;; (if (looking-at "METHOD ")
          (+ (current-column) tab-width offset)
        (+ (current-column) offset)
        )
      )))

(defun abap-indent-line ()
  "Indent ABAP Line"
  (let (
        (width tab-width)
        (indent (calc-indent)))
    ;; (save-excursion
    (delete-leading-space)
    (indent-to indent)))

;;;###autoload
(define-derived-mode abap-mode prog-mode
  "ABAP Mode"
  ;; Code for syntax highlighting
  (setq-local font-lock-defaults '(abap-font-lock-keywords nil t))
  (setq-local indent-line-function 'abap-indent-line)
  (setq-local comment-start "* ")
  (setq-local comment-style "plain")

  "Major mode for the ABAP Programming Language"
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?\" "<")
  (modify-syntax-entry ?*  "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?_  "w")
  (modify-syntax-entry ?|  "\"")
  )

;; clear memory
(setq abap-keywords nil)
(setq abap-types    nil)
(setq abap-constants nil)
(setq abap-events    nil)
(setq abap-functions nil)

(setq abap-keywords-regexp nil)
(setq abap-type-regexp    nil)
(setq abap-constants-regexp nil)
(setq abap-event-regexp    nil)
(setq abap-functions-regexp nil)

;; add the mode to the list
(provide 'abap-mode)

;; Local Variables:
;; coding: utf-8
;; End:
