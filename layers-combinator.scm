(define (script-fu-layers-combinator prefix separator useInnerFolder isSilent)

  ; strings
  (define (strings-join delimiter list)
    (let strings-join ((current "") (values list))
      (if (null? values)
          current
          (strings-join (string-append current delimiter (car values)) (cdr values)))
      )
    )

  (define (string-rest line)
    (substring line 1 (string-length line)))

  (define (last-char string)
    (string-ref string (- (string-length string) 1)))

  (define (string-split delimiter line)
    (let string-split ((result '()) (currentSeg "") (delimiter delimiter) (line line))

      (if (equal? "" line)
          (append result (list currentSeg))

          (let (
                (currentChar (string-ref line 0))
                )

            (if (equal? delimiter currentChar)
                (string-split (append result (list currentSeg)) "" delimiter (string-rest line))
                (string-split result (string-append currentSeg (string currentChar)) delimiter (string-rest line))          
                ))    
          ))    
    )

  ; (string-remove-after "1.test" #\.) -> "1"
  ; (string-remove-after "1.2.3" #\.) -> "1.2"
  ; (string-remove-after "1" #\.) -> "1"
  (define (string-remove-after name separator)

    (let iterate ((left name) (original name))
      (if (equal? (string-length left) 0)
          original        
          (if (equal? (last-char left) separator)
              (substring left 0 (- (string-length left) 1))
              (iterate (substring left 0 (- (string-length left) 1)) original)
              )
          )))

  ;
  (define (++ a)
    (+ 1 a)
    )

  (define (-- a)
    (- a 1)
    )

  (define (push stack value)
    (append (list value) stack)
    )

  (define (replace stack val)
    (push (cdr stack) val))


  (define (true? val)  
    (or (equal? #t val) (equal? TRUE val))
  )

  (define (false? val)
    (not (true? val)))

  (define (for-each list func)
    (let loop ((rest list))
      (unless (null? rest)
        (func (car rest))
        (loop (cdr rest))
        )
      )
    )

  (define (list-count val)
    (let list-count ((result 0) (tail val))
      (if (null? tail)
          result
          (list-count (++ result) (cdr tail))
          )
      )
    )

  (define (drop-last input)
    (let drop-last ((head '()) (tail input))
      (if (null? (cdr tail)) 
          head
          (drop-last (append head (list (car tail))) (cdr tail))    
          )    
      )
    )

  (define (filter func values)
    (let filter ((func func) (values values) (result '()))      
      (if (null? values)
          result
          (if (true? (func (car values)))
              (filter func (cdr values) (append result (list (car values))))
              (filter func (cdr values) result)
              )
      
          )  
      )
    )   

  ;options 

  ;line: string like "some_random_text[key1=val1 key2=val2]"
  ;result: string like "key1=val1 key2=val2"
  (define (get-options-text-segment line)
    (let get-options-text-segment ((result "") (isOpen #f) (line line))
      (if (equal? "" line)
          result
          (let (
                (currentChar (string-ref line 0))
                )
            (if (not isOpen)
                (get-options-text-segment result (equal? #\[ currentChar) (string-rest line))

                (if (equal? #\] currentChar)
                    result

                    (get-options-text-segment (string-append result (string currentChar)) #t (string-rest line))
                    )           
                ))
          )
      )
  )

  (define (string->option val)
    (cond
      ((equal? "min" val) 'min)
      ((equal? "max" val) 'max)
      ((equal? "combine" val) 'combine)
      (error (string-append "Unknown option " val))
      )    
    )

  ; options: list of "key=value" strings
  ; result: list of splitten options i.e. '(('min "val1") ('max "val2"))
  (define (get-options options)

    (let get-options ((result '()) (options options))
      (if (null? options)
          result

          (let (
                (words (string-split #\= (car options)))
                )
            (get-options (append result (list (list (string->option (list-ref words 0)) (list-ref words 1)))) (cdr options))
            )
      
          )
      )
    )

  (define (get-options-default)
    '((min "1")
      (max "*")
      (combine "seq"))
    )

  (define (option-get key set)

    (if (equal? key (list-ref (car set) 0))
        (car set)
        (option-get key (cdr set))
        )  
    )

  (define (option-get-value key set)

    (list-ref (option-get key set) 1)
    
    )

  (define (option-remove key set)
    (let option-remove ((key key) (set set) (result '()))
      (if (null? set)
          result
          
          (if (equal? key (list-ref (car set) 0))
              (option-remove key (cdr set) result)
              (option-remove key (cdr set) (append result (list (car set)))))
      
          )
      )    
    )

  (define (option-change option set)
    (append (option-remove (list-ref option 0) set) (list option))
    )

  (define (options-override set newSet)
    (if (null? newSet)
        set
        (options-override (option-change (car newSet) set) (cdr newSet))
        ))

  (define (options-from-name name)

    (let ((segment (get-options-text-segment name)))
      (if (equal? "" segment)
          (get-options-default)

          (options-override
          (get-options-default)
          (get-options (string-split #\space segment))
          )
          ))
    )    

  (define (set-valid? set option)
    (let (
          (length (list-count set))
          )
      (if (< length (string->number (option-get-value 'min option)))
          #f

          (let ((maxValue (option-get-value 'max option)))

            (if (equal? "*" maxValue)
                #t
                (if (> length (string->number (option-get-value 'max option)))
                    #f
                    #t)
            
                )       
            ))
      )
    )    

  ;iterators
  (define (iterate to groups)
    (let iterate ((from 0) (to to) (result '()))

      (if (= from to)
        result          

        (let (
          (currentLayer (list-ref groups from))
        )
          (iterate (++ from) to (push result (list-ref groups from))))      
      )        
    )
  )

  (define (iterate-all amount group)
    (let iterate-all ((from 0) (to amount) (direction #t) (set '()) (picks '(#t)) (result '()))
      
      (if (and (true? direction) (<= (++ from) to))
          (let (
                (set (append set (list (list-ref group from))))
              )                
            (iterate-all (++ from) to #t set (push picks #t) (push result set))      
          )

          (if (false? direction)
              (if (true? (car picks)) 
                  (iterate-all (++ from) to #t set (push (replace picks #f) #t) result)
                  (iterate-all (-- from) to #f set (cdr picks) result))
              (if (= from to)
                  (if (equal? set '())
                      result
                      (if (true? (car picks))        
                          (iterate-all (-- from) to #f (drop-last set) (cdr picks) result)
                          (iterate-all (-- from) to #f set (cdr picks) result)))
                  (display "")
                  )
              )
          )
      )
    )

  (define (combine-list list1 list2)
    (let combine-list ((set '()) (olist2 list2) (list1 list1) (list2 list2))

      (if (null? olist2)
          (if (null? list1)
              set
              (combine-list (push set (car list1)) olist2 (cdr list1) list2)
            )
          (if (null? list1)
              set
              (if (null? list2)
                  (combine-list set olist2 (cdr list1) olist2)
                  (combine-list (push set (append (car list2) (car list1))) olist2 list1 (cdr list2))
                  )
              )
          )
      )
  )  

  (define (combine-groups result groups)
    (if (null? groups)
        result
        (combine-groups (combine-list (list-ref (car groups) 2) result) (cdr groups))
    )
  )

  (define (iterate-all-groups result amount groups)
    
    (if (< (-- amount) 0)
        result
        (let* (
              (currentGroup (car groups))
              (options (options-from-name (list-ref currentGroup 1)))
              (set-valid-for-option? (lambda (set) (set-valid? set options)))
              )
          (if (equal? "mix" (option-get-value 'combine options))
              (iterate-all-groups (append result
                                          (list(list
                                                (list-ref currentGroup 0)
                                                (list-ref currentGroup 1)
                                                (filter set-valid-for-option? (iterate-all (list-ref currentGroup 2) (list-ref currentGroup 3)))))
                                          ) (-- amount) (cdr groups))
              (iterate-all-groups (append result
                                          (list(list
                                                (list-ref currentGroup 0)
                                                (list-ref currentGroup 1)
                                                (filter set-valid-for-option? (map list (iterate (list-ref currentGroup 2) (list-ref currentGroup 3))))))
                                          ) (-- amount) (cdr groups))
              ))
        )
    )   

  ; gimp-specific
  (define (get-gimp-groups image)

    (let get-gimp-groups ((result '()) (tail (vector->list (cadr (gimp-image-get-layers image)))))
      (if (null? tail)
        result
        (let* (
          (currentGroupIndex (car tail))
          (currentGroupName (car (gimp-item-get-name currentGroupIndex)))
          (currentGroupChildrens (filter is-visible? (vector->list (cadr (gimp-item-get-children currentGroupIndex)))))
          (currentGroupItemsCount (list-count currentGroupChildrens))
        )        
          (get-gimp-groups 
            (append result (list (list currentGroupIndex currentGroupName currentGroupItemsCount currentGroupChildrens)))
            (cdr tail)
          ))        
      )      
    )
  )

  (define (get-original-layer-name layerNumber)
    (car (gimp-item-get-name layerNumber))
  )

  (define (is-visible? layerNumber)  
    (equal? 1 (car (gimp-item-get-visible layerNumber)))
  )

  (define (get-group-name-components group)
       
    (let get-group-name-components ((result '()) (suffix '()) (group group))
      (if (null? group)
        (append result suffix)

        (let (
          (layerName (get-original-layer-name (car group)))
        )
          (if (equal? #\^ (last-char layerName))
              (get-group-name-components 
                                          result 
                                          (append suffix (list (substring layerName 0 (-- (string-length layerName)))))
                                          (cdr group))

              (if (equal? #\- (last-char layerName))
                (get-group-name-components result suffix (cdr group))
                (get-group-name-components 
                                            (append result (list layerName))
                                            suffix
                                            (cdr group))
                )
            )
          )
        )
    )  
  )

  (define (flatten-and-save! originalImage image useSeparateFolder)
      (let* (
          (mergedLayer (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE)))
          (dirname (string-remove-after (car (gimp-image-get-filename originalImage)) #\.))
          (filename (string-append (list-ref (gimp-image-get-name image) 0) ".png"))
          (filepath (string-append dirname DIR-SEPARATOR filename))
      )
          (if (true? useSeparateFolder)
            (file-png-save-defaults RUN-NONINTERACTIVE image mergedLayer filepath filepath)
            (file-png-save-defaults RUN-NONINTERACTIVE image mergedLayer filename filename)
          )
      ) 
    )  

  (let* (         
         (originalImage (aref (cadr (gimp-image-list)) 0))
         (imageWidth (list-ref (gimp-image-width originalImage) 0))
         (imageHeight (list-ref (gimp-image-height originalImage) 0))
         (groupsCount (car (gimp-image-get-layers originalImage)))
         )

          (for-each (combine-groups '() (iterate-all-groups '() groupsCount (get-gimp-groups originalImage)))
            (lambda (group)
              (let* (
                (newImage (car (gimp-image-new imageWidth imageHeight RGB)))
              )            
                (gimp-image-set-filename newImage 
                        (string-append prefix
                          (strings-join separator (get-group-name-components group))))  

                (for-each group (lambda (layerId)
                  (gimp-image-insert-layer 
                                        newImage
                                        (head (gimp-layer-new newImage imageWidth imageHeight RGBA-IMAGE 
                                                (car (gimp-item-get-name layerId))
                                                100 LAYER-MODE-NORMAL))
                                        0 0)
                  (gimp-image-set-active-layer originalImage layerId)
                  (gimp-edit-copy (car (gimp-image-get-active-layer originalImage)))
                  (gimp-floating-sel-anchor (car (gimp-edit-paste (car (gimp-image-get-active-layer newImage)) TRUE)))                
                ))

                (if (true? isSilent)
                  (begin 
                    (flatten-and-save! originalImage newImage useInnerFolder)
                    (gimp-image-delete newImage)
                  )

                  (gimp-display-new newImage)
                )                 
              )
            )             
          )
  )
)

(script-fu-register "script-fu-layers-combinator"
                    _"Create all layers combinations"
                    _"Create all layers combinations"
                    ""
                    ""
                    ""
                    "*"  
                    SF-STRING "Prefix" "img"
                    SF-STRING "Separator" "_"
                    SF-TOGGLE "Save in the separate folder (should be created manually)" TRUE
                    SF-TOGGLE "Silent mode" TRUE
                    )

(script-fu-menu-register "script-fu-layers-combinator"
                         "<Image>/Tools/")

