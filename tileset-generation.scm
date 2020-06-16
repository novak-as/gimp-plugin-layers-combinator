(define (script-fu-tileset-generator prefix separator)

  ; strings
  (define (strings-join delimiter list)
    (let loop ((current "") (values list))
      (if (null? values)
          current
          (loop (string-append current delimiter (car values)) (cdr values)))
      )
    )

  (define (string-rest line)
    (substring line 1 (string-length line)))

  (define (last-char string)
    (string-ref string (- (string-length string) 1)))

  (define (string-split delimiter line)

    (define (_string-split result currentSeg delimiter line)

      (if (equal? "" line)
          (append result (list currentSeg))

          (let (
                (currentChar (string-ref line 0))
                )

            (if (equal? delimiter currentChar)
                (_string-split (append result (list currentSeg)) "" delimiter (string-rest line))
                (_string-split result (string-append currentSeg (string currentChar)) delimiter (string-rest line))          
                ))    
          ))

    (_string-split '() "" delimiter line)
    
    )  

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

  (define (pop stack)
    (cdr stack)
    )

  (define (peek stack)
    (car stack)
    )

  (define (replace stack val)
    (push (pop stack) val))

  (define (true? val)
    (equal? #t val))

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

    (define (_list-count result tail)
      (if (null? tail)
        result
        (_list-count (++ result) (cdr tail))
      )
    )

    (_list-count 0 val)
  )

  (define (drop-last input)

    (define (_drop-last head tail)
      (if (null? (cdr tail)) 
          head
          (_drop-last (append head (list (car tail))) (cdr tail))    
          )    
      )

    (_drop-last '() input)    
    )

  (define (filter func values)

    (define (_filter func values result)
      
      (if (null? values)
          result
          (if (true? (func (car values)))
              (_filter func (cdr values) (append result (list (car values))))
              (_filter func (cdr values) result)
              )
      
          )  
      )

    (_filter func values '())
    )   

  ;options 
  (define (get-options-text-segment name)
    (define (_get-options-text-segment result isOpen line)
      (if (equal? "" line)
          result
          (let (
                (currentChar (string-ref line 0))
                )
            (if (not isOpen)
                (_get-options-text-segment result (equal? #\[ currentChar) (string-rest line))

                (if (equal? #\] currentChar)
                    result

                    (_get-options-text-segment (string-append result (string currentChar)) #t (string-rest line))
                    )           
                ))
          )
      )


    (_get-options-text-segment "" #f name)
  )

  (define (string->option val)
    (cond
      ((equal? "min" val) 'min)
      ((equal? "max" val) 'max)
      ((equal? "combine" val) 'combine)
      (error (string-append "Unknown option " val))
      )    
    )

(define (get-options options)

  (define (get-options result options)

    (if (null? options)
        result

        (let (
              (words (string-split #\= (car options)))
              )
          (get-options (append result (list (list (string->option (list-ref words 0)) (list-ref words 1)))) (cdr options))
          )
    
        )
    )

  (get-options '() options)
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

    (define (option-remove key set result)

      (if (null? set)
          result
          
          (if (equal? key (list-ref (car set) 0))
              (option-remove key (cdr set) result)
              (option-remove key (cdr set) (append result (list (car set)))))
      
          )
      )

    (option-remove key set '())
    
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
    (define (_iterate from to result)

      (if (= from to)
        result          

        (let (
          (currentLayer (list-ref groups from))
        )
          (_iterate (++ from) to (push result (list-ref groups from))))      
      )        
    )

    (_iterate 0 to '())
  )

  (define (iterate-all amount group)
    (define (_iterate-all from to direction set picks result)
      
      (if (and (true? direction) (<= (++ from) to))
          (let (
                (set (append set (list (list-ref group from))))
              )                
            (_iterate-all (++ from) to #t set (push picks #t) (push result set))      
          )

          (if (false? direction)
              (if (true? (peek picks)) 
                  (_iterate-all (++ from) to #t set (push (replace picks #f) #t) result)
                  (_iterate-all (-- from) to #f set (pop picks) result))
              (if (= from to)
                  (if (equal? set '())
                      result
                      (if (true? (peek picks))        
                          (_iterate-all (-- from) to #f (drop-last set) (pop picks) result)
                          (_iterate-all (-- from) to #f set (pop picks) result)))
                  (display "")
                  )
              )
          )
      )

    (_iterate-all 0 amount #t '() '(#t) '())
    )

  (define (combine-list list1 list2)
    (define (_combine-list set olist2 list1 list2)

      (if (null? olist2)
          (if (null? list1)
              set
              (_combine-list (push set (car list1)) olist2 (cdr list1) list2)
            )
          (if (null? list1)
              set
              (if (null? list2)
                  (_combine-list set olist2 (cdr list1) olist2)
                  (_combine-list (push set (append (car list2) (car list1))) olist2 list1 (cdr list2))
                  )
              )
          )
      )
    (_combine-list '() list2 list1 list2)
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

    (define (_get-gimp-groups result tail)
      (if (null? tail)
        result
        (let* (
          (currentGroupIndex (car tail))
          (currentGroupName (car (gimp-item-get-name currentGroupIndex)))
          (currentGroupChildrens (filter is-visible? (vector->list (cadr (gimp-item-get-children currentGroupIndex)))))
          (currentGroupItemsCount (list-count currentGroupChildrens))
        )        
          (_get-gimp-groups 
            (push result (list currentGroupIndex currentGroupName currentGroupItemsCount currentGroupChildrens))
            (cdr tail)
          ))        
      )      
    )

    (_get-gimp-groups '() (vector->list (cadr (gimp-image-get-layers image))))
  )

  (define (get-original-layer-name layerNumber)
    (car (gimp-item-get-name layerNumber))
  )

  (define (is-visible? layerNumber)  
    (equal? 1 (car (gimp-item-get-visible layerNumber)))
  )

  (define (get-group-name-components group)
       
    (define (_get-group-name-components result suffix group)
      (if (null? group)
        (append result suffix)

        (let (
          (layerName (get-original-layer-name (car group)))
        )
          (if (equal? #\^ (last-char layerName))
              (_get-group-name-components 
                                          result 
                                          (append suffix (list (substring layerName 0 (-- (string-length layerName)))))
                                          (cdr group))

              (if (equal? #\- (last-char layerName))
                (_get-group-name-components result suffix (cdr group))
                (_get-group-name-components 
                                            (append result (list layerName))
                                            suffix
                                            (cdr group))
                )
            )
          )
        )
    )

    (_get-group-name-components '() '() group)    
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

                (gimp-display-new newImage)            
              )
            ))         
  )
)

(script-fu-register "script-fu-tileset-generator"
                    _"Create all possible tiles"
                    _"Create all possible tiles from existing layers"
                    ""
                    ""
                    ""
                    "*"  
                    SF-STRING "Prefix" "img"
                    SF-STRING "Separator" "_"
                    )

(script-fu-menu-register "script-fu-tileset-generator"
                         "<Image>/Tools/")

