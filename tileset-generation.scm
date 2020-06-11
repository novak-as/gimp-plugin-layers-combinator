(define (script-fu-tileset-generator prefix separator)

  (define (strings-join delimiter list)
    (let loop ((current "") (values list))
      (if (null? values)
          current
          (loop (string-append current delimiter (car values)) (cdr values)))
      )
    )

  (define (last-char string)
    (string-ref string (- (string-length string) 1)))

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

  (define (iterate-all amount groups)
    (define (_iterate-all from to direction set picks result)
      
      (if (and (true? direction) (<= (++ from) to))
          (let (
                (set (append set (list (list-ref groups from))))
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
        (let (
              (currentGroup (car groups))
              )
          (if (equal? #\! (last-char (list-ref currentGroup 1)))
              (iterate-all-groups (append result
                                          (list(list
                                                (list-ref currentGroup 0)
                                                (list-ref currentGroup 1)
                                                (iterate-all (list-ref currentGroup 2) (list-ref currentGroup 3))))
                                          ) (-- amount) (cdr groups))
              (iterate-all-groups (append result
                                          (list(list
                                                (list-ref currentGroup 0)
                                                (list-ref currentGroup 1)
                                                (map list (iterate (list-ref currentGroup 2) (list-ref currentGroup 3)))))
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
            (_get-group-name-components 
              (append result (list layerName))
              suffix
              (cdr group))
            ))
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

