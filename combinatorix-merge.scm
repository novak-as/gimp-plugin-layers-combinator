(define (for-each list func)
  (let loop ((rest list))
    (unless (null? rest)
      (func (car rest))
      (loop (cdr rest))
    )
  )
)

(define (script-fu-combinatorix-merge)
  (let* (
    (imageWidth 50)
    (imageHeight 50)
    (originalImage (aref (cadr (gimp-image-list)) 0))
    (layersCount (car (gimp-image-get-layers originalImage)))
  )

  (define (get-layer-number image sequenceNumber)
    (aref (cadr (gimp-image-get-layers image)) sequenceNumber)
  )

  (define (iterate-all-combinations fromValue toValue skipAmount currentSet currentName)

    (let (
        (newSet (append (list (+ fromValue skipAmount)) currentSet))
        (newName (string-append currentName "-" (number->string (+ fromValue skipAmount))))
      )    
      
        (let (
          (newImage (car (gimp-image-new imageWidth imageHeight RGB)))
        ) 
          (for-each newSet (lambda (layerSequenceNumber)
                        
            (gimp-image-insert-layer 
              newImage
              (head (gimp-layer-new newImage imageWidth imageHeight RGBA-IMAGE (number->string layerSequenceNumber) 100 LAYER-MODE-NORMAL))
              0 0)

            (gimp-image-set-active-layer originalImage (get-layer-number originalImage layerSequenceNumber))

            (gimp-edit-copy (car (gimp-image-get-active-layer originalImage)))
            (gimp-floating-sel-anchor (car (gimp-edit-paste (car (gimp-image-get-active-layer newImage)) TRUE)))
          ))                    

          (gimp-display-new newImage)
        )

      (when (< (+ 1 skipAmount fromValue) toValue)  
        (iterate-all-combinations (+ 1 fromValue) toValue skipAmount newSet newName))  
    )

    (when (< (+ 1 skipAmount fromValue) toValue)        
      (iterate-all-combinations fromValue toValue (+ 1 skipAmount) currentSet currentName))   
  )
  
  (iterate-all-combinations 0 layersCount 0 '() "layer"))
)

(script-fu-register "script-fu-combinatorix-merge"
  _"Create all possible combinations"
  _"Create all possible combinations from existing layers"
  ""
  ""
  ""
  "*"
)

(script-fu-menu-register "script-fu-combinatorix-merge"
                         "<Image>/Tools/")

