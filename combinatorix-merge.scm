(define (for-each list func)
  (let loop ((rest list))
    (unless (null? rest)
      (func (car rest))
      (loop (cdr rest))
    )
  )
)

(define (join-strings-with-delimiter delimiter list)
  (let loop ((current "") (values list))
    (if (null? values)
        current
        (loop (string-append current delimiter (car values)) (cdr values)))
    )
)

(define (script-fu-combinatorix-merge)
  (let* (
    (imageWidth 50)
    (imageHeight 50)
    (originalImage (aref (cadr (gimp-image-list)) 0))
    (layersCount (car (gimp-image-get-layers originalImage)))
    (imageNamePrefix "image")
    (imageNameLayerSeparator "_") 
  )

  (define (get-layer-number image sequenceNumber)
    (aref (cadr (gimp-image-get-layers image)) sequenceNumber)
  )

  (define (get-original-layer-name layerNumber)
    (car (gimp-item-get-name layerNumber))
  )

  (define (iterate-all-combinations fromValue toValue skipAmount currentSet)

    (let* (
      (newImage (car (gimp-image-new imageWidth imageHeight RGB)))
      (newSet (append (list (get-layer-number originalImage (+ fromValue skipAmount))) currentSet))
    ) 
      (for-each newSet (lambda (layerId)
                    
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

      (gimp-image-set-filename newImage 
        (string-append imageNamePrefix 
          (join-strings-with-delimiter imageNameLayerSeparator (map get-original-layer-name newSet)) 
        )
      )

      (gimp-display-new newImage)

      (when (< (+ 1 skipAmount fromValue) toValue)  
        (iterate-all-combinations (+ 1 fromValue) toValue skipAmount newSet))            
    )

    (when (< (+ 1 skipAmount fromValue) toValue)        
      (iterate-all-combinations fromValue toValue (+ 1 skipAmount) currentSet))   
  )
  
  (iterate-all-combinations 0 layersCount 0 '()))
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

