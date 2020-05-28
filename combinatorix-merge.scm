(define (script-fu-combinatorix-merge)
  (define imageWidth 50)
  (define imageHeight 50)

  (define originalImage (aref (cadr (gimp-image-list)) 0) )
  (define layersCount (car (gimp-image-get-layers originalImage)))

  (define newImage (car (gimp-image-new imageWidth imageHeight RGB)))  

  (define (iterateAllCombinations fromValue toValue skipAmount currentSet currentName)

    (let (
        (newSet (append (list (+ fromValue skipAmount)) currentSet))
        (newName (string-append currentName "-" (number->string (+ fromValue skipAmount))))
      )    
      
      (gimp-image-insert-layer 
        newImage
        (head 
          (gimp-layer-new 
            newImage imageWidth imageHeight RGB-IMAGE newName 100 LAYER-MODE-NORMAL))
        0
        0)      
    
      (when (<= (+ 1 skipAmount fromValue) toValue)
        (iterateAllCombinations (+ 1 fromValue) toValue skipAmount newSet newName))
    )

    (when (<= (+ 1 skipAmount fromValue) toValue)        
      (iterateAllCombinations fromValue toValue (+ 1 skipAmount) currentSet currentName))   
    )
  

  (iterateAllCombinations 0 layersCount 0 '() "layer")

  (gimp-display-new newImage)
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
                         "<Image>/Tools/CombinatorixMerge")

