(define (script-fu-combinatorix-merge)
  (define imageWidth 50)
  (define imageHeight 50)

  (define originalImage (aref (cadr (gimp-image-list)) 0) )
  (define layersCount (car (gimp-image-get-layers originalImage)))

  (define newImage (car (gimp-image-new imageWidth imageHeight RGB)))  

  (let loop ((times (expt 2 layersCount)))
    (gimp-image-insert-layer 
      newImage
      (head 
        (gimp-layer-new 
          newImage imageWidth imageHeight RGB-IMAGE (number->string times) 100 LAYER-MODE-NORMAL))
      0
      0)

    (if (= times 0)
      ()
      (loop (- times 1))
    )
  )

      


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

