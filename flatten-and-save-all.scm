(define (script-fu-flatten-and-save-all)

    (define (for-each list func)
        (let loop ((rest list))
            (unless (null? rest)
            (func (car rest))
            (loop (cdr rest))
            )
        )
    )

    (for-each (vector->list (cadr (gimp-image-list))) (lambda (image)

        (let (
            (mergedLayer (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE)))
            (filename (string-append (list-ref (gimp-image-get-name image) 0) ".png"))
        )
            (file-png-save-defaults RUN-NONINTERACTIVE image mergedLayer filename filename)
        )

    )) 
)

(script-fu-register "script-fu-flatten-and-save-all"
                    _"Flatten and save all"
                    _"Flatten all opened images and save it as a png"
                    ""
                    ""
                    ""
                    "*"  
                    )

(script-fu-menu-register "script-fu-flatten-and-save-all"
                         "<Image>/Tools/")