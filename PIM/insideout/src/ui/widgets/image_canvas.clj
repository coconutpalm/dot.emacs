(ns ui.widgets.image-canvas
  (:import [org.eclipse.swt SWT]
           [org.eclipse.swt.graphics Font FontData Image]
           [org.eclipse.swt.widgets Display Canvas]
           [org.eclipse.swt.layout FillLayout]))


(defn svg-image
  ;; https://linuxtut.com/edit-svg-with-java-+-apache-batik-to-convert-to-png-or-jpeg-f243e/
  "Return an Image from SVG file.  Options are :width x :height x; or :size [w h]; :keep-aspect-ratio true/false"
  [svg-file-path & opts])


(defn image-canvas
  [image-file-path & inits]
  (let [image (Image. (Display/getDefault) image-file-path)]
    ;; Something like:
    #_(canvas SWT/DOUBLE_BUFFERED
              (on-paint [props event]
                        ;; Scale image example https://www.aniszczyk.org/2007/08/09/resizing-images-using-swt/
                        )
              (widget-disposed [props event] (.dipose image)))))
