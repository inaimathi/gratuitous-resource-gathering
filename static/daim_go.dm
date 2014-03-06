outer
  @req xhr-send

  @board-html dom-set-html board
  @board-click dom-on-click board

  @res sse-receive /yodel
  @go-form dom-on-submit
  @output dom-set-text

  $pieces []
  piece

  @go-form -> make-move

  @board-click -> { __ | tap } -> { __ | * (:color :red :x __.x :y __.y)} -> req-wrapper

  @res -> res-decode -> @output
          res-decode -> { __ | >$pieces.#0 }-> piece -> @board-html

  res-decode { __ | string uri-decode | list from-json | tap (:x __.x)}
  
  make-move { __ | >form | * (:color _form.color :x _form.x :y _form.y) }

  req-wrapper { ("echo?value=" __in ) | string join on "" }

  make-move -> req-wrapper
  req-wrapper -> @req