<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
 <!ENTITY % html "IGNORE">
 <![%html;[
  <!ENTITY % print "IGNORE">
  <!ENTITY docbook.dsl PUBLIC
   "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA dsssl>
  ]]>
 <!ENTITY % print "INCLUDE">
 <![%print;[
  <!ENTITY docbook.dsl PUBLIC
   "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA dsssl>
  ]]>
]>

<style-sheet>
<style-specification use="docbook">
<style-specification-body>

;; common to both html and print

(define %default-quadding% "justify")
(define ($generate-book-lot-list$) '())

(define (book-titlepage-recto-elements)
  (list (normalize "title")
	(normalize "subtitle")
	(normalize "date")
	(normalize "pubdate")
	(normalize "edition")
        (normalize "author")
        (normalize "authorblurb")
	(normalize "graphic")
	(normalize "copyright")
	(normalize "legalnotice")))

(element imagedata
  (if (have-ancestor? (normalize "mediaobject"))
      ($img$ (current-node) #t)
      ($img$ (current-node) #f)))

(element emphasis
  (let ((role (attribute-string (normalize "role"))))
    (case role
      (("strong") ($bold-seq$))
      (else ($italic-seq$)))))

(element literal
  (let ((role (attribute-string (normalize "role"))))
    (case role
      (("sexp" "type")
       (make element gi: "TT" attributes: (list (list "CLASS" role))
             (process-children)))
      (else
       ($mono-seq$)))))

(element quote
   (let ((role (attribute-string (normalize "role"))))
     (case role
       (("package")
        (make element gi: "STRONG" attributes: (list (list "CLASS" role))
              (next-match)))
       (else (next-match)))))

(element (varlistentry term)
  (make sequence
    (process-children-trim)
    (if (not (last-sibling?))
	(make empty-element gi: "BR")
	(literal ""))))

(element (firstterm)
  (make element gi: "STRONG" attributes: (list (list "CLASS" "FIRST"))
        (next-match)))

<![%print;[ ;; customize the print stylesheet here

(define %paper-type% "USletter")

]]>

<![%html;[  ;; customize the html stylesheet here

;; /usr/share/sgml/docbook/dsssl-stylesheets/html/dbparam.dsl
(define %force-chapter-toc% #t)
(define %shade-verbatim% #t)
(define biblio-citation-check #t)
(define %html-ext% ".html")
;;(define %html-header-tags% '(("META" ("NAME" "name") ("CONTENT" "content"))))
(define %html-pubid% "-//W3C//DTD HTML 4.01//EN")
;;(define html-index #t)
;;(define html-index-filename "impnotes.idx")
;;(define html-manifest #t)
;;(define html-manifest-filename "impnotes.lst")
(define %stylesheet% "impnotes.css")
(define %use-id-as-filename% #t)
(define %funcsynopsis-decoration% #t)
(define %link-mailto-url% "mailto:clisp-list@sf.net")
(define %section-autolabel% #t)
(define %graphic-extensions%
  '("gif" "png" "jpg" "jpeg" "tif" "tiff" "eps" "epsf" ))
(define %admon-graphics% #t)
(define %admon-graphics-path%
  "/usr/share/sgml/docbook/dsssl-stylesheets/images/")
(define %show-comments% #t)     ; show the REMARK element

;; (define %generate-legalnotice-link% #f) ; default

(define %clisp-gnu-sourceforge-footer%
  (make element gi: "div" attributes: '(("class" "custom-footer"))
        (make empty-element gi: "hr" attributes: '(("width" "100%")))
        (make element gi: "table" attributes: '(("width" "100%"))
              (make element gi: "tr"
                    (make element gi: "td" attributes: '(("align" "left"))
                          (make element gi: "a" attributes:
                                '(("href" "http://clisp.cons.org"))
                                (make empty-element gi: "img" attributes:
                                      '(("src" "clisp.png")
                                        ("width" "48") ("height" "48")
                                        ("alt" "[CLISP home]")))))
                    (make element gi: "td" attributes: '(("align" "center"))
                          (make element gi: "a" attributes:
                                '(("href" "http://www.gnu.org"))
                                (make empty-element gi: "img" attributes:
                                      '(("src" "http://www.gnu.org/graphics/gnubanner.jpg")
                                        ("width" "468") ("height" "60")
                                        ("alt" "[Come and see what GNU creates for YOU]")))))
                    (make element gi: "td" attributes: '(("align" "right"))
                          (make element gi: "a" attributes:
                                '(("href" "http://sourceforge.net"))
                                (make empty-element gi: "img" attributes:
                                      '(("src" "http://sflogo.sourceforge.net/sflogo.php?group_id=1355&#38;amp;type=2")
                                        ("width" "125") ("height" "37")
                                        ("alt" "[SourceForge]")))))))))

(define (nav-footer elemnode) %clisp-gnu-sourceforge-footer%)

]]>

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
