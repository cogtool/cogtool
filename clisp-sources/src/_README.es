Esto es GNU CLISP, una implementación de Common Lisp.


¿ Qué es LISP ?
---------------

LISP es un lenguaje de programación inventado por J. McCarthy en
1959. Aunque ha habido muchos dialectos de él, actualmente se ha
estandarizado y difundido ampliamente gracias al estandar industrial
COMMON LISP. Hay aplicaciones en los dominios del procesamiento del
conocimiento simbólico (IA), cálculo numérico (MACLISP generaba código
tan bueno como el de FORTRAN), y en programas ampliamente utilizados
como editores (EMACS) y CAD (AUTOCAD). Si lo desea, puede consultar la
introducción al lenguaje LISP:

  Sheila Hughes: Lisp. Pitman Publishing Limited, London 1986.
  107 pages.

Después de un rato, necesitará el texto estandar que contiene la
definición del lenguaje:

Guy L. Steele Jr.: Common Lisp - The Language. Digital Press.
  1. edition 1984, 465 pages.
  2. edition 1990, 1032 pages.

Este libro está disponible en formato HTML via FTP en:
  ftp.cs.cmu.edu:/user/ai/lang/lisp/doc/cltl/cltl_ht.tgz

y puede consultarse a través de WWW en:

  http://www-2.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html o
  http://www-2.cs.cmu.edu/afs/cs/project/ai-repository/ai/html/cltl/cltl2.html .

Nota para los expertos: Este texto estandar se ha convertido en un
estándar ANSI, que puede obtenerse <<<exceptionally>>> sin cargo alguno en:

  http://www.lisp.org/HyperSpec/FrontMatter/

LISP se ejecuta en un entorno interactivo. Usted introduce formas, que
serán evaluadas de inmediato. Por lo tanto, puede inspeccionar
variables, invocar funciones con unos argumentos concretos o definir
sus propias funciones.


Contenidos:
-----------

Consta de los siguientes ficheros:

#ifdef UNIX
#ifndef UNIX_BINARY_DISTRIB
   base/lisp.a            programa principal, listo para ser enlazado
#endif
#ifdef UNIX_BINARY_DISTRIB
   base/lisp.run          programa principal
#endif
   base/lispinit.mem      imagen de memoria necesaria para la inicialización
   doc/clisp.1            manual en formato man de Unix
   doc/clisp.man          manual
   doc/clisp.html         manual en format HTML
   doc/impnotes.html      notas de la implementación
#ifdef GNU_READLINE
   doc/clreadline.3       manual de edición de línea en formato man de Unix
   doc/clreadline.man     manual de edición de línea
#endif
   doc/LISP-tutorial.txt  tutorial de LISP para aprendices
   doc/CLOS-guide.txt     breve guía de CLOS
   README                 este texto
   SUMMARY                pequeña descripción de CLISP
   ANNOUNCE               declaración
   NEWS                   lista de modificaciones desde la última versión
   COPYRIGHT              derechos de autor <<<copyright>>>
   GNU-GPL                licencia de software libre
#ifdef GNU_READLINE
   doc/readline.dvi       documentación de la librería GNU readline
                          en formato DVI
#endif
   doc/editors.txt        Lista de editores que soportan Lisp
   emacs/*.el             personalización de Emacs, véase doc/editors.txt
#ifndef UNIX_BINARY_DISTRIB
   src/clisp.c            fuentes del programa
#endif
   src/config.lisp        configuración dependiente del lugar

y - cuando le apetezca, si le gusta leer código fuente -

   src/*.lisp             el código fuente de lispinit.mem
   src/*.fas              los mismos ficheros, una vez compilados
#if !defined(UNIX_BINARY_DISTRIB) && defined(GNU_READLINE)

Para crear el ejecutable, también necesitará:

   base/libreadline.a     librería GNU readline

o

   base/libnoreadline.a   sustituto ficticio de la librería GNU readline
#endif
#else /* !defined(UNIX) */
      lisp.exe           programa principal
      lispinit.mem       imagen de memoria necesaria para la inicialización
#ifdef GNU_GETTEXT
      locale/*/LC_MESSAGES/clisp.mo  <<<localized messages databases>>>
#endif
      clisp.1            manual en formato `man' de Unix
      clisp.man          manual
      clisp.html         manual en format HTML
      impnotes.html      notas de la implementación
#ifdef GNU_READLINE
      clreadline.3       manual de edición de línea en formato `man' de Unix
      clreadline.man     manual de edición de línea
      clreadline.html    manual de edición de línea en format HTML
#endif
      LISP-tutorial.txt  tutorial de LISP para aprendices
      CLOS-guide.txt     breve guía de CLOS
      editors.txt        <<<some words about text editors for Lisp>>>
      README             este texto
      SUMMARY            pequeña descripción de CLISP
      ANNOUNCE           declaración
      NEWS               lista de modificaciones desde la última versión
      COPYRIGHT          derechos de autor
      GNU-GPL            licencia de software libre
#ifdef GNU_READLINE
      readline.dvi	 documentación de la librería GNU readline en formato DVI
#endif
      config.lisp        configuración dependiente del lugar
#if !(defined(UNIX) || defined(WIN32))
      timezone.lisp      zona horaria dependiente del lugar
#endif

y - cuando le apetezca, si le gusta leer código fuente -

      *.lisp             el código fuente de lispinit.mem
      *.fas              los mismos ficheros, una vez compilados
#endif

#if defined(SINGLEMAP_MEMORY) && (defined(UNIX_LINUX) || !defined(HAVE_MMAP_ANON))

Requisitos Software:
--------------------

#ifdef UNIX_LINUX
#ifdef GENERATIONAL_GC
#ifdef IMMUTABLE
Esta versión de CLISP necesita Linux 1.2.2 o más reciente.
#else
Esta versión de CLISP necesita Linux 1.1.52 o más reciente.
#endif
#else
Esta versión de CLISP necesita Linux 0.99.7 o más reciente.
#endif
#endif
#if !defined(HAVE_MACH_VM) && !defined(HAVE_MMAP_ANON) /* impliziert HAVE_MMAP_DEVZERO */
/dev/zero debe ser legible por cualquiera. Para ello, debe ejecutar el
comando "chmod a+r /dev/zero".
#endif

#endif

Instalación:
------------

#if defined(UNIX) || defined(WIN32)
#if defined(UNIX) && !defined(UNIX_BINARY_DISTRIB)
Teclee

         make

#if 0 /* def GNU_READLINE - man muß Makefile verändern */
Si desea renunciar a las capacidades de edición de lectura de la
librería GNU readline, debería haber reemplazado "libreadline.a" en la
línea LIBS del fichero BASE/MAKEVARS por "libnoreadline.a".

#endif
#endif
Cambie las cadenas en SRC/CONFIG.LISP, empleando para ello un editor de
textos.
#else
Edite el fichero CONFIG.LISP y modifíquelo adecuadamente para su
estación, con especial atención a las definiciones de short-site-name
y long-site-name. Si lo desea, también puede modificar la definición
de la zona horaria al final del fichero TIMEZONE.LISP.
#endif
Luego ejecute

#ifdef WIN32_NATIVE
         lisp.exe -M lispinit.mem
#endif
#ifdef UNIX
         base/lisp.run -M base/lispinit.mem
#endif

Cuando aparezca el inductor de comandos

      > _

teclee

#if defined(UNIX) || defined(WIN32)
      (without-package-lock ()
        (compile-file "src/config.lisp")
        (load "src/config.fas"))
#else
      (without-package-lock ()
        (compile-file "config.lisp")
        (load "config.fas"))

y - si modificó el fichero TIMEZONE.LISP -

      (without-package-lock ()
        (compile-file "timezone.lisp")
        (load "timezone.fas"))
#endif

y luego

#ifdef UNIX
        (cd "base/")
#endif
        (saveinitmem)

para sobreescribir el fichero LISPINIT.MEM con su configuración. A
continuación

        (exit)

#ifdef UNIX
El resto se hace simplemente con

        make install

En vez de esto, puede hacerlo usted mismo, paso por paso:

#endif
Luego cree un directorio, y ponga en él el ejecutable con la imagen de
memoria.
#ifdef UNIX
Le recomiendo /usr/local/lib/lisp :

   mkdir /usr/local/lib/lisp
   mv base/lisp.run /usr/local/lib/lisp
   mv base/lispinit.mem /usr/local/lib/lisp
#endif
#ifdef WIN32_NATIVE
Suponiendo D:\LIB\LISP :

   mkdir d:\lib\lisp
   copy lisp.exe d:\lib\lisp
   copy lispinit.mem d:\lib\lisp
#endif

#ifdef WIN32_NATIVE
Y cree un fichero de ejecución por lotes que ejecute lisp:

   copy con c:\bat\clisp.bat
   d:\lib\lisp\lisp.exe -M d:\lib\lisp\lispinit.mem -B d:\lib\lisp\ %1 %2 %3 %4 %5 %6 %7 %8 %9
   [Ctrl-Z]
#endif
#ifdef UNIX
Y cree el programa que ejeute lisp:

#ifdef UNIX_BINARY_DISTRIB
   cc -O -DLISPLIBDIR='"/usr/local/lib/lisp"' \
         -DLOCALEDIR='"/usr/local/share/locale"' \
      src/clisp.c -o /usr/local/bin/clisp
#else
   ./hardcode -DLISPLIBDIR='/usr/local/lib/lisp' \
              -DLOCALEDIR='/usr/local/share/locale' \
              clisp /usr/local/bin/clisp
#endif
#ifdef WIN32_NATIVE
You can also use file install.bat which creates the driver file
on your desktop and sets up the registry. [FIXME]
#endif

#ifdef GNU_READLINE
Ahora, instale las páginas de man.
#else
Ahora, instale la página man.
#endif

   mv doc/clisp.1 /usr/local/man/man1/clisp.1
#ifdef GNU_READLINE
   mv doc/clreadline.3 /usr/local/man/man3/clreadline.3
#endif

and try

   man clisp
#endif

Cuando encuentre problemas:
---------------------------

Después de un error, se encontrará en el depurador:

     1. Break> _

En él, usted puede evaluar formas como siempre. Más aún:

     Help
               invoca la ayuda
     Abort     o
     Unwind
               retrocede hasta el bucle de entrada más reciente
     Backtrace
               muestra los contenidos de la pila, útil para la depuración

Y puede consultar el valor de las variables de las funciones donde se
produjo el error.

#ifdef UNIX
Cuando los problemas sean mayores, por ejemplo `core dumps', por favor
#endif
envíe una descripción del error y una descripción de cómo reproducir
el error a los autores o al "mantenedor". Por favor, acompañe su mensaje
de la versión de CLISP que puede obtener invocando la función
(lisp-implementation-version).


Código fuente:
--------------

El código fuente de CLISP está disponible en
     ftp://clisp.cons.org/pub/lisp/clisp/source/clispsrc*
#ifdef UNIX_LINUX
La última distribución binaria de CLISP para Linux tiene su propio
código fuente en
     ftp://sunsite.unc.edu/pub/Linux/devel/lang/lisp/clisp-source.tar.gz
#endif


#if 0
<<<Mailing lists:>>>
#endif
Lista de correo:
----------------

#if 0
Hay una lista de correo para los usuarios de CLISP. Ése es el foro
adecuado para cualquier cuestión relacionada con CLISP, problemas de
instalación, errores, paquetes de aplicaciones, etc.
#endif
<<<There are three mailing lists for users of CLISP. You find subscription
information and archives on the homepage http://clisp.cons.org/.>>>


Agradecimientos:
----------------

Estamos muy agradecidos a
  * Guy L. Steele y otros muchos por la especificación de Common Lisp.
#ifdef UNIX
  * El proyecto GNU de Richard Stallman para el GCC, Autoconf y la librería
    readline.
#else
#ifdef GNU_READLINE
  * El proyecto GNU de Richard Stallman para el GCC y la librería readline.
#else
#ifdef GNU
  * El proyecto GNU de Richard Stallman para el GCC.
#endif
#endif
#endif


Autores:
--------

        Bruno Haible
        Michael Stoll

Email: clisp-list@lists.sourceforge.net

"Mantenedor":
-------------

        Sam Steingold

Email: clisp-list@lists.sourceforge.net
