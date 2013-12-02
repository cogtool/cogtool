(setf (ext:package-lock "I18N") nil
      custom:*system-package-list*
      (delete "I18N" custom:*system-package-list* :test #'string=))
