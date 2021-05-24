(in-package :cl-incognia)

(deftype address-type () '(member
                           :|shipping|
                           :|billing|
                           :|home|))

(defclass address ()
  ((line
    :initarg :line
    :initform nil
    :accessor addr-line)
   (lat
    :initarg :lat
    :initform nil
    :accessor addr-lat)
   (lng
    :initarg :lng
    :initform nil
    :accessor addr-lng)
   (locale
    :initarg :locale
    :initform nil
    :accessor addr-locale)
   (country-code
    :initarg :country-code
    :initform nil
    :accessor addr-country-code)
   (country-name
    :initarg :country-name
    :initform nil
    :accessor addr-country-name)
   (state
    :initarg :state
    :initform nil
    :accessor addr-state)
   (city
    :initarg :city
    :initform nil
    :accessor addr-city)
   (borough
    :initarg :borough
    :initform nil
    :accessor addr-borough)
   (neighborhood
    :initarg :neighborhood
    :initform nil
    :accessor addr-neighborhood)
   (number
    :initarg :number
    :initform nil
    :accessor addr-number)
   (complements
    :initarg :complements
    :initform nil
    :accessor addr-complements)
   (street
    :initarg :street
    :initform nil
    :accessor addr-street)
   (postal-code
    :initarg :postal-code
    :initform nil
    :accessor addr-postal-code)
   (type
    :initarg :type
    :initform :|home|
    :accessor addr-type)))

(defmethod initialize-instance :after ((obj address) &key)
  (with-slots (type) obj
    (check-type type address-type)))

(defun make-address (&key line lat lng locale country-code country-name state city borough neighborhood number complements street postal-code (type :|home|))
  (make-instance 'address
                 :line line
                 :lat lat
                 :lng lng
                 :locale locale
                 :country-code country-code
                 :country-name country-name
                 :state state
                 :city city
                 :borough borough
                 :neighborhood neighborhood
                 :number number
                 :complements complements
                 :street street
                 :postal-code postal-code
                 :type type))

(defmethod addr-coordinates-valid-p ((obj address))
  (and (typep  (addr-lat obj) 'float) (typep (addr-lng obj) 'float)))

(defmethod addr-line-valid-p ((obj address))
  (addr-line obj))

(defmethod addr-structured-valid-p ((obj address))
  (or (addr-locale obj)
      (addr-country-code obj)
      (addr-country-name obj)
      (addr-state obj)
      (addr-city obj)
      (addr-borough obj)
      (addr-neighborhood obj)
      (addr-number obj)
      (addr-complements obj)
      (addr-street obj)
      (addr-postal-code obj)))

(defmethod addr-valid-p ((obj address))
  (or (addr-line-valid-p obj)
      (addr-coordinates-valid-p obj)
      (addr-structured-valid-p obj)))

(defmethod addr-line-plist ((obj address))
  (list :|address_line| (addr-line obj)))

(defmethod addr-coordinates-plist ((obj address))
  (list :|lat| (addr-lat obj)
        :|lng| (addr-lng obj)))

(defmethod addr-structured-plist ((obj address))
  (list :|locale| (addr-locale obj)
        :|country_code| (addr-country-code obj)
        :|country_name| (addr-country-name obj)
        :|state| (addr-state obj)
        :|city| (addr-city obj)
        :|borough| (addr-borough obj)
        :|neighborhood| (addr-neighborhood obj)
        :|number| (addr-number obj)
        :|complements| (addr-complements obj)
        :|street| (addr-street obj)
        :|postal_code| (addr-postal-code obj)))

(defmethod addr-plist ((obj address))
  (append
   (list :|type| (addr-type obj))
   (and (addr-line-valid-p obj) (addr-line-plist obj))
   (and (addr-coordinates-valid-p obj) (addr-coordinates-plist obj))
   (and (addr-structured-valid-p obj) (addr-structured-plist obj))))
