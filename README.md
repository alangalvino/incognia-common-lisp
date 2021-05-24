# Incognia API Common Lisp Client
![tests workflow](https://github.com/alangalvino/incognia-wrapper/workflows/.github/workflows/tests.yml/badge.svg)

Common Lisp lightweight client library for [Incognia APIs](https://dash.incognia.com/api-reference).

## Installation

In the future you'll be able to install from Quicklisp:

```
(ql:quickload :cl-incognia)
```

For now you can install localy this way:

```
;; Move your repository to ~/common-lisp
mv cl-incognia ~/common-lisp/

;; Run load-system
(asdf:load-system "cl-incognia")
```

## Dependencies

- [jonathan (json)](https://github.com/Rudolph-Miller/jonathan)
- [dexador (http)](https://github.com/fukamachi/dexador)

## Usage

### Configuration

Before calling the API methods, you need to configure your Incognia credentials:

```lisp
(incognia:configure :client-id "your-client-id"
                    :client-secret "your-client-secret"
                    :region :us)
```

### Registering New Signup

```lisp
(incognia:register-signup :installation-id "your-installation-id"
                          :address (incognia:make-address :line "340 Avenue, CA")
```

### Getting a Signup

```lisp
(incognia:get-signup-assessment :signup-id "your-signup-id")
```

### Registering Login

```lisp
(incognia:register-login :installation-id "your-installation-id"
                         :account-id "your-account-id")
```

### Registering Payment

```lisp
(incognia:register-payment :installation-id "your-installation-id"
                           :account-id "your-account-id"
                           :addresses (list
                                      (incognia:make-address :line "340 Avenue, CA" :type :|home|)
                                      (incognia:make-address :line "500 Street, CA" :type :|billing|)))
```

### Sending Feedback

```lisp
(incognia:send-feedback :installation-id "your-installation-id"
                        :event :|verified|
                        :account-id "your-account-id"
                        :timestamp (get-universal-time))
```

## Incognia Response Structure

```lisp
(:|id| "<resource-id>"
 :|risk_assessment| "low_risk"
 :|evidence|  (:|account_integrity| (:|recent_high_risk_assessment| NIL) 
               :|last_location_ts| "2021-05-24T14:05:58.557Z" 
               :|distance_to_trusted_location| 50.0744561989637d0
               :|device_behavior_reputation| "unknown" 
               :|device_fraud_reputation| "allowed"
               :|device_integrity| (:|from_official_store| T :|gps_spoofing| NIL :|emulator| NIL :|probable_root| NIL)
               :|location_services| (:|location_sensors_enabled| NIL :|location_permission_enabled| NIL)
               :|known_account| T 
               :|device_model| "motorola one vision")
 )
```

## Handling Unexpected HTTP Status Code

## What is Incognia?

Incognia is a location identity platform  

## Create a Free Incognia Account

## License

 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
