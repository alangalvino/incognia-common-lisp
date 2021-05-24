# Incognia API Common Lisp Client
![tests workflow](https://github.com/alangalvino/incognia-wrapper/workflows/.github/workflows/tests.yml/badge.svg)

Common Lisp lightweight client library for [Incognia APIs](https://dash.incognia.com/api-reference).

## Installation

In the future you'll be able to install from Quicklisp:

```
(ql:quickload :cl-incognia)
```

For now you can install it locally:

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

### Incognia API

The implementation is based on the [Incognia API Reference](https://dash.incognia.com/api-reference).

#### Authentication

Authentication is done transparently so you don't need to worry about it.

#### Registering Signup

This method registers a new signup for the given installation (`:installation-id`), returning a risk assessment and supporting evidence (see [Response Structure](#response-structure)):

```lisp
(incognia:register-signup :installation-id "your-installation-id"
                          :address (incognia:make-address :line "340 Avenue, CA")
```

#### Getting a Signup

This method allows you to query the latest assessment for a given signup event, identified by its `:signup-id` (see [Response Structure](#response-structure)). 

```lisp
(incognia:get-signup-assessment :signup-id "your-signup-id")
```

#### Registering Login

This method registers a new login for the given installation (`:installation-id`) and user account (`:account-id`), returning a risk assessment and the supporting evidence (see [Response Structure](#response-structure)).

```lisp
(incognia:register-login :installation-id "your-installation-id"
                         :account-id "your-account-id")
```

#### Registering Payment

This method registers a new payment for the given installation (`:installation-id`) and user account (`:account-id`), returning a risk assessment and the supporting evidence (see [Response Structure](#response-structure)).

```lisp
(incognia:register-payment :installation-id "your-installation-id"
                           :account-id "your-account-id"
                           :addresses (list
                                      (incognia:make-address :line "340 Avenue, CA" :type :|home|)
                                      (incognia:make-address :line "500 Street, CA" :type :|billing|)))
```

#### Sending Feedback

This method registers a feedback event for the given installation (`:installation-id`) related to a signup, login or payment (see [Response Structure](#response-structure)).

```lisp
(incognia:send-feedback :installation-id "your-installation-id"
                        :event :|verified|
                        :account-id "your-account-id"
                        :timestamp (get-universal-time))
```

## Response Structure

With the exception of [Sending Feedback](#sending-feedback) method that returns an empty body (`nil`), Incognia API response is composed by:

- id (resource id, ex., login id)
- request-id
- risk_assessment (low_risk, high_risk or unknow_risk)
- evidence

```lisp
(:|id| "<resource-id>"
 :|request-id| "<request-id>"
 :|risk_assessment| "low_risk"
 :|evidence|  (:|account_integrity| (:|recent_high_risk_assessment| NIL) 
               :|last_location_ts| "2021-05-24T14:05:58.557Z" 
               :|distance_to_trusted_location| 50.0744
               :|device_behavior_reputation| "unknown" 
               :|device_fraud_reputation| "allowed"
               :|device_integrity| (:|from_official_store| T :|gps_spoofing| NIL :|emulator| NIL :|probable_root| NIL)
               :|location_services| (:|location_sensors_enabled| NIL :|location_permission_enabled| NIL)
               :|known_account| T 
               :|device_model| "iphone-8")
 )
```



## Handling Unexpected HTTP Status Code

// TODO

## What is Incognia?

Incognia is a location identity platform for mobile apps that enables:

- Real-time address verification for onboarding
- Frictionless authentication
- Real-time transaction verification

## Create a Free Incognia Account

1. Go to https://www.incognia.com/ and click on "Sign Up For Free"
2. Create an Account
3. You're ready to integrate [Incognia SDK](https://docs.incognia.com/sdk/getting-started) and use [Incognia APIs](https://dash.incognia.com/api-reference)

## License

 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
