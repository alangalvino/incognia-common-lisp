# Incognia API Common Lisp Wrapper
![tests workflow](https://github.com/alangalvino/incognia-wrapper/workflows/.github/workflows/tests.yml/badge.svg)

## How To Use

```lisp
(incognia:configure :client-id "your-client-id"
                    :client-secret "your-client-secret"
                    :region :us)

(incognia:register-signup :installation-id "your-installation-id"
                          :address (incognia:make-address :line "340 Avenue, CA"))

(incognia:register-login :installation-id "your-installation-id"
                         :account-id "your-account-id")

(incognia:register-payment :installation-id "your-installation-id"
                           :account-id "your-account-id"
                           :addresses (list
                                      (incognia:make-address :line "340 Avenue, CA" :type :|home|)
                                      (incognia:make-address :line "500 Street, CA" :type :|billing|)))

(incognia:send-feedback :installation-id "your-installation-id"
                        :event :|verified|
                        :account-id "your-account-id"
                        :timestamp (get-universal-time))
```

## License

 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
