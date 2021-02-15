# Common Lisp Incognia APIs Wrapper
![tests workflow](https://github.com/alangalvino/incognia-wrapper/workflows/.github/workflows/tests.yml/badge.svg)

Simple as:

```lisp
(authenticate '("client-id" . "client-secret")) ;; or create a credentials.yml file

(incognia:signups :installation-id "your-installation-id"
                  :address-line "your-address-line")
```

## License

 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
