# Common Lisp Incognia APIs Wrapper

Create a credentials.yaml file like this (or fill the :credentials argument):

```lisp
client-id: <credentials-client-id>
client-secret: <credentials-client-secret>
```

Now you're ready to:

```lisp
(authenticate '("client-id" . "client-secret")) ;; or create a credentials.yml file

(incognia-apis:onboarding-signups :installation-id "your-installation-id"
                                  :address-line "your-address-line")
```

Simple, isn't?


## License

MIT
