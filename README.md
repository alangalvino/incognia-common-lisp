# Common Lisp Incognia APIs Wrapper

Create a credentials.yaml file like this (or fill the :credentials argument):

```lisp
client-id: <credentials-client-id>
secret: <credentials-secret>
```

Now you're ready to:

```lisp
(incognia-apis:onboarding-signups :installation-id "your-installation-id"
                                  :address-line "your-address-line"
                                  :credentials '("client" . "secret")) ;; or create a credentials.yml file
```

Simple, isn't?


## License

MIT
