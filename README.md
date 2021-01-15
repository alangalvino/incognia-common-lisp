# Common Lisp Incognia APIs Wrapper

Create a credentials.yaml file like this:

```lisp
client-id: <credentials-client-id>
secret: <credentials-secret>
```

Now you're ready to:

```lisp
(incognia-apis:onboarding-signups "your-installation-id"
                                  "your-address-line")
```

Simple, isn't?


## License

MIT
