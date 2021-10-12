(in-package :cl-incognia)

(deftype feedback-event-type () '(member
                                  :|signup_accepted|
                                  :|signup_declined|
                                  :|payment_accepted|
                                  :|payment_accepted_by_third_party|
                                  :|payment_declined|
                                  :|payment_declined_by_risk_analysis|
                                  :|payment_declined_by_manual_review|
                                  :|payment_declined_by_business|
                                  :|payment_declined_by_acquirer|
                                  :|login_accepted|
                                  :|login_declined|
                                  :|verified|
                                  :|not_verified|
                                  :|account_takeover|
                                  :|chargeback|
                                  :|mpos_fraud|))

(defun send-feedback (&key timestamp event installation-id account-id)
  (check-type event feedback-event-type)

  (do-auth-request
    :uri (incognia-uri *feedbacks-uri*)
    :method :post
    :body (to-json (list :|timestamp| timestamp
                         :|event| event
                         :|installation_id| installation-id
                         :|account_id| account-id))))

(defun get-signup-assessment (&key signup-id)
  (assert signup-id)

  (do-auth-request
    :uri (concatenate 'string  (incognia-uri *signups-uri*) "/" signup-id)
    :method :get))

(defun register-signup (&key installation-id address app-id)
  (assert installation-id)

  (let* ((request-body (append
                        (list :|installation_id| installation-id)
                        (and app-id (list :|app_id| app-id))
                        (and address (addr-line-validp address) (addr-line-plist address))
                        (and address (addr-coordinates-validp address) (addr-coordinates-plist address))
                        (and address (addr-structured-validp address) (addr-structured-plist address)))))

    (do-auth-request
      :uri (incognia-uri *signups-uri*)
      :method :post
      :body (to-json request-body))))

(defun register-transaction (&key installation-id account-id type app-id external-id addresses payment-value payment-methods)
  (assert (and installation-id account-id type))

  (let* ((request-body (append
                        (list :|installation_id| installation-id)
                        (list :|account_id| account-id)
                        (list :|type| type)
                        (and app-id (list :|app_id| app-id))
                        (and external-id (list :|external_id| external-id))
                        (and payment-value (list :|payment_value| payment-value))
                        (and payment-methods (list :|payment_methods| payment-methods))
                        (and addresses (mapcar #'addr-plist addresses)))))

    (do-auth-request
      :uri (incognia-uri *transactions-uri*)
      :method :post
      :body (to-json request-body))))

(defun register-login (&key installation-id account-id app-id)
  (register-transaction :installation-id installation-id
                        :account-id account-id
                        :type :|login|
                        :app-id app-id))

(defun register-payment (&key installation-id account-id app-id external-id addresses payment-value payment-methods)
  (register-transaction :installation-id installation-id
                        :account-id account-id
                        :external-id external-id
                        :payment-value payment-value
                        :payment-methods payment-methods
                        :addresses addresses
                        :type :|payment|
                        :app-id app-id))
