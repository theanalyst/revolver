(require 'ert)
(require 'pushbullet)

(ert-deftest pb/json-extract ()
  (let ((json-object-type 'alist)
        (devices-json "{
                        \"devices\":[
                                   {
                                   \"iden\":19137,
                                   \"extras\":{
                                   \"manufacturer\":\"samsung\",
                                   \"model\":\"Galaxy Nexus\",
                                   \"android_version\":\"4.1.1\",
                                   \"sdk_version\":\"16\",
                                          \"app_version\":\"8\",
                                         \"nickname\":\"Galaxy Nexus\"
                                   }
                                   }
                                   ]
                        }"))
    (should (equal '(19137) (pb/json-extract 'iden 'devices devices-json)))))

(ert-deftest pushbullet-clear-devices ()
  (setq pb/device-ids '(123))
  (pushbullet-clear-devices)
  (should (equal nil  pb/device-ids)))
