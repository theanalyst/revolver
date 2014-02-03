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
                                   ],
                        \"shared_devices\":[
                                          {
                                          \"iden\":7,
                                          \"extras\":{
                                          \"manufacturer\":\"samsung\",
                                          \"model\":\"Galaxy Nexus\",
                                          \"android_version\":\"4.1.1\",
                                          \"sdk_version\":\"16\",
                                          \"app_version\":\"8\"
                                          },
                                          \"owner_name\":\"Alex\"
                                          }
                                          ]
                        }"))
    (should (equal '(19137) (pb/json-extract 'iden 'devices devices-json)))
    (should (equal '(7) (pb/json-extract 'iden 'shared_devices devices-json)))))

(ert-deftest pb/clear-devices ()
  (setq pb/device-id-table (ht ("devices" 123)))
  (pb/clear-devices)
  (should (equal nil (ht-items pb/device-id-table))))
