(require 'ert)
(require 'pushbullet)

(ert-deftest pb/extract-device-ids ()
  (let ((devices-json "	{
			\"devices\":[
				   {
				   \"id\":19137,
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
					  \"id\":7,
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
    (should (equal '(19137) (pb/extract-device-ids devices-json)))))

