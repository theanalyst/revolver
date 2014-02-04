Revolver
==========

  Revolver is an emacs client to push bullet(s).

  [Pushbullet](https://pushbullet.com) is an Android app that can be
  used to *push* notes,url and other stuff that resembles these things
  from desktops and browsers etc. to your Android phone. Luckily
  Pushbullet provides an [API](https://pushbullet.com/api) which we
  can use to push items from your favorite text editor to your phone.
  HTTP Requests are created using the
  [grapnel](https://github.com/leathekd/grapnel) library

## Usage

  Set the variable `pushbullet-api-key` to the API key for your
  account

  At present after loading `pushbullet.el`, selecting a region and
  calling M-x `pushbullet` will send the region as a note prompting
  for a title, if region is inactive entire buffer is sent

## Note for Shared Devices

  Ideally `C-u M-x pushbullet` should be able to push to shared
  devices, however after recent api changes, I'm not recieving
  anything in the shared devices in the json response. If this is
  working for anyone, do let me know.

## TODO

- Write tests, tests are good
- Convert org-mode structure into a todo list and send
