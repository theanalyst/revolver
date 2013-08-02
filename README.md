Revolver
==========

  Revolver is an emacs client to push bullet(s).

  [Pushbullet](https://pushbullet.com) is an Android app that can be
  used to *push* notes,url and other stuff that resembles these things
  from desktops and browsers etc. to your Android phone. Luckily
  Pushbullet provides an [API](https://pushbullet.com/api) which we
  can use to push items from your favorite text editor to your phone.

## Usage

  Set the variable `pushbullet-api-key` to the API key for your
  account

  At present after loading `pushbullet.el`, selecting a region and
  calling M-x `pb/send-region` will prompt for a post title which will
  send the selection along with the title as a note to your android
  device.

  I'm very much a rookie when it comes to Emacs Lisp,
  comments\free PRs\brickbats are always welcome.
