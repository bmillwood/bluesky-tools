# bluesky-tools

This library contains tools that I've developed as part of trying to integrate
an application I'm writing with the [Bluesky](https://bsky.social) social media
platform. It's likely to focus on what's necessary to get my app working in
particular, and neglect other parts of the API.

In principle there is a distinction between the [AT
Protocol](https://atproto.com/) and the Bluesky social network, but I haven't
attempted to make that distinction here.

## Version conventions

I never know what to do with version numbers between releases, so my convention
for this library will be:

* Released versions A.B.C.D will have B, C, and D all even.
* Between releases, if I make a major change, I bump B by one (making it
  odd) and zero C and D. Similarly, if I make a minor change (and haven't made a
  major change yet), I bump C by one and zero D. It's not necessary to bump
  anything if the corresponding component is already odd.
* Versions with odd components (apart from the first, which I bump according to
  my whims) therefore may exist in many different Git revisions, potentially
  with breaking changes between them, and are ambiguous. This is fine, because
  they are never released.
* When it comes time to release, I bump any odd component up to the next even
  component.
