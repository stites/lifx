# Unfinished Haskell bindings to LIFX LAN protocol

Status: Haitus. This was primarily a POC for someone else who, ultimately,
found a completed node library doing the same thing. I am publicising this
solely in case someone else decides to take this from me (I do own some LIFX,
so I might also return to this). The whole thing was under 3 hours of work, I
would treat this repository as one unfinished perspective on how to set up
this kind of project.

Features:
- All of documentation (02/19/2019) has been mapped into datatypes.
- Uses serialise/cborg under the hood because _cborg has a readable binary
  format_ and you can convert the cbor spec to JSON easily (this would make an
  HTTP api trivial, in theory).
- Intended to use udp-streaming since the `streaming` libraries can be easily
  converted into pipes (and maybe conduit) from what I've been told.

TODO:
- [ ] Datatypes other than the header types need `serialise` instances.
- [ ] Need to integrate with udp-streaming
- [ ] Need to expose an HTTP API since there doesn't seem to be too many
      haskellers working with lifx.

