## 0.9.3 (2023-04-10)

* Use Seq.t instead of stream type, memoize default bit streams,
  change init/seed functions, and include stateful 'sampler'
  interface.

## 0.9.2 (2023-04-10)

* API change: include Stream module in root Zar module.
  
## 0.9.1 (2023-04-04)

* Fix qcheck tests for 32-bit systems and reorganize modules and
  interface to use streams.

## 0.9 (2023-04-01)

* Initial release. Includes coin, die, and findist samplers with
  QCheck tests for numeric conversion functions.
