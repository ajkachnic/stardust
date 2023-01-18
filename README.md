# stardust

A light, playful programming language (*named after the jazz standard*)

## Examples

### Basic http server

```js
import http

fn handler(request) {
  return "hello world"
}

server = http.Server(handler)
server.listen(3000)
```

### http server with routing

```js
import http

router = http.Router()

router.get("/", fn index() {
  return "welcome to the index page"
})

fn about() {
  return "this is the about page"
}

server = http.Server(router.handler())
server.listen(3000)
```

## Design

Stardust's design is based on [it's zen](/ZEN.md)

## Implementation

### Strings

Strings in stardust are stored as copy-on-write. Static strings are interned and if one tries to mutate them, they are copied into a different value which can be manipulated. If strings were pure constants, complex string manipulation would put stress on the garbage collector and waste memory. For example, this code would run quite slow:

```kotlin
import random

alphabet = "qwertyuiopasdfghjklzxcvbnmm"

fun generateId(length) {
  let str = ""
  for i in 0..length {
    str = str .. random.pick(alphabet)
  }
}
```

### Garbage Collection
