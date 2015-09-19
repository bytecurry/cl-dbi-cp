# cl-dbi-cp - Connection pool library for cl-dbi

cl-dbi-cp is a fairly simple connection pool library for cl-dbi. It provides thread-safe
connection pool classes that manage connections in a more general way than just using
`dbi:connect-cached`.

If you can think of a better name for this library, I am open to suggestions.

## Usage

There are currently three different classes provided, each with a different
caching strategy.

The `simple-connection-pool` is basically just a stack of connections, and when
the user asks for a connection, it returns the connection at the top of the stack, and
if there aren't any available, creates a new one. It never cleans up old connections.

The `elastic-connection-pool` is similar to the `simple-connection-pool`, but it
has a maximum number of free connections, and if that number is exceeded it will
disconnect any additional connections that are freed.

The `fixed-size-connection-pool` is the strictest pool. It has a fixed number of
connections, and if all of them are in use then calls to `request-connection` will block
until a connection becomes available.

The user-level functions are below.

### **make-simple-connection-pool** _&rest params_

Create a `simple-connection-pool`, with connections created using `(apply #'dbi:connect params)`

### **make-elastic-connection-pool** _max &rest params_

Create an `elastic-connection-pool`, with connections created using `(apply #'dbi:connect params)`.

A maximum of `max` free connections are cached.

### **make-fixed-size-connection-pool** _size &rest params_

Create a `fixed-size-connection-pool`, with connections created using
`(apply #'dbi:connect params)`.

There are a total of `size` connections in the pool.

### **request-connection** _pool_

Request a connection from a connection pool.

### **release-connection** _pool connection_

Return a connnection back to a pool after you are done with it. Note that the user
is responsible for calling this, and _should not_ disconnect the connection, but call this
instead. In the future I may provide a wrapper connection that calls release-connection when
disconnected.

### **with-pooled-connection** _(var pool) &body body_

Executes body with a pooled connection that will be returned to the pool at the end of the
macro.

## Notes:

* It is up to the user to make sure that connections are released to the same pool that they were
requested from.
* All the connections from a single pool are created with the same connect parameters.
* The user is responsible for storing the connection pool, I don't provide a special variable for it.

## Installation

Clone the repo into your local-projects quicklisp directory and run

`(ql:quickload :cl-dbi-cp)`

## Author

* Thayne McCombs (bytecurry.software@gmail.com)

## Copyright

Copyright (c) 2015 Thayne McCombs (bytecurry.software@gmail.com)

## License

Licensed under the MIT License.
