# free-layers
An experiment to layer Free monads and interpreters. The bottom layer is a key value store taken from the Cats documentation. The layer above that is a Users service that manages uses. It has two interpreters, a test interpreter and another that maps to operations in the store layer which can be interpreted by the store's interpreters.
