# super-size-png
Increase your PNG file size for testing

PNG file is built from chunks which are either important data (like image data)
or some meta data which is not that important. Chunks can be of arbitrary size
(there is a limit in the spec of course) and we can use that fact to insert an
extra chunk with meta data to the input PNG file to increase its size.

You can read about the PNG header and its chunks:
- http://www.libpng.org/pub/png/spec/1.1/PNG-Rationale.html#R.PNG-file-signature
- http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html

Running the code:

    cabal sandbox init
    cabal install --dependencies-only
    cabal run -- -i input.png -s 100 -o output.png

As a result of the executed command your output.png will have an extra of 100
bytes in its body but the viewer will still show you the same picture as if you
opened input.png

There are some shortcuts taken in the implementation and there might be some
cases where the program misbehaves. But it works in most situations so it is
fine for now.
