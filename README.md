# Shīkensu

> シーケンス    
> Sequence

Run a sequence of functions on in-memory representations of files.  
__Build static websites with ease__, without conforming to a specific structure.

[![](https://www.stackage.org/package/shikensu/badge/nightly?label=version)](https://www.stackage.org/package/shikensu)
[![](https://travis-ci.org/icidasset/shikensu.svg?branch=master)](https://travis-ci.org/icidasset/shikensu)



### Markdown example


```haskell
import Shikensu
import Shikensu.Contrib (clone, copyPropsToMetadata, permalink, renameExt, renderContent)
import Shikensu.Contrib.IO (read, write)

import Data.ByteString (ByteString)
import Flow
import Prelude hiding (read)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)


dictionary_io :: IO Dictionary
dictionary_io =
    Shikensu.listRelative ["src/**/*.md"] "./"
        >>= read
        >>= flow
        >>= write "./build"


flow :: Dictionary -> IO Dictionary
flow =
       renameExt ".md" ".html"
    .> permalink "index"
    .> clone "index.html" "200.html"
    .> copyPropsToMetadata
    .> renderContent markdownRenderer
    .> return


markdownRenderer :: Definition -> Maybe ByteString
markdownRenderer def =
    content def
        |> fmap Text.decodeUtf8
        |> fmap Markdown.render
        |> fmap Text.encodeUtf8
```

This lists all the markdown files in the `./src` directory  
and then does the following in this exact order:

1. Change the extension of each matched file to `.html`.
2. Change the path of each matched file to `%matched_path/%file_basename/index.%file_extension`.
3. Make an in-memory copy of the file that has the `index.html` path.
4. Copy the information of each definition (aka. file) to its metadata "object"
   (so we can use that information in our content renderer).
5. Map the `content` property of each definition
   (in this case we are rendering markdown).
6. __{post-flow}__ Write to the `./build` directory.



### Why?

Because this allows me to easily define a workflow for building a static website, and more specifically:

- Have a clear overview of what's happening.
- Do a bunch of actions in memory and then write it to disk in one go.
- Have a list of information about the other files in the project which can then be shared with, for example, templates.



### Usage examples

- [Simple example](https://github.com/icidasset/ongaku-ryoho/blob/47139dd903494beccb9d18bb23261ae85f7d510e/system/Main.hs#L17)
- [Slightly more complicated example](https://github.com/icidasset/icidasset/blob/f6acd52525abe38286aebe915dd4e97a9d3add33/system/Build/Main.hs#L33)
