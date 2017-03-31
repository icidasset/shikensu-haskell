# Shīkensu

> シーケンス    
> Sequence

A small toolset for building static websites.


```haskell
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Shikensu

import Data.ByteString (ByteString)
import Flow
import Prelude hiding (read)
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO (read, write)


dictionary_io :: IO Dictionary
dictionary_io =
    Shikensu.list ["src/**/*.md"] absolute_path_to_cwd
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



### Why?

Because this allows me to easily define a workflow for building a static website, and more specifically:

- Have a clear overview of what's happening.
- Do a bunch of actions in memory and then write it to disk in one go.
- Have a list of information about the other files in the project which can then be shared with, for example, templates.



### To do

- Test on Windows (to be clear, it is supported)
