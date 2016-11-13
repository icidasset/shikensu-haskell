# Shikensu

__Work in progress__.

A small toolset for building static websites.


```haskell

import qualified Shikensu

import Shikensu.Types (Definition, Dictionary)
import Shikensu.Contrib (clone, renameExt, permalink)
import Shikensu.Contrib.IO (read, write)


dictionary_io :: IO Dictionary
dictionary_io =
  Shikensu.list ["**/*.html"] absolute_path_to_cwd
    >>= read
    >>= flow
    >>= write "./build"


flow :: IO Dictionary
flow =
     renameExt ".mustache" ".html"
  .> permalink "index"
  .> clone "index.html" "200.html"
  .> copyPropsToMetadata
  .> renderContent (\def -> Markdown.render $ content def)
  .> return
```



### To do

- Support binary data (`content` property)
- Test on Windows (to be clear, it is supported)
