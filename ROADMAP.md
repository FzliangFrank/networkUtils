# {networkUtils} roadmap

### Reduce Dependency

This package has used a lot of function from `visNetwork`. This is a full implementation of the `visNetwork` package which is build up on `vis.js` javascript library.

In the 'future' we want to depend more on the javascript library itself for more customisation and editing.

As web technology evolve, there are more package to explore. For example `networkD3` package.

### Graph Attribute Editing UI

I have a lot thought about this. An ideal UI design is where selecting a node and editing can exists side by side. 

For perhaps visualize node and edge as an table for easy bulk editing. This is actually much better facilitated when you download as excel sheet and *borrow* excels powerful search and formula engine.

### Version Control 

Graph changes are captured by `$editing` element from output from `mod_visNet_server`. This output can be rendered into UI via `log_timeline_item`


### Multi User support

