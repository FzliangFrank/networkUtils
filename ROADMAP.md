# {networkUtils} roadmap

Below outline some necessary features for making the application complete.

### Reduce Dependency

This package has used a lot of function from `visNetwork`. This is a full implementation of the `visNetwork` package which is build up on `vis.js` javascript library.

In the 'future' we want to depend more on the javascript library itself for more customization and editing.

As web technology evolve, there are more package to explore. For example `networkD3` package.

### Graph Attribute Editing UI

I have a lot thought about this. An ideal UI design is where selecting a node and editing can exists side by side. 

For perhaps visualize node and edge as an table for easy bulk editing. This is actually much better facilitated when you download as excel sheet and *borrow* excels powerful search and formula engine.

### Version Control 

Graph changes are captured by `$editing` element from output from `mod_visNet_server`. This output can be rendered into UI via `log_timeline_item`.
You can capture this into a reactive value and `map` this. 

Change is captured per command. This is a rough approx of capture change a better way to do may be to capture change every 1 or a few minutes. 

In additional the logic to revert graph change can be even more complicated. 

### Multi User support

This will not be possible without having a back end database. The change could be as complicated as how github manage changes. Because changes could be overlapping on one and another it would be difficult to track which. 

Unless we can disable certain node for editing. For instance, when 
a user press the editing button, the application ask the area they want to editing, this will set request back to database to lock this selected area (reserve this area). So the next user will not be able to edit this area. 

This will put a lot of font-end development stress I think, that is better to be put in javascript rather than a R- backend. Because if we have to store position, view of the nodes in the network. If the server fetch new data, and we simply ask R - server to re-render visNetwork_html output, the layout will also re-render. Hense the user will lose track of which node they are working on. This is ultimitaly frastrating. 




