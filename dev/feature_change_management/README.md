# Change Management Feature

- requires G$editing, a dynmaic list monitor graph change



### Step 1: record a list of change values, and bind for every change

```r
change_log = reactiveValues(log=NULL)
...
observe({
    req(G$editing)
    req(!is.null(G$editing))
    req(G$editing != '')
    change_log$log = isolate(change_log$log) |>
      append(list(list(
        time = Sys.time(),
        change = G$editing
      )))
  })
```
This observer monitor change happen this code chunk 
can bind changes in; (don't know why but double list seems nessessary)

For **commit** changes we can set up two list

```r
# set up two stage change log
change_log <- reactiveValues(
  log=NULL,
  commited=NULL
)

# two seperate change log stating
observeEvent(input$commit, {
  # join change log
  change_log$commited <- isolate(change_log$commited) |> 
    append(list(list(
      change_log$log
    )))
  # empty change log
  change_log$log = list(NULL),
  
})
```

**roll back** feature can be implemented once per commit using the change log?

### Step 2: mutli-user support.

VISION: 

You can implement this via reservation, every-time a user fires up editing model
on a specific joints, this fires up to reserve that join from being edited.

This features turns out to be harder to implement than I thought... because when use
fetch a data, it will be it's own local version of it, hence won't go back into 
main frame. 

You can only do so as... when user fetch a set of data, sent into datbase to
lock it so no one else could edit it. 


## Implementation 

This function here `fct_log_timeline_item` transform one reactive log 
into one `bs4Dash::timelineItem()`. You can apply this function into ui to update
in whatever manner you like. 
This function only passively display time log. It would be useful to add buttons in each timeline Item every time someone press commit button.


