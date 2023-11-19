#' Access ID of Vis Network Object
#' @param id shiny id
#' @export
visNetId = function(id) {
  a_id=paste0(id,'-','visNetworkId')
}
#' Access ID of Vis Network Object
#' @param id shiny id
#' @export
vizNetId = function(id) {
  visNetId(id)
}
#' Javascript tha that help Resize Widget in `bs4Dash::box`
#' @param id shiny id
#' @param offset number of pixel to offset from main screen
#' @export
maximise_helper=function(id, offset=180) {
  maximize_helper(id,offset)
}
#' Javascript tha that help Resize Widget in `bs4Dash::box`
#' @param id shiny id
#' @param offset number of pixel to offset from main screen
#' @export
maximize_helper=function(id, offset = 180) {
  HTML(sprintf(r'(
  // Access JS Object
  var target=document.getElementById('%s');
  var cardBody=target.parentElement
  var card = cardBody.parentElement
  var cardHeader = card.getElementsByClassName('card-header')[0]
  var cardTool = cardHeader.getElementsByClassName('card-tools')[0]
  var maximizeBtn = cardTool.querySelectorAll('[data-card-widget="maximize"]')[0]

  console.log("find btn", maximizeBtn)
  let maximized=false
  const ogHeight = target.offsetHeight

  maximizeBtn.addEventListener('click', function(){
      maximized = !maximized
      console.log('button is clicked')
      if(maximized) {
          console.log('try to maiximise target')
          const h = window.innerHeight - %i
          target.style.height = h + 'px'
      } else {
          console.log('try to reset target')
          target.style.height = ogHeight + 'px'
      }
  })
  )',id, offset))
}
