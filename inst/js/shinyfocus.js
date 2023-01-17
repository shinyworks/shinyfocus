function updateFocus(){
  let active_element = document.activeElement.id;
  Shiny.setInputValue('shinyfocuspkg-active_element', active_element);
}

$(document).on('shiny:connected', function(ev){
  updateFocus();
});

document.addEventListener('focusin', function(){
  updateFocus();
});
