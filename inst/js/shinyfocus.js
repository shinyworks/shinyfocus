// This needs to be available across calls.
var active_element;

function updateFocus(){
  Shiny.setInputValue('shinyfocuspkg-previous_element', active_element);
  active_element = document.activeElement.id;
  Shiny.setInputValue('shinyfocuspkg-active_element', active_element);
}

$(document).on('shiny:connected', function(ev){
  active_element = document.activeElement.id;
  Shiny.setInputValue('shinyfocuspkg-previous_element', active_element);
});

document.addEventListener('focusin', function(){
  updateFocus();
});
