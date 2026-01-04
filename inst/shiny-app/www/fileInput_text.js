$(document).ready(function(){
  $('#uploaded_progress').on("DOMSubtreeModified",function(){

    var target = $('#uploaded_progress').children()[0];
    if(target.innerHTML === "Upload complete"){
        console.log('Change')
        target.innerHTML = 'YOUR TEXT HERE';      
    }

  });
});