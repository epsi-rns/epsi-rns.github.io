document.addEventListener("DOMContentLoaded", function(event) { 

  const leftResizer  = document.getElementById("left_resizer");
  const rightResizer = document.getElementById("right_resizer");

  leftResizer.onclick = function() {
    leftResizerToggler();    
    return false;
  }

  rightResizer.onclick = function() {
    rightResizerToggler();    
    return false;
  }

  // Toggle maxwidth feature class
  // Enable/disable to use full width wide screen
  function leftResizerToggler() {
    leftResizer.classList
      .toggle("left_resizer_active");

    const isActive = leftResizer.classList
      .contains("left_resizer_active");

    const leftIcon = leftResizer
      .getElementsByTagName("i")[0];

    if (isActive) {
      leftIcon.classList.remove("fa-angle-double-right");
      leftIcon.classList.add("fa-angle-double-left");
      console.log("left resizer class is active");
    } else {
      leftIcon.classList.remove("fa-angle-double-left");
      leftIcon.classList.add("fa-angle-double-right");
      console.log("left resizer class is inactive");
    }

    const maxWidthResizers = document
      .getElementsByClassName("maxwidth_resizer");

    var i=0;
    while (i < maxWidthResizers.length) {
      maxWidthResizers[i].classList.toggle("maxwidth");
      i++;
    }
    
    leftResizer.blur();
  }

  // Toggle sidebar
  // Enable/disable full width of content on tablet screen or beyond
  function rightResizerToggler() {
    rightResizer.classList
      .toggle("right_resizer_active");

    const isActive   = rightResizer.classList
      .contains("right_resizer_active");

    const rightIcon    = rightResizer.getElementsByTagName("i")[0];
    const mainResizer  = document.getElementById("main_resizer");
    const asideResizer = document.getElementById("aside_resizer");
  
    if (isActive) {
      rightIcon.classList.remove("fa-angle-double-left");
      rightIcon.classList.add("fa-angle-double-right");
      
      mainResizer.classList.add("is-two-thirds");
      mainResizer.classList.remove("is-full");

      asideResizer.classList.remove("is-hidden-tablet"); 

      console.log("right resizer class is active");
    } else {
      rightIcon.classList.remove("fa-angle-double-right");
      rightIcon.classList.add("fa-angle-double-left");

      mainResizer.classList.add("is-full");
      mainResizer.classList.remove("is-two-thirds");

      asideResizer.classList.add("is-hidden-tablet");

      console.log("right resizer class is inactive");
    }
    
    rightResizer.blur();
  }

});
