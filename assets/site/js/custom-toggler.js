document.addEventListener("DOMContentLoaded", function(event) { 

  const leftToggler  = document.getElementById("left_toggler");
  const rightToggler = document.getElementById("right_toggler");

  leftToggler.onclick = function() {
    leftToggler.classList.toggle("left_toggler_active");
 
    toggleLeftIcon();
    toggleLeftLayout(); 
    fixGap();

    return false;
  }

  rightToggler.onclick = function() {
    rightToggler.classList.toggle("right_toggler_active");

    toggleRightIcon();
    toggleRightLayout();
    fixGap();

    return false;
  }

  function toggleLeftIcon() {
    const isActiveLeft = leftToggler.classList
      .contains("left_toggler_active");

    const leftIcon     = leftToggler
      .getElementsByTagName("i")[0];
    
    if (isActiveLeft) {
      leftIcon.classList.remove("fa-angle-double-right");
      leftIcon.classList.add("fa-angle-double-left");

      console.log("Left toggler class is active");
    } else {
      leftIcon.classList.remove("fa-angle-double-left");
      leftIcon.classList.add("fa-angle-double-right");

      console.log("Left toggler class is inactive");
    }
  }

  function toggleRightIcon() {
    const isActiveRight = rightToggler.classList
      .contains("right_toggler_active");

    const rightIcon     = rightToggler
      .getElementsByTagName("i")[0];
  
    if (isActiveRight) {
      rightIcon.classList.remove("fa-angle-double-left");
      rightIcon.classList.add("fa-angle-double-right");

      console.log("Right toggler class is active");
    } else {
      rightIcon.classList.remove("fa-angle-double-right");
      rightIcon.classList.add("fa-angle-double-left");

      console.log("Right toggler class is inactive");
    }
  }

  // Toggle maxwidth feature class
  // Enable/disable to use full width wide screen
  function toggleLeftLayout() {
    const maxWidthTogglers = document
      .getElementsByClassName("maxwidth_toggler");

    // ECMAScript 2015 
    for (let mwt of maxWidthTogglers) {
      mwt.classList.toggle("maxwidth");
    }

    leftToggler.blur();
  }

  // Toggle sidebar
  // Enable/disable full width of content on tablet screen or beyond
  function toggleRightLayout() {
    const isActiveRight = rightToggler.classList
      .contains("right_toggler_active");

    const mainToggler  = document.getElementById("main_toggler");
    const asideToggler = document.getElementById("aside_toggler");
  
    if (isActiveRight) {    
      mainToggler.classList.add("is-two-thirds");
      mainToggler.classList.remove("is-full");

      asideToggler.classList.remove("is-hidden-tablet"); 
    } else {
      mainToggler.classList.add("is-full");
      mainToggler.classList.remove("is-two-thirds");

      asideToggler.classList.add("is-hidden-tablet");
    }

    rightToggler.blur();
  }

  // Fix gap between two columns
  function fixGap() {
    const isActiveLeft = leftToggler.classList
      .contains("left_toggler_active");
    const isActiveRight = rightToggler.classList
      .contains("right_toggler_active");

    const mainToggler  = document.getElementById("main_toggler");
    const asideToggler = document.getElementById("aside_toggler");

    mainToggler.classList.remove("p-r-0");
    asideToggler.classList.remove("p-l-0");

    if (!isActiveLeft && isActiveRight) {
      mainToggler.classList.add("p-r-0");
      asideToggler.classList.add("p-l-0");
    }
    
    console.log("Fix gap class.");
  }

});
