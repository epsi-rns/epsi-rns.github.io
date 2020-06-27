document.addEventListener("DOMContentLoaded", function(event) { 
  // Check for click events on the navbar burger icon
  const navbarBurger = document.getElementsByClassName("navbar-burger")[0];
  const navbarBulma  = document.getElementById("navbarBulma");

  // Toggle if navbar menu is open or closed
  function toggleMenu() {
    navbarBurger.classList.toggle("is-active");
    navbarBulma.classList.toggle("is-active");
    console.log('Toggle is-active class in navbar burger menu');
  }

  // Event listeners
  navbarBurger.addEventListener('click', toggleMenu, false);
});


