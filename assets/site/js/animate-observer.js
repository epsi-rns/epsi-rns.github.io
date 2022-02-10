// credits and options can be read
// at animate-observer.md

document.addEventListener(
  "DOMContentLoaded", function(event) { 

  // only what I might need, and exclude the rest.
  const effects = [
    'bounce', 'rubberBand', 'wobble', 'swing',
    'rollIn', 'zoomIn', 'flash', 'flip',
    'pulse', 'slideInRight', 'headShake'
  ];

  let elementObserver =
    new IntersectionObserver( (entries) => {
      entries.forEach((entry) => {
        const el = entry.target;
        const animate = el.dataset.animate;

        if (effects.includes(animate)) {
          const effect = 'animate__' + animate;

          if (entry.intersectionRatio > 0) {
            el.classList.add(effect);
          } else {
            el.classList.remove(effect);
          }
        }
      });
  });

  const elementsToObserve = document
    .querySelectorAll(".animate__observe");

  elementsToObserve.forEach((element) => {
    elementObserver.observe(element);
  });
});


