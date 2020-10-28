// This script part is inspired by
// https://nurofsun.github.io/membuat-fitur-pencarian-hugo/

document.addEventListener("DOMContentLoaded", function(event) { 
  // DOM stuff
  const searchBox     = document.getElementById("search_box");
  const searchSite    = document.getElementById("search_site");
  const searchResults = document.getElementById("search_results");

  // URL of the data from the JSON file we generated
  const url  = '/pages/archives.json'
  
  // Get the posts lists in json format.
  const getArchivesJSON = async () => {
    let response = await fetch(url)
    let data = await response.json()
    return data
  }

  function displaySearchResults(items) {
    // Are there any results?
    if (items.length) {
      // Clear any old results
      while(searchResults.firstChild)
        searchResults.removeChild(searchResults.firstChild)

      // Iterate over the results
      items.forEach((item) => {
        // Build a snippet of HTML for this result
        // Then, add it to the results
        searchResults.innerHTML += '<li><a href="'
          + item.url + '">' + item.title + '</a></li>'
      });
    } else {
      searchResults.html('<li>No results found</li>');
    }
  }

  // Event when the form is submitted
  searchSite.addEventListener("submit", (submitEvent) => {
    submitEvent.preventDefault()

    // Get the value for the text field
    searchQuery = searchBox.value

    // trigger async function
    // log response or catch error of fetch promise
    getArchivesJSON()
      .then(data => {
         // Perform a search to an array
         filtered = data.filter(
           item => item.title.includes(searchQuery)
         )

         // Hand the results off to be displayed
         displaySearchResults(filtered);
       })
      .catch(reason => console.log(reason.message))
  }); 

  // Get search results if q parameter is set in querystring
  const urlParams = new URLSearchParams(window.location.search);
  if (urlParams.get('q')) {
      let paramQuery = decodeURIComponent(urlParams.get('q'))
      searchBox.value = paramQuery
      
      // Trigger submit event
      let event = document.createEvent('HTMLEvents');
      event.initEvent('submit', true, true);
      searchSite.dispatchEvent(event);
  }
});
