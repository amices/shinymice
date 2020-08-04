// define css theme filepaths
const themes = {
    dark: 'shinythemes/css/darkly.min.css',
    light: 'shinythemes/css/flatly.min.css'
}

// function that creates a new link element
function newLink(theme) {
    let el = document.createElement('link');
    el.setAttribute('rel', 'stylesheet');
    el.setAttribute('text', 'text/css');
    el.setAttribute('href', theme);
    return el;
}

// function that remove <link> of current theme by href
function removeLink(theme) {
    let el = document.querySelector(`link[href='${theme}']`)
    return el.parentNode.removeChild(el);
}

// define vars
const darkTheme = newLink(themes.dark);
const lightTheme = newLink(themes.light);
const head = document.getElementsByTagName('head')[0];
const toggle = document.getElementById('themeToggle');

// define extra css and add as default
const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {color: grey!important;} .paginate_button {background: white!important;} thead {color: grey;}'
const extraDarkThemeElement = document.createElement('style');
extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
head.appendChild(extraDarkThemeElement);


// define event - checked === 'light'
toggle.addEventListener('input', function(event) {
    // if checked, switch to light theme
    if (toggle.checked) {
        // else add darktheme
        removeLink(themes.light);
        head.appendChild(extraDarkThemeElement)
        head.appendChild(darkTheme);
    }  else {
        removeLink(themes.dark);
        head.removeChild(extraDarkThemeElement);
        head.appendChild(lightTheme);
    }
});

//                 # make options appear on hover, see https://stackoverflow.com/questions/34597421/dropdown-bootstrap-menu-with-unfold-effect

$('.navbar .dropdown').hover(function() {
                $(this).find('.dropdown-menu').first().stop(true, true).slideDown();
            }, function() {
                $(this).find('.dropdown-menu').first().stop(true, true).slideUp()
            });