let themes = {
  light: null,
  dark: null
};
// theme objects

function setCssVars(theme) {
  Object.entries(theme).forEach(
    ([key, value]) => {
      document.documentElement.style.setProperty(`--col-${key}`, value);
    }
  );
}
// save to css vars

(async () => await Promise.all(Object.entries(themes).map(
  ([key, _]) => fetch(`themes/${key}.json`)
    .then(response => response.json())
    .then(json => {
      themes[key] = json;
      setCssVars(themes.light);
    })
)))();
// load json
