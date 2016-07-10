//source: http://stackoverflow.com/questions/11582512/how-to-get-url-parameters-with-javascript

function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}

function getURLParameterDef(name, def) {
  var val = getURLParameter(name);
  if (val)
    return val;
  return def;
}

function init() {
  var mainDiv = document.getElementById('main');
  var language = getURLParameterDef("lang", "en");
  elmContent = Elm.DivineOrBenign.embed(mainDiv);
  // It seems to need the delay to work with Elm 0.17.1. I have no idea why.
  setTimeout(function(){ elmContent.ports.languagePort.send(language); }, 10);

  $(".btn").mouseup(function(){
    $(this).blur();
})
}