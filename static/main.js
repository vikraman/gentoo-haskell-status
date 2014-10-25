function updatePackages() {
  var xhr = new XMLHttpRequest();
  xhr.open("POST", "/updatePackages");
  xhr.send();
  return false;
}

function updateHackage() {
  var xhr = new XMLHttpRequest();
  xhr.open("POST", "/updateHackage");
  xhr.send();
  return false;
}
