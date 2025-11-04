var selectedTags = {};

function hideEntryByTag() {
  console.log(selectedTags);
  var noSeleted = true, isSelected;
  var i, j, topic;

  for (i in selectedTags) {
    if (selectedTags[i] === true) {
      noSeleted = false;
      break;
    }
  }

  var topics = document.getElementById("topics").getElementsByTagName('span');
  if (noSeleted) {
    for (i = 0; i < topics.length; i++) {
      topics[i].style.display = 'inline';
    }
  }
  else {
    for (i = 0; i < topics.length; i++) {
      isSelected = false;
      topic = topics[i];
      for (j = 0; j < topic.classList.length; j++) {
        console.log(topic.classList[j], selectedTags[topic.classList[j]]);
        if (selectedTags[topic.classList[j]]) {
          isSelected = true;
          break;
        }
      }
      topic.style.display = isSelected ? 'inline' : 'none';
    }
  }
}

function tagOnClick() {
  var tagClass = this.id;
  selectedTags[tagClass] = !selectedTags[tagClass];
  this.classList.toggle("selected");
  hideEntryByTag();
}

function setTagEvents() {
  var tags = document.getElementById("tags").children;
  var len = tags.length;
  var i;
  for (i = 0; i < len; i++) {
    tags[i].addEventListener("click", tagOnClick);
  }
}

setTagEvents();
