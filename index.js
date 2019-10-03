

// Mobile default width
var mobile = document.documentElement.clientWidth <= 700;
                       
// Link to Mapbox                                                                          
mapboxgl.accessToken = 'pk.eyJ1Ijoibm1hcmNoaTAiLCJhIjoiY2p6dTljeDhiMGRwcjNubnl2aXI0OThhYyJ9.4FdGkBJlOXMPRugyqiXrjg';
window.map = new mapboxgl.Map({
  container: "map", // container id
  style: "mapbox://styles/nmarchi0/ck0yada9s02hp1cqhizxmwmlc", 
  center: [12.794268, -1.316134], // starting position  -5.96, 16.89
  zoom: 2,
  maxZoom: 16.5,
  minZoom: 1,
  hash: true
});

// Sidebase mobile adjustment
var sidebar = document.getElementById('sidebar');
if (!mobile) {
  window.map.addControl(new mapboxgl.NavigationControl());
  sidebar.className += " pin-bottomleft";
} else {
  window.map.addControl(new mapboxgl.NavigationControl(), 'bottom-right');
}

// Fly to location buttons
function flyHandler(id, options) {
  var button = document.getElementById(id);
  if(!button) return;
  button.addEventListener('click', function() {
    map.flyTo({
      center: options.center,
      zoom: options.zoom || 10,
      bearing: options.bearing,
      pitch: options.pitch
    });
    if (options.speed) {
      setSpeed(options.speed);
    }
  });
}

flyHandler('sierra-leone', {
  center: [-13.250978, 8.480201],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('nigeria', {
  center: [3.3528302631,6.4650446851],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('liberia', {
  center: [-10.806036, 6.328368],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('south-africa', {
  center: [18.6519186575,-34.0667541339],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('kenya', {
  center: [36.7700362138,-1.318913533],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('zimbabwe', {
  center: [31.12492652, -17.92837714],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('tanzania', {
  center: [39.2139383738,-6.8511294828],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('ghana', {
  center: [-0.2295671004,5.5419797951],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('haiti', {
  center: [-72.336652, 18.538995],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('brasil', {
  center: [-43.2558354349,-22.9931642826],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('nepal', {
  center: [85.344876, 27.699009],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('philippines', {
  center: [120.9526113007,14.6548062731],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('india', {
  center: [72.84803101,19.0365929141],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});
flyHandler('pakistan', {
  center: [66.9894659945,24.9345039855],
  zoom: 12,
  bearing: 0,
  pitch: 0,
  speed: .2
});

// Legend
var layers = ['High access', 'Moderate access', 'Low access', 'Limited access', 'Very limited access'];
var colors = ['#0571b0', '#92c5de', '#f4a582', '#d6604d', '#ca0020'];

for (i = 0; i < layers.length; i++) {
  var layer = layers[i];
  var color = colors[i];
  var item = document.createElement('div');
  var key = document.createElement('span');
  key.className = 'legend-key';
  key.style.backgroundColor = color;

  var value = document.createElement('span');
  value.innerHTML = layer;
  item.appendChild(key);
  item.appendChild(value);
  legend.appendChild(item);
}

// Interactive popups
var title = document.getElementById('location-title');
var description = document.getElementById('location-description');
 
var locations = [
{"id": 1,
  "title": "Why street access matters",
  "description": "Having a street outside one's home is something often taken for granted. Yet for over a billion people in over a million neighborhoods accessing nearby street networks is a daily challenge and can indicate deficits in other types of infrastructure which rely on streets, such as light sources, fire hydrants, power lines, and pipes that provide clean water and sanitation.",
  "camera": {
    center: [12.794268, -1.316134],
    bearing: 0,
    pitch:0,
    zoom: 2
  }
},
{"id": 2,
  "title": "How the map can help",
  "description": "The Million Neighborhoods map attempts to identify these neighborhoods and show how democratizing data at scale provide resources for communities to develop street plans that resolve access deficiencies from the bottom-up. Since the map relies on OpenStreetMap some neighborhoods may be less well documented than others so the accuracy is only as good as what’s currently available.",
  "camera": {
    center: [12.794268, -1.316134],
    bearing: 0,
    pitch:0,
    zoom: 2
  }
},
{"id": 3,
  "title": "Nairobi, Kenya",
  "description": "Insert text popup 1.",
  "camera": {
    center: [36.82287, -1.28937],
    zoom: 12.61,
    pitch: 50
  }
}, {
  "id": 4,
  "title": "Nairobi Central",
  "description": "Insert text popup 2.",
  "camera": {
    center: [36.825969, -1.284919],
    bearing: -8.9,
    zoom: 16.5
  }
}, {
  "id": 4,
  "title": "Kibera neighborhood",
  "description": "Insert text popup 3.",
  "camera": {
    center: [36.794268, -1.316134],
    bearing: 25.3,
    zoom: 16
  }
}, {
  "id": 5,
  "title": "Reblocking Kibera",
  "description": "Insert text popup 4.",
  "camera": {
    center: [36.794268, -1.316134],
    bearing: 25.3,
    zoom: 16.5,
    speed: .05
  }
}, {
  "id": 6,
  "title": "About the project",
  "description": "Something about data, code, history, who's involved, what's next",
  "camera": {
    center: [12.794268, -1.316134],
    bearing: 0,
    pitch:0,
    zoom: 2
  }
}, {
  "id": 7,
  "title": "What the map shows",
  "description": "This map answers the basic question: How hard is it to get from the buildings in a block to the streets around it? In this map red areas have buildings with more limited street access and blue areas have buildings with more street access. The data that underlies the map comes from OpenStreetMap, an open source GIS database crowdsourced from around the world.",
  "camera": {
    center: [12.794268, -1.316134],
    bearing: 0,
    pitch:0,
    zoom: 2
  }
}];


function debounce(func, wait, immediate) {
  var timeout;
  return function executedFunction() {
    var context = this;
    var args = arguments; 
    var later = function() {
      timeout = null;
      if (!immediate) func.apply(context, args);
    };
    var callNow = immediate && !timeout;
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
    if (callNow) func.apply(context, args);
  };
};

function playback(id, index) {
  var button = document.getElementById(id);
  if(!button) return;
  button.addEventListener('click', debounce(function() {
    title.textContent = locations[index].title;
    description.textContent = locations[index].description;
    map.flyTo(locations[index].camera);
    index = ((index + 1) === locations.length) ? 0 : index + 1;
    //map.once('moveend', function() {
    //  window.setTimeout(function() {index = (index + 1 === locations.length) ? 0 : index + 1;playback(index);}, 1000);});
  }, 1000, 500)
  );
}

title.textContent = locations[locations.length - 1].title;
description.textContent = locations[locations.length - 1].description;

playback('play-interactive',0)

