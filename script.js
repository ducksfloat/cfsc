document.addEventListener("DOMContentLoaded", () => {
  // initialize the map and set view
  const map = L.map('map').setView([41.8781, -87.6298], 12); 

  // add OSM tile layer
  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: '© OpenStreetMap contributors'
  }).addTo(map);

  // initialize layer groups for data
  let warehouseLayer = L.layerGroup();
  let rescueLayer = L.layerGroup();
  let neighborhoodLayer = L.layerGroup();

//get pct_n quantiles
  function getQuantiles(values, numQuantiles = 5) {
  values.sort((a, b) => a - b);
  const quantiles = [];
  for (let i = 0; i <= numQuantiles; i++) {
    const idx = Math.floor((i / numQuantiles) * (values.length - 1));
    quantiles.push(values[idx]);
  }
  return quantiles;
}

//get color pallete for pct_n quatile circle markers
const colorScale = [
  '#f7fcfd',
  '#d0ecf2',
  '#a1d5db',
  '#66c2a4',
  '#2ca25f',
  '#006d2c'
];

//create full style function
function getColorByQuantile(value, quantiles, palette) {
  for (let i = 0; i < quantiles.length - 1; i++) {
    if (value <= quantiles[i + 1]) return palette[i];
  }
  return palette[palette.length - 1];
}

// create the quantile legend
function addQuantileLegend(map, quantiles, colorScale) {
  const legend = L.control({ position: 'bottomright' });

  legend.onAdd = function () {
    const div = L.DomUtil.create('div', 'info legend');
    const labels = [];
    
    div.innerHTML += ('<strong>% of total food rescues at location</strong><br>');

    for (let i = 0; i < quantiles.length; i++) {
      div.innerHTML += `
        <i style="background: ${colorScale[i]}; border-radius: 50%; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"></i> 
        ${quantiles[i]} ${quantiles[i + 1] ? '– ' + quantiles[i + 1] + '%' : '+'}<br>
      `;
    }
    
        console.log('Quantiles:', quantiles);
    console.log('Legend HTML:', div.innerHTML);

    return div;
  };

  legend.addTo(map);
}

//get rescue locations and style
map.createPane('rescuePane');
map.getPane('rescuePane').style.zIndex = 650;

fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/geog585_proj/geojson/locations_geocoded.geojson')
  .then(response => response.json())
  .then(data => {
    const pctnValues = data.features
      .map(f => f.properties.pct_n)
      .filter(pct_n => typeof pct_n === 'number' && !isNaN(pct_n));

    const quantiles = getQuantiles(pctnValues, 5);

    const geojsonLayer = L.geoJSON(data, {
      pointToLayer: (feature, latlng) => {
        const pct_n = feature.properties.pct_n;
        const color = getColorByQuantile(pct_n, quantiles, colorScale);
        
        //console.log(`n: ${n}, color: ${color}`);
        
        return L.circleMarker(latlng, {
          pane: 'rescuePane',
          radius: 4,
          fillColor: color,
          color: '#333',
          weight: 1,
          fillOpacity: 0.8
        });
      },
      onEachFeature: (feature, layer) => {
        const name = feature.properties.name || 'Unknown';
        const n = feature.properties.n ?? 'N/A';
        const pct_n = feature.properties.pct_n ?? 'N/A';
        layer.bindPopup(
          `<strong>${name}</strong><br/>
          ${n} rescues this year<br/>
          ${pct_n}% of total rescues this year
          `);
      }
    });

    geojsonLayer.addTo(rescueLayer);

    addQuantileLegend(map, quantiles, colorScale);
  })
  .catch(error => console.error('Error loading locations_geocoded.geojson:', error));
  

  
  // neighborhoods:
  
  //function to create neighborhood flag legend
  function addFoodDistroLegend(map) {
    const legend = L.control({ position: 'bottomright' });

  legend.onAdd = function () {
    const div = L.DomUtil.create('div', 'info legend');
    const labels = [];

    div.innerHTML += '<strong>CFSC Neighborhood Distribution</strong><br>';
    
    const colors = ['lightblue', 'transparent'];
    const labelsText = ['With Food Distribution', 'Without Food Distribution'];

    for (let i = 0; i < colors.length; i++) {
      const borderStyle = colors[i] === 'transparent' ? 'border: 1px solid black; background-color: transparent;' : 'background-color: ' + colors[i];
      div.innerHTML += `
        <i style="width: 15px; height: 15px; display: inline-block; margin-right: 5px; ${borderStyle}"></i>
        ${labelsText[i]}<br>
      `;
    }

    return div;
  };

  legend.addTo(map);
}
  //create neighborhood distro flag styling
      function neighborhoodStyle(feature) {
        if (feature.properties.cfsc_foodDistro_flag === "1") {
          return{
            fillColor: "lightblue",
            weight: 2,
            opacity: 1,
            color: "black",
            fillOpacity: 0.6
          };
        }else if (feature.properties.cfsc_foodDistro_flag === "0") {
          return{
            fillColor: 'transparent',
            weight: 2,
            opacity: 1,
            color: 'black',
            fillOpacity: 0
          };
        } else {
          return{
            fillColor: 'grey',
            weight: 2,
            opacity: 1,
            color: 'black',
            fillOpacity: 0.4
          };
        }
      }
  
    //get neighborhood layer
      fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/geog585_proj/geojson/neighborhoods.geojson')
    .then(response => response.json())
    .then(data => {
      L.geoJSON(data, {
        style: neighborhoodStyle,
        onEachFeature: (feature, layer) => {
          const name = feature.properties.COMMUNITY || 'Unknown';
          layer.bindPopup(`
          <strong>${name}</strong>
          `);
        }
      }).addTo(neighborhoodLayer);
      
      addFoodDistroLegend(map);
    })
    .catch(error => console.error('Error loading warehouseLocations.geojson:', error));
    
//warehouse locations
  fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/geog585_proj/geojson/locations_warehouse.geojson')
    .then(response => response.json())
    .then(data => {
      L.geoJSON(data, {
        pointToLayer: (feature, latlng) => L.marker(latlng),
        onEachFeature: (feature, layer) => {
          const name = feature.properties.name || 'Unknown';
          layer.bindPopup(
          `<strong>${name}</strong>
          `);
        }
      }).addTo(warehouseLayer);
      
    })
    .catch(error => console.error('Error loading warehouseLocations.geojson:', error));

  // add all layers to the map
  warehouseLayer.addTo(map);
  neighborhoodLayer.addTo(map);
  rescueLayer.addTo(map);

  // add layer control
  L.control.layers({}, {
    "Warehouse Locations": warehouseLayer,
    "Rescue Locations": rescueLayer,
    "Neighborhoods": neighborhoodLayer
  }).addTo(map);

  // sidebar toggle
  const sidebar = document.getElementById('sidebar');
  const openBtn = document.getElementById('open-sidebar');
  const closeBtn = document.getElementById('close-btn');
  const container = document.getElementById('container');
  const mapContainer = document.getElementById('map');

 // open sidebar -- shift map content to right
openBtn.addEventListener('click', () => {
  sidebar.classList.add('open');
  container.classList.add('shifted');
  openBtn.style.display = 'none';
  map.invalidateSize(); 
});

// close sidebar -- dynamically shift map back
closeBtn.addEventListener('click', () => {
  sidebar.classList.remove('open');
  container.classList.remove('shifted');
  openBtn.style.display = 'block'; 
  map.invalidateSize(); 
});

  // layer toggle functionality
  function setupLayerToggle(id, layer) {
    const checkbox = document.getElementById(id);
    if (checkbox) {
      checkbox.addEventListener('change', (e) => {
        if (e.target.checked) {
          map.addLayer(layer);
        } else {
          map.removeLayer(layer);
        }
      });
    }
  }

  // toggle checkboxes
  setupLayerToggle('toggle-warehouses', warehouseLayer);
  setupLayerToggle('toggle-rescue-locations', rescueLayer);
  setupLayerToggle('toggle-neighborhoods', neighborhoodLayer);

  // sidebar open on page loads
  sidebar.classList.add('open');
  container.classList.add('shifted');
  openBtn.style.display = 'none'; 
  map.invalidateSize();
  
    // add data button
  const dataNoteButton = document.getElementById('data-note-button');

  dataNoteButton.addEventListener('click', () => {
    const popupContent = `
      <h3>A Note on Data</h3>
      <p>Food rescue data was pulled from the urban canopy warehouse slack channel. R tidyverse package was used to synthesize and clean the data. Manual review was done to pull out like spelling, locations and patterns. 
      There are currently large holes in the data, as there is more room for clenaer manual review. A large amount of internal data with CFSC is learned and shared knowledge. This is not simply quantifiable, however there
      is interested to bring learned knowledge into the map to fill food rescue and distribution holes. 
      Additional boundary data, such as the neighborhood boundaries, was pulled from the City of Chicago open data portal.</p>
    `;
    
    // create a popup div
    const popup = document.createElement('div');
    popup.classList.add('popup');

    // popup content into div
    popup.innerHTML = popupContent;

    // add content to body
    document.body.appendChild(popup);

    // popup close
    const closeButton = document.createElement('button');
    closeButton.textContent = 'Close';
    closeButton.classList.add('popup-close-button');
    popup.appendChild(closeButton);

    closeButton.addEventListener('click', () => {
      popup.remove();
    });
  });

  //style pop up body -- do html in js
  const style = document.createElement('style');
  style.innerHTML = `
    .popup {
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      background-color: white;
      padding: 20px;
      border: 1px solid #ccc;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.2);
      z-index: 1002;
      width: 450px;
      max-width: 80%;
      box-sizing: border-box;
    }

    .popup-close-button {
      background-color: #ff3b3b;
      color: white;
      padding: 5px 10px;
      border: none;
      border-radius: 5px;
      cursor: pointer;
      margin-top: 10px;
      display: block;
      width: 100%;
    }

    .popup-close-button:hover {
      background-color: #cc0000;
    }
  `;
  document.head.appendChild(style);
  
});
