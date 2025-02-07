let map;
let currentLayer; // To keep track of the current layer
let acsData; // To hold the ACS data
let genderLayer; // To hold the gender choropleth layer
let affordableHousingLayer;
let affordableHousingLegend;
let genderLegend;
let affordableHousingPointLayer; // Updated variable name
let zipcodeData;
let currentZipLayer;
let timeSlider;

// Function to load geographical data
async function loadGeographies() {
    try {
        console.log("Loading geographies...");
        const wardResponse = await fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/main/wards_simplified.geojson');
        const wardsData = await wardResponse.json();
        console.log("Wards Data:", wardsData); // Log wards data

        const neighborhoodResponse = await fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/main/neighborhoods.geojson');
        const neighborhoodsData = await neighborhoodResponse.json();
        console.log("Neighborhoods Data:", neighborhoodsData); // Log neighborhoods data

        initializeMap();
        await loadACSData(); // Load ACS data
        await loadPointData(); // Ensure point data is loaded here
        await loadzipcodeData(); // Load ZIP data here
        setupGeoOptions(wardsData, neighborhoodsData);
        //setupZIPSlider(); // Set up ZIP slider after loading data
    } catch (error) {
        console.error("Error loading geography data:", error);
    }
}

// Function to load point data
async function loadPointData() {
    try {
        console.log("Loading point data...");
        const pointResponse = await fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/main/affordableHousingPoints.geojson');
        const pointData = await pointResponse.json();
        
        // Create a marker cluster group
        affordableHousingPointLayer = L.markerClusterGroup();

        // Add the points to the cluster group
        L.geoJSON(pointData, {
            onEachFeature: function(feature, layer) {
                layer.bindPopup(`Point Info: ${feature.properties.info}`); // Adjust based on your properties
                affordableHousingPointLayer.addLayer(layer); // Add each layer to the cluster group
            }
        }).addTo(affordableHousingPointLayer); // Ensure points are added to the layer
        map.addLayer(affordableHousingPointLayer); // Add the cluster group to the map
        console.log("affordableHousingPointLayer initialized:", affordableHousingPointLayer); // Log the layer
    } catch (error) {
        console.error("Error loading point data:", error);
    }
}

// Initialize the map
function initializeMap() {
    console.log("Initializing map...");
    map = L.map('map').setView([41.863021, -87.631995], 11);

    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: 'Map data © OpenStreetMap contributors, MapTiler',
    }).addTo(map);

    // Initialize the legends
    genderLegend = document.getElementById('genderLegend');
    affordableHousingLegend = document.getElementById('affordableHousingLegend');
    
    if (genderLegend) genderLegend.style.display = 'none'; // Hide by default
    if (affordableHousingLegend) affordableHousingLegend.style.display = 'none'; // Hide by default
}

// CURRENTLY NOT WORKING: Function to update the legends dynamically
function updateLegend(type) {
    if (type === "percentMale") {
        genderLegend.style.display = 'block';
        affordableHousingLegend.style.display = 'none';
        genderLegend.style.backgroundColor = getGenderColor(0.5); // Example: midpoint color for the legend
    } else if (type === "percentUnitsAffordable") {
        affordableHousingLegend.style.display = 'block';
        genderLegend.style.display = 'none';
        affordableHousingLegend.style.backgroundColor = getaffordableHousingColor(0.03); // Example: midpoint color for the legend
    } else {
        genderLegend.style.display = 'none';
        affordableHousingLegend.style.display = 'none';
    }
}

// Function to load ACS data
async function loadACSData() {
    try {
        console.log("Loading ACS data...");
        const acsResponse = await fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/main/tract.geojson');
        acsData = await acsResponse.json();
    } catch (error) {
        console.error("Error loading ACS data:", error);
    }
}

// Function to load ZIP code data
async function loadzipcodeData() {
    try {
        console.log("Loading ZIP data...");
        const zipResponse = await fetch('https://raw.githubusercontent.com/ducksfloat/cfsc/refs/heads/main/zips.geojson');
        zipcodeData = await zipResponse.json();
        displayzipcodeData(1); // Display the first time period by default
    } catch (error) {
        console.error("Error loading ZIP data:", error);
    }
}

// CURRENTLY NOT WORKING: Function to display ZIP data based on the selected time period
function displayzipcodeData(period) {
    // Remove the existing layer if it exists
    if (currentZipLayer) {
        map.removeLayer(currentZipLayer);
    }

    // Create a new layer for the selected time period
    currentZipLayer = L.geoJSON(zipcodeData, {
        style: feature => styleZIP(feature, period),
        onEachFeature: function(feature, layer) {
            layer.bindPopup(`ZIP: ${feature.properties.zip}<br>Value: ${feature.properties[`period${period}`]}`);
        }
    }).addTo(map);
}

// Function to style the ZIP data based on the selected time period
function styleZIP(feature, period) {
    const value = feature.properties[`period${period}`];
    return {
        fillColor: getZIPColor(value), // Use a function to determine color based on value
        weight: 1,
        opacity: 1,
        color: 'white',
        dashArray: '5',
        fillOpacity: 0.7
    };
}

// Function to determine color based on ZIP data value
function getZIPColor(d) {
    return d > 100 ? '#800026' :
           d > 50  ? '#BD0026' :
           d > 20  ? '#E31A1C' :
           d > 10  ? '#FC4E2A' :
           d > 0   ? '#FD8D3C' :
                      '#FFEDA0';
}

// Function to style the choropleths for overlay layers
function styleGender(feature) {
    return {
        fillColor: getGenderColor(feature.properties.pctMale), // Adjust property name as needed
        weight: 1,
        opacity: 1,
        color: 'white',
        dashArray: '5',
        fillOpacity: 0.7
    };
}

function styleaffordableHousing(feature) {
    return {
        fillColor: getaffordableHousingColor(feature.properties.pctUnitsAffordable), // Adjust property name as needed
        weight: 1,
        opacity: 1,
        color: 'white',
        dashArray: '5',
        fillOpacity: 0.7
    };
}

// Functions for getting colors based on overlay data
function getGenderColor(d) {
    return d < 0.2 ? '#f0f9e8' :
           d >= 0.2 && d < 0.4 ? '#bae4bc' :
           d >= 0.4 && d < 0.6 ? '#7bccc4' :
           d >= 0.6 && d < 0.8 ? '#43a2ca' :
           d >= 0.8  ? '#0868ac' :
                        '#cccccc';
}

function getaffordableHousingColor(d) {
    return d < 0.0166 ? '#edf8fb' :
           d >= 0.0166 && d < 0.0332 ? '#b3cde3' :
           d >= 0.0332 && d < 0.0498 ? '#8c96c6' :
           d >= 0.0498 && d < 0.0664 ? '#8856a7' :
           d >= 0.0664  ? '#810f7c' :
                        '#cccccc';
}

// Set up the geography options in the control panel
function setupGeoOptions(wardsData, neighborhoodsData) {
    console.log("Setting up geography options...");
    const controls = document.getElementById('controls');

    // Clear existing options first
    controls.innerHTML = '';

    // Create and add the header for geography options
    const header = document.createElement('h5');
    header.innerText = 'Selected Geography';
    controls.appendChild(header); // Append the header first

    const wardsOption = createGeoOption('wards', 'Wards');
    controls.appendChild(wardsOption);

    const neighborhoodsOption = createGeoOption('neighborhoods', 'Neighborhoods');
    controls.appendChild(neighborhoodsOption);

    const zipOption = createGeoOption('zip', 'ZIP Codes');
    controls.appendChild(zipOption);

    // Set up event listeners
    wardsOption.addEventListener('click', () => toggleLayer('wards', wardsData));
    neighborhoodsOption.addEventListener('click', () => toggleLayer('neighborhoods', neighborhoodsData));
    zipOption.addEventListener('click', () => toggleLayer('zip', zipcodeData));
}

// Create a geography option button
function createGeoOption(type, name) {
    const button = document.createElement('button');
    button.innerText = name;
    button.className = 'geo-option'; // Apply a class for styling
    button.dataset.type = type; // Store the type in data attributes
    return button;
}

// Toggle the visibility of the specified layer
function toggleLayer(type, data) {
    if (currentLayer) {
        map.removeLayer(currentLayer); // Remove the current layer
    }

    switch (type) {
        case 'wards':
            currentLayer = L.geoJSON(data, {
                style: styleGender,
                onEachFeature: function(feature, layer) {
                    layer.bindPopup(`Ward: ${feature.properties.ward}`);
                }
            }).addTo(map);
            updateLegend("percentMale"); // Update the legend for gender
            break;
        case 'neighborhoods':
            currentLayer = L.geoJSON(data, {
                style: styleaffordableHousing,
                onEachFeature: function(feature, layer) {
                    layer.bindPopup(`Neighborhood: ${feature.properties.name}`);
                }
            }).addTo(map);
            updateLegend("percentUnitsAffordable"); // Update the legend for affordable housing
            break;
        case 'zip':
            displayzipcodeData(1); // Display ZIP data for the first period
            break;
    }
}

// Initialize the map and load data
loadGeographies();
