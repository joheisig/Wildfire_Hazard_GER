// map.js
document.addEventListener("DOMContentLoaded", function () {

    // 1. Initialize Map
    const map = L.map('map', {
        center: [51.1657, 10.4515],
        zoom: 6,
        fadeAnimation: false
    });

    // 2. Basemaps
    const osmLayer = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap'
    }).addTo(map);

    const satelliteLayer = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: 'Tiles © Esri'
    });

    // 3. State
    let activeVariable = 'WildFireHazard';
    let activeMoisture = 'D1L1';
    let activeWind = '97';
    let currentOpacity = 1.0;
    let hideUnburnable = false;
    let germanyBounds = null;

    // The single currently-displayed layer and its georaster
    let activeLayer = null;
    let activeGeoraster = null;

    // Cache PARSED georaster promises by filename.
    // COGs only read headers on parse; pixel data streams per-tile on demand,
    // so caching 27 handles is cheap and makes re-selection instant.
    const georasterCache = new Map();

    // Monotonic token: any async result whose token != requestId is stale
    // and gets discarded. This is what kills the ghosting race condition.
    let requestId = 0;

    // 4. Helpers
    function currentFileName() {
        return `${activeVariable}_class_${activeMoisture}_p_${activeWind}.tif`;
    }

    function createColorFn(georaster) {
        return function (pixelValues) {
            const val = pixelValues[0];
            if (val === georaster.noDataValue || val === 0 || (hideUnburnable && val === 8)) return null;
            if (georaster.palette && georaster.palette[val]) {
                const c = georaster.palette[val];
                return `rgba(${c[0]}, ${c[1]}, ${c[2]}, ${c[3] / 255})`;
            }
            return `rgba(255, 0, 0, 1.0)`;
        };
    }

    function getGeoraster(fileName) {
        if (!georasterCache.has(fileName)) {
            //const url = new URL(`data/cog/${fileName}`, window.location.origin).href;
            const url = new URL(`data/cog/${fileName}`, window.location.href).href;
            
            // store the PROMISE (dedupes concurrent requests for the same file)
            georasterCache.set(fileName, parseGeoraster(url));
        }
        return georasterCache.get(fileName);
    }

    function buildLayer(georaster) {
    return new GeoRasterLayer({
        georaster: georaster,
        resolution: 128,
        opacity: currentOpacity,
        pixelValuesToColorFn: createColorFn(georaster),
        keepBuffer: 0,            // <-- do NOT retain tiles from other zoom levels (kills zoom-back ghosting)
        updateWhenZooming: false
        });
    }

    function updateMap() {
        const token = ++requestId;
        const fileName = currentFileName();

        getGeoraster(fileName).then(georaster => {
            if (token !== requestId) return;          // discard stale async result

            const oldLayer = activeLayer;
            const newLayer = buildLayer(georaster);

            newLayer.addTo(map);

            // THE INSTANT-PAINT FIX:
            // addTo() creates the tiles, but they're not positioned until a view reset.
            // A pan won't do it; only a zoom/viewreset repositions the tile pyramid.
            // Calling _resetView() forces that reposition+repaint immediately — no zoom needed.
            if (typeof newLayer._resetView === 'function') {
                newLayer._resetView();
            } else {
                map._resetView(map.getCenter(), map.getZoom()); // fallback
            }

            activeLayer = newLayer;
            activeGeoraster = georaster;
            if (!germanyBounds) germanyBounds = newLayer.getBounds();

            // Remove the previous layer once the new one has actually drawn (no flash),
            // with a safety net in case 'load' never fires.
            const cleanup = () => {
                if (oldLayer && oldLayer !== activeLayer && map.hasLayer(oldLayer)) {
                    map.removeLayer(oldLayer);
                }
            };
            newLayer.once('load', cleanup);
            setTimeout(cleanup, 1200);

        }).catch(err => console.error("Error loading COG:", err));
    }

    // 6. Controls (unchanged)
    const ActionButtons = L.Control.extend({
        options: { position: 'topleft' },
        onAdd: function () {
            const container = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            const zoomBtn = L.DomUtil.create('a', '', container);
            zoomBtn.innerHTML = '🔍';
            zoomBtn.title = 'Zoom to Extent';
            L.DomEvent.on(zoomBtn, 'click', (e) => { L.DomEvent.stop(e); if (germanyBounds) map.fitBounds(germanyBounds); });

            const fsBtn = L.DomUtil.create('a', '', container);
            fsBtn.innerHTML = '⛶';
            fsBtn.title = 'Fullscreen';
            L.DomEvent.on(fsBtn, 'click', (e) => {
                L.DomEvent.stop(e);
                if (!document.fullscreenElement) document.getElementById('map').requestFullscreen();
                else document.exitFullscreen();
            });
            return container;
        }
    });
    map.addControl(new ActionButtons());

    const DashboardControl = L.Control.extend({
        options: { position: 'topright' },
        onAdd: function () {
            const container = L.DomUtil.create('div', 'custom-dashboard-control');
            container.style.background = 'white';
            container.style.padding = '12px';
            container.style.borderRadius = '5px';
            container.style.width = '240px';
            container.innerHTML = `
                <div style="font-weight: bold; margin-bottom: 5px;">Variable</div>
                <label><input type="radio" name="var-group" value="WildFireHazard" checked> Hazard</label><br>
                <label><input type="radio" name="var-group" value="CondBurnProbability"> Burn Probability</label><br>
                <label><input type="radio" name="var-group" value="CondFlameLength"> Flame Length</label><br>

                <div style="font-weight: bold; margin-top: 10px;">Moisture</div>
                <label><input type="radio" name="moisture-group" value="D1L1" checked> D1L1</label>
                <label><input type="radio" name="moisture-group" value="D2L2"> D2L2</label>
                <label><input type="radio" name="moisture-group" value="D3L3"> D3L3</label>

                <div style="font-weight: bold; margin-top: 10px;">Wind Percentile</div>
                <label><input type="radio" name="wind-group" value="80"> 80th</label>
                <label><input type="radio" name="wind-group" value="90"> 90th</label>
                <label><input type="radio" name="wind-group" value="97" checked> 97th</label>

                <div style="margin-top: 10px;"><input type="checkbox" id="hide-unburnable"> Hide Unburnable</div>
                <div style="margin-top: 10px;">Opacity: <input type="range" id="opacity-slider" min="0" max="1" step="0.1" value="1"></div>
            `;
            L.DomEvent.disableClickPropagation(container);
            return container;
        }
    });
    map.addControl(new DashboardControl());

    // 7. Event Listeners

    // TEMPORARY DIAGNOSTIC — counts how many GeoRasterLayers are alive on the map
    function logLiveLayers(label) {
        setTimeout(() => {
            let count = 0;
            map.eachLayer(l => {
                if (l instanceof GeoRasterLayer) {
                    count++;
                    console.log(label, '→ live GeoRasterLayer, opacity:', l.options.opacity);
                }
            });
            console.log(label, '→ total GeoRasterLayers:', count);
        }, 1500); // wait for the swap to finish
    }

    document.querySelectorAll('input[name="var-group"]').forEach(r => r.addEventListener('change', (e) => {
        activeVariable = e.target.value;
        updateMap();
        logLiveLayers('after variable switch');
    }));
    document.querySelectorAll('input[name="moisture-group"]').forEach(r => r.addEventListener('change', (e) => { activeMoisture = e.target.value; updateMap(); }));
    document.querySelectorAll('input[name="wind-group"]').forEach(r => r.addEventListener('change', (e) => { activeWind = e.target.value; updateMap(); }));

    document.getElementById('hide-unburnable').addEventListener('change', (e) => {
        hideUnburnable = e.target.checked;
         updateMap();   // rebuild -> no stale-zoom tiles survive; consistent everywhere
    });

    document.getElementById('opacity-slider').addEventListener('input', (e) => {
        currentOpacity = parseFloat(e.target.value);
        if (activeLayer) activeLayer.setOpacity(currentOpacity);
    });

    // Initial Load
    updateMap();
});