var latlng = new google.maps.LatLng(1.355655  ,  103.829556);
var options = {
    zoom: 12,
    center: latlng,
    mapTypeId: google.maps.MapTypeId.ROADMAP,
    draggableCursor: "crosshair"
};

var locationsAdded = 1;
var map = new google.maps.Map(document.getElementById("map"), options);
map.controls[google.maps.ControlPosition.TOP_RIGHT].push(FullScreenControl(map));
var points = [];
var markers = [];
google.maps.event.addListener(map, "click", function (location) {
    getLocationInfo(location.latLng, "Location " + locationsAdded);
    locationsAdded++;
});
var directionsDisplay = new google.maps.DirectionsRenderer({ polylineOptions: { strokeColor: "#0000FF" } });

var trafficLayer = new google.maps.TrafficLayer();

trafficLayer.setMap(map)



function addLatLng() {
    var latLong = new google.maps.LatLng($("#lat").val(), $("#lng").val());
    getLocationInfo(latLong, "Location " + locationsAdded);
    locationsAdded++;
    map.setCenter(latLong);
    $("#lat").val("");
    $("#lng").val("");
}

function addLocation() {
    var geocoder = new google.maps.Geocoder();
    geocoder.geocode({ address: $("#location").val() }, function (results) {
        if (results[0]) {
            var result = results[0];
            var latLong = result.geometry.location;

            getLocationInfo(latLong, $("#location").val());
            map.setCenter(latLong);
            $("#location").val("");
        } else {
            alert("Location not found");
        }
    });
}

function getLocationInfo(latlng, locationName) {
    if (latlng != null) {
        var point = { LatLng: latlng, LocationName: locationName };
        points.push(point);
        buildPoints();
    }
}

function clearMarkers() {
    for (var i = 0; i < markers.length; i++) {
        markers[i].setMap(null);
    }
    markers = [];
}

function buildPoints() {
    clearMarkers();

    var html = "";

    for (var i = 0; i < points.length; i++) {
        var marker = new google.maps.Marker({ position: points[i].LatLng, title: points[i].LocationName });
        markers.push(marker);
        marker.setMap(map);

        html += "<tr><td>" + points[i].LocationName + "</td><td>" + roundNumber(points[i].LatLng.lat(), 6) + "</td><td>" + roundNumber(points[i].LatLng.lng(), 6) + "</td><td><button class=\"delete\" onclick=\"removeRow(" + i + ");\">Delete</button></td><td>";
        html += "</td></tr>";
    }

    $("#waypointsLocations tbody").html(html);
}

function clearPolyLine() {
    points = [];
    buildPoints();
    clearRouteDetails();
}

function clearRouteDetails() {
    directionsDisplay.setMap(null);
    directionsDisplay.setPanel(null);
    $("#distance").html("");
    $("#duration").html("");
}

function removeRow(index) {
    points.splice(index, 1);
    buildPoints();
    clearRouteDetails();
}

function moveRowDown(index) {
    var item = points[index];
    points.splice(index, 1);
    points.splice(index + 1, 0, item);
    buildPoints();
    clearRouteDetails();
}

function moveRowUp(index) {
    var item = points[index];
    points.splice(index, 1);
    points.splice(index - 1, 0, item);
    buildPoints();
    clearRouteDetails();
}

function getDirections() {
    var directionsDiv = document.getElementById("directions");

    var directions = new google.maps.DirectionsService();
    directionsDisplay.setMap(map);
    directionsDisplay.setPanel(directionsDiv);

    // build array of waypoints (excluding start and end)
    var waypts = [];
    var end = points.length - 1;
    var dest = points[end].LatLng;
    if (document.getElementById("roundTrip").checked) {
        end = points.length;
        dest = points[0].LatLng;
    }
    for (var i = 1; i < end; i++) {
        waypts.push({ location: points[i].LatLng });
    }

    var routeType = $("#routeType").val();
    var travelMode = google.maps.TravelMode.DRIVING;

    var unitsVal = $("#directionUnits").val();
    var directionUnits = google.maps.UnitSystem.METRIC;
    var optimiseRoute = document.getElementById("optimise").checked;
    var request = {
        origin: points[0].LatLng,
        destination: dest,
        waypoints: waypts,
        travelMode: travelMode,
        unitSystem: directionUnits,
        optimizeWaypoints: optimiseRoute
    };
    directions.route(request, function (result, status) {
        if (status === google.maps.DirectionsStatus.OK) {
            directionsDiv.innerHTML = "";
            directionsDisplay.setDirections(result);

            // calculate total distance and duration
            var distance = 0;
            var time = 0;
            var theRoute = result.routes[0];

            // start KML
            var kmlCode = kmlDocumentStart() + kmlStyleThickLine() + "<Placemark>\n" + kmlLineStart();

            for (var i = 0; i < theRoute.legs.length; i++) {
                var theLeg = theRoute.legs[i];
                distance += theLeg.distance.value;
                time += theLeg.duration.value;

                for (var j = 0; j < theLeg.steps.length; j++) {
                    for (var k = 0; k < theLeg.steps[j].path.length; k++) {
                        var thisPoint = theLeg.steps[j].path[k];
                        kmlCode += roundNumber(thisPoint.lng(), 6) + "," + roundNumber(thisPoint.lat(), 6) + " ";
                    }
                }
                //
            }

            clearMarkers()
            $("#distance").html("Total distance: " + getDistance(distance) + ", ");
            $("#duration").html("total duration: " + Math.round(time / 60) + " minutes");

            // end KML
            kmlCode += kmlLineEnd() + kmlStyleUrl("thickLine") + "</Placemark>\n" + kmlDocumentEnd();
            $("#kmlData").val(kmlCode);
        } else {
            var statusText = getDirectionStatusText(status);
            directionsDiv.innerHTML = "An error occurred - " + statusText;
        }
    });
}

function getDistance(distance) {
    if ($("#directionUnits").val() === "Miles") {
        return Math.round((distance * 0.621371192) / 100) / 10 + " miles";
    } else {
        return Math.round(distance / 100) / 10 + " km";
    }
}