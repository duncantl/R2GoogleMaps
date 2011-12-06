function initialize(file, clat, clong) {
      if (GBrowserIsCompatible()) {
        var map = new GMap2(document.getElementById("map_canvas"));
        map.setCenter(new GLatLng(clat, clong), 12);

        makeMarkers(file, map);
      }
}

function makeMarkers(file, map)
{
        // Download the data in data.xml and load it on the map. The format we
        // expect is:
        // <markers>
        //   <marker lat="37.441" lng="-122.141"/>
        //   <marker lat="37.322" lng="-121.213"/>
        // </markers>
        GDownloadUrl(file, function(data) {
          var xml = GXml.parse(data);
          var markers =
           xml.documentElement.getElementsByTagName("marker");
          for (var i = 0; i < markers.length; i++) {
            var latlng = new GLatLng(parseFloat(markers[i].getAttribute("lat")),
                                     parseFloat(markers[i].getAttribute("lng")));
            map.addOverlay(new GMarker(latlng));
          }
        });
}
