function createMarker(point, html, icon, size)
{
    var baseIcon = new GIcon(G_DEFAULT_ICON);
    if(size)
       baseIcon.iconSize = new GSize(size);
    if(icon)
       baseIcon.image = icon;

          // Set up our GMarkerOptions object
    markerOptions = { icon: baseIcon };
    var marker = new GMarker(point, markerOptions);

    if(html) {
      GEvent.addListener(marker, "click", function() {
          marker.openInfoWindowHtml(html);
      });
    }

    return marker;
}

