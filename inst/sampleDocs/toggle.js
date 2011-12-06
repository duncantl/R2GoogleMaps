var polylines = [];
var map;

function toggleOverlay(obj, val) {
       if(val)
         map.removeOverlay(obj);
       else
         map.addOverlay(obj);
}
