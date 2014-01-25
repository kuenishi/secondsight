function format_links(json) {
    //console.log('{"key":0}');
    //var metrics = JSON.parse('{"key":0}');
    //var metrics=j;
    var metrics = JSON.parse(json);
    h = "<ul>";
    for (var key in metrics) {
        if (metrics.hasOwnProperty(key)){
            //console.log(key);
            h += "<li><a onclick='open_graph(\""+key+"\");'>"+key+"</a></li>\n";
        }
    }
    h += "<ul>";
    return h;
}

//alert("hello");
//console.log('hello');
function menu(){
    var r = new XMLHttpRequest();
    r.onload = function() {
        var h = format_links(this.response);
        document.getElementById("links").innerHTML = h;
        return;
    }
    r.open("get", "assets/stats_sample.json", true);
    r.send();
}

function tail(){
    var data = [4, 8, 15, 16, 23, 42, 22];
    d3.select(".chart")
        .selectAll("div")
        .data(data)
        .enter().append("div")
        .style("width", function(d) { return d * 10 + "px"; })
        .text(function(d) { return d; });
}


function draw_graph(name, data){
    var margin = {top: 20, right: 20, bottom: 30, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;
    
    var parseDate = d3.time.format("%d-%b-%y").parse;
    
    var x = d3.time.scale()
        .range([0, width]);
    
    var y = d3.scale.linear()
        .range([height, 0]);
    
    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom");
    
    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left");
    
    var line = d3.svg.line()
        .x(function(d) { return x(d.date); })
        .y(function(d) { return y(d.close); });
    
    var svg = d3.select("body").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
//    d3.tsv("/assets/data2.tsv", function(error, data) {
    //data.forEach(function(d) {
    for(var d in data){
        if (data.hasOwnProperty(d)){
            //d.date = parseDate(d.date);
            d.date = d;
            d.close = data[d];
            //d.close = +d.close;
        }
    };
    
    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain(d3.extent(data, function(d) { return d.close; }));
    
    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);
    
    svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
        .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("Price ($)");
    
    svg.append("path")
        .datum(data)
        .attr("class", "line")
        .attr("d", line);
    //});

}

function open_graph(key){
    console.log(key);
    var r = new XMLHttpRequest();
    r.onload = function() {
        console.log(this);
        var data = JSON.parse(this.response);
        draw_graph(key, data);
        document.getElementById("links").innerHTML = "done";
        return;
    }
    r.open("post", "v1/data/", true);
    r.send("key="+key);
    return;
}