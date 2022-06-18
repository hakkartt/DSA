package cutVertices
import java.io.{ File, FileWriter, IOException }

/**
 * Visualize a graph.
 * Change the call in the "main" method to change visualized graph or property.
 * "Run As" -> "Scala Application" to make the necessary data,
 * then click "Open With" -> "Web Browser" on visualizer/show.html
 */
object visualizer {
  val testGraph = new Graph(60, List((11,53), (9,48), (1,58), (56,23), (44,6), (17,49), (56,9), (54,14), (21,22), (2,29), (41,15), (51,55), (56,52), (17,52), (54,39), (31,37), (36,21), (53,13), (40,42), (25,51), (5,20), (6,56), (35,32), (3,17), (55,57), (47,10), (47,5), (40,33), (0,41), (43,3), (35,50), (35,42), (50,2), (38,59), (12,16), (33,0), (24,19), (52,25), (20,10), (37,34), (59,54), (8,40), (18,38), (23,47), (15,1), (7,14), (51,10), (49,11), (59,5), (13,49), (4,7), (57,4), (28,50), (14,28), (10,31), (27,36), (58,44), (29,26), (34,30), (57,32), (57,45), (22,8), (32,46), (19,43), (39,12), (16,35), (26,27), (42,24)))
  //val testGraph = new Graph(60, List((11,53), (9,48), (1,58), (56,23), (44,6), (17,49), (56,9), (54,14), (21,22), (2,29), (41,15), (51,55), (56,52), (17,52), (54,39), (31,37), (36,21), (53,13), (40,42), (25,51), (5,20), (6,56), (35,32), (3,17), (55,57), (47,10), (47,5), (40,33), (0,41), (43,3), (35,50), (35,42), (50,2), (38,59), (12,16), (33,0), (24,19), (52,25), (20,10), (37,34), (59,54), (8,40), (18,38), (23,47), (15,1), (7,14), (51,10), (49,11), (59,5), (13, 49), (4,7), (57,4), (28,50), (14,28), (10,31), (27,36), (58,44), (29,26), (34,30), (57,32), (57,45), (22,8), (32,46), (19,43), (39,12), (16,35), (26,27), (42,24)))

  def main(args: Array[String]): Unit = {
    val what = 1
    what match {
      case 0 => {
        // Degree of the testGraph above
        visualizeDegree(testGraph)
      }
      case 1 => {
        // Cut vertices of the testGraph above
        visualizeCutVertices(testGraph)
      }
    }
  }

  private def visualizeDegree(g: Graph, names: Map[Int, String] = Map[Int, String]()) = {
    val coloring = (0 until g.nofVertices).map(v => (v, g.degree(v) * 2.0 / g.maxDegree - 1.0)).toMap
    output(g, names, coloring)
    println("""The vertices that have largest degree are red and those with minimum are blue.""")
  }

  private def visualizeCutVertices(g: Graph, names: Map[Int, String] = Map[Int,String]()) = {
    val cutVertices = g.cutVertices
    val coloring = (0 until g.nofVertices).map(v => (v, if(cutVertices.contains(v)) 1.0 else -1.0)).toMap
    output(g, names, coloring)
    println("""The cut vertices are red and the others are blue.""")
  }

  private val htmlPrefix = """<!DOCTYPE html>
<meta charset="utf-8">
<style>
.node {stroke: #bbb; stroke-width: 1.5px; }
.link {stroke: #999; stroke-opacity: .6; }
.graphView {background-color: #eef; width: auto; }
</style>
<title>A simple D3js visualizer</title>
<body>
<p>
This is a simple D3js-based graph visualizer modified from http://bl.ocks.org/mbostock/4062045.
</p>
<p><b>Remember to reload me</b> after running a visualizer object</p>
<svg class="graphView" id="graphView"></svg>
<script src="http://d3js.org/d3.v3.min.js"></script>
<script>
var width = 960, height = 500;
//var color = d3.scale.category20();
var color = d3.scale.linear().domain([-1, 0, 1]).range(["blue", "white", "red"]);
"""
  val htmlPostfix = """var force = d3.layout.force()
    .charge(-120)
    .linkDistance(50)
    .size([width, height]);

var svg = d3.select("#graphView") //.append("svg")
    .attr("width", width)
    .attr("height", height);

force.nodes(graph.nodes).links(graph.links).start();

var link = svg.selectAll(".link")
    .data(graph.links)
    .enter().append("line")
    .attr("class", "link")
    .style("stroke-width", function(d) { return Math.sqrt(d.value); });

var node = svg.selectAll(".node")
    .data(graph.nodes)
    .enter().append("circle")
    .attr("class", "node")
    .attr("r", 10)
    .style("fill", function(d) { return color(d.group); })
    .call(force.drag);

node.append("title")
    .text(function(d) { return d.name; });

var texts = svg.selectAll("text.label")
    .data(graph.nodes)
    .enter().append("text")
    .attr("class", "label")
    .attr("fill", "black")
    .text(function(d) {  return d.name;  });

force.on("tick", function() {
    link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
        
    texts.attr("transform", function(d) {
        return "translate(" + d.x + "," + d.y + ")";});  
});
</script>
</body>
</html>
"""

  private def output(g: Graph, names: Map[Int, String], coloring: Map[Int, Double]) = {
    require(g.nofVertices <= 100, "Drawing large graphs is not recommended with this toolset")

    val htmlFile = new File(new File("visualizer"), "show.html")
    try {
      var ostream: FileWriter = null
      try {
        ostream = new FileWriter(htmlFile)
        ostream.write(htmlPrefix)
        ostream.write("var graph = "+g.toJSON(names, coloring)+";\n")
        ostream.write(htmlPostfix)
      } finally {
        if (ostream != null) ostream.close()
      }
    } catch {
      case e: IOException => throw e
    }
    println("""The graph has now been output to the file visualizer/show.html ("Open With" -> "Web Browser" in eclipse).""")

  }
}
