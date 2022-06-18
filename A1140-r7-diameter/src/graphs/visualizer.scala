package graphs
import java.io.{ File, FileWriter, IOException }

/**
 * Visualize a graph.
 * Change the call in the "main" method to change visualized graph or property.
 * "Run As" -> "Scala Application" to make the necessary data,
 * then click "Open With" -> "Web Browser" on visualizer/show.html
 */
object visualizer {
  val testGraph = (60,List((39,10), (56,19), (22,7), (30,45), (57,23), (26,35), (18,24), (36,31), (12,43), (5,18), (12,49), (51,38), (23,2), (47,34), (28,5), (50,36), (41,2), (55,49), (42,44), (12,21), (44,50), (58,56), (27,11), (19,10), (18,15), (23,14), (37,47), (10,45), (46,6), (32,8), (48,53), (7,59), (4,30), (54,32), (11,18), (17,4), (1,0), (44,9), (52,39), (26,6), (53,25), (3,22), (10,40), (40,37), (45,55), (30,48), (25,52), (19,20), (9,33), (28,54), (33,51), (38,13), (19,12), (29,57), (34,28), (16,41), (41,42), (49,58), (31,16), (14,26), (20,3), (26,1), (59,27), (6,16), (31,29), (0,17), (8,46)))

  def main(args: Array[String]): Unit = {
    val what = 0
    what match {
      case 0 => {
        // Degree of a small word similarity graph
        val (g, wordToIndex) = wordGraphs.getGraph(wordGraphs.words3common)
        val indexToWord = wordToIndex.map({case (w,i) => (i,w)}).toMap
        visualizeDegree(g, indexToWord)
      }
      case 1 => {
        // Degree of a small social network graph
        val (g, wordToIndex) = fbGraph.getGraph
        val indexToWord = wordToIndex.map({case (w,i) => (i,w)}).toMap
        visualizeDegree(g, indexToWord)
      }
      case 2 => {
        // Eccentricity of a small social network graph
        val (g, wordToIndex) = fbGraph.getGraph
        val indexToWord = wordToIndex.map({case (w,i) => (i,w)}).toMap
        visualizeEccentricity(g, indexToWord)
      }
      case 3 => {
        // Degree of a smallish pseudorandom graph
        val g = new Graph(testGraph._1, testGraph._2)
        visualizeDegree(g)
      }
      case 4 => {
        // Eccentricity of a smallish pseudorandom graph
        val g = new Graph(testGraph._1, testGraph._2)
        visualizeEccentricity(g)
      }
    }
  }

  private def visualizeDegree(g: Graph, names: Map[Int, String] = Map[Int, String]()) = {
    val coloring = (0 until g.nofVertices).map(v => (v, g.degree(v) * 2.0 / g.maxDegree - 1.0)).toMap
    output(g, names, coloring)
    println("""The vertices that have largest degree are red and those with minimum are blue.""")
  }

  private def visualizeEccentricity(g: Graph, names: Map[Int, String] = Map[Int,String]()) = {
    val eccentricity = (0 until g.nofVertices).map(v => g.eccentricity(v)).toArray
    val minEccentricity = eccentricity.min
    val maxEccentricity = eccentricity.max
    val diff = maxEccentricity - minEccentricity
    val coloring = (0 until g.nofVertices).map(v => (v, (eccentricity(v) - minEccentricity) * 2.0 / diff - 1.0)).toMap
    output(g, names, coloring)
    println("""The vertices that have maximum eccentricity are red, those with minimum are blue and average cases are white.""")
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
