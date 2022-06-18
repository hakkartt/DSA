/* Author: Tatu Timonen, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

/*
 * Reference:
 * http://web.archive.org/web/20071224095835/http://www.openasthra.com/wp-content/uploads/2007/12/binary_trees1.c
 */

package bst

import scala.swing._
import event._
import java.awt.Color
import java.awt.Toolkit
import util.matching.Regex
import collection.mutable.{ArrayStack, HashMap}
import java.lang.StringBuilder

protected class GUI extends MainFrame {
  
  trait Direction
  private object RootDir extends Direction
  private object LeftDir extends Direction
  private object RightDir extends Direction
  
  case class NodeAux(var asString: String, var parentDir: Direction = RootDir,
                                           var edgeLength: Int = 0,
                                           var textHeight: Int = 0)

  val tk = Toolkit.getDefaultToolkit
  val ppi = tk.getScreenResolution
  val ppiRef = 113.0f;
  val scale = ppi / ppiRef;
                                           
  preferredSize = new Dimension((800 * scale).toInt, (600 * 1.1f * scale).toInt)
  minimumSize = preferredSize
  private val textAreaFont = new Font("monospaced", 0, (15 * scale).toInt)
  private val uiFont = new Font("monospaced", 0, (13 * scale).toInt)
  private val colorSucc = new Color(0,128,0)
  private val colorErr = new Color(178,34,34)
  private val tree = new TreeMap[Int,Int]()
  private val nodeInfo = HashMap[Int, NodeAux]()
  private val renderMax = 2 << 8 // 512
  private val infty = Int.MaxValue >> 4 // 134217727
  private val leftProfile, rightProfile = Array.fill(renderMax)(0)
  
  private val infoField = new Label {
    font = uiFont
    text = "Input: an integer--key-value pair separated by anything but numerals or '-' characters"
  }
  private val infoField2 = new Label {
    font = uiFont
    text = "(The key and value may consist of a maximum of 8 digits each; value defaults to 0 if not provided)"
    foreground = Color.gray
  }
  private val inputField = new TextField {
    def reset(): Unit = text = ""
    font = uiFont
    minimumSize = new Dimension((200 * scale).toInt, (30 * scale).toInt)
    maximumSize = minimumSize
  }
  private def parseInput: Either[Throwable, (Int,Int)] = {
    val pattern = new Regex("(-)?[0-9]{1,8}([^-0-9]+(-)?[0-9]{1,8})?")
    pattern.findFirstIn(inputField.text)
           .map(_.split("[^-0-9]+")
                 .map(_.toInt))
           .map{case Array(_1,_2) => (_1,_2)
                case Array(_1)    => (_1, 0)}
            match {
              case Some(p) => Right(p)
              case None => Left(SoftException("Invalid input"))
            }
  }
  
  private val currentLabel = new Label {
    font = uiFont
    text = "Current"
  }
  private val prevLabel = new Label {
    font = uiFont
    text = "Previous"
  }
  private val treeTextArea = new TextArea {
    val defaultRows = 27
    val defaultCols = 30
    def reset(): Unit = text = ""
    rows = defaultRows
    columns = defaultCols
    font = textAreaFont
    lineWrap = false
    editable = false
  }
  private val treeTextAreaPrev = new TextArea {
    val defaultRows = 27
    val defaultCols = 30
    rows = defaultRows
    columns = defaultCols
    font = textAreaFont
    lineWrap = false
    editable = false
  }
  
  private def renderTree(): Unit = {
    treeTextAreaPrev.text = treeTextArea.text
    val node = tree.root
    if (node == null) {
      treeTextArea.reset()
      return
    }
    val stack = ArrayStack[Node[Int,Int]](node)
    while (stack.nonEmpty) {
      val node = stack.pop()
      if (node.hasRight) { nodeInfo(node.right.key).parentDir = LeftDir; stack.push(node.right) }
      if (node.hasLeft) { nodeInfo(node.left.key).parentDir = RightDir; stack.push(node.left) }
    }
    computeEdgeLengths(node)
    (0 until (nodeInfo(node.key).textHeight min renderMax)).foreach(leftProfile(_) = infty)
    computeProfiles(node, LeftDir)
    val c = (0 until (nodeInfo(node.key).textHeight min renderMax)).foldLeft(0) { (res,i) =>
      res min leftProfile(i)
    }
    val treeStrb = new StringBuilder
    var colNext = 0
    (0 until nodeInfo(node.key).textHeight).foreach { l =>
      treeStrb append buildLevelWithEdges(node, l, -c)
      colNext = 0
    }
    def buildLevelWithEdges(node: Node[Int,Int], lvl: Int, col: Int): StringBuilder =  {
      val strb = new StringBuilder
      case class Frame(n: Node[Int,Int], pd: Direction, l: Int, c: Int)
      val stack = ArrayStack[Frame](Frame(node, RootDir, lvl, col))
      while (stack.nonEmpty) {
        val f = stack.pop()
        if (f.n != null) {
          val adj = nodeInfo(f.n.key).parentDir match { case LeftDir => 1; case _ => 0 }
          if (f.l == 0) {
            val nofSpaces = f.c - colNext - (nodeInfo(f.n.key).asString.length() - adj)/2
            strb append " " * nofSpaces + nodeInfo(f.n.key).asString
            colNext += nofSpaces + nodeInfo(f.n.key).asString.length()
          }
          else if (nodeInfo(f.n.key).edgeLength >= f.l) {
            if (f.n.hasLeft) {
              val nofSpaces = f.c - colNext - f.l
              strb append " " * nofSpaces + "/"
              colNext += nofSpaces + 1
            }
            if (f.n.hasRight) {
              val nofSpaces = f.c - colNext + f.l
              strb append " " * nofSpaces + "\\"
              colNext += nofSpaces + 1
            }
          }
          else {
            stack.push(Frame(f.n.right, LeftDir, 
                             f.l - nodeInfo(f.n.key).edgeLength - 1,
                             f.c + nodeInfo(f.n.key).edgeLength + 1))
            stack.push(Frame(f.n.left, RightDir, 
                             f.l - nodeInfo(f.n.key).edgeLength - 1,
                             f.c - nodeInfo(f.n.key).edgeLength - 1))
          }
        }
      }
      strb append '\n'
    }
    treeTextArea.text = treeStrb.toString()
  }
  private def computeEdgeLengths(node: Node[Int,Int]): Unit = {
    case class Frame(n: Node[Int,Int], pd: Direction, var leftDone: Boolean = false,
                                                      var rightDone: Boolean = false)
    val stack = ArrayStack[Frame](Frame(node, RootDir))
    while (stack.nonEmpty) {
      val f = stack.top
      if (f.n == null) {
        f.pd match {
          case LeftDir => { stack.pop(); stack.top.rightDone = true }
          case RightDir => { stack.pop(); stack.top.leftDone = true }
          case _ => stack.pop()
        }
      }
      else {
        f match {
          case Frame(n, _, false, false) => stack.push(Frame(n.left, RightDir))
          case Frame(n, _, true, false) => stack.push(Frame(n.right, LeftDir))
          case Frame(n, pd, _, _) => {
            if (!n.hasLeft && !n.hasRight)
              nodeInfo(n.key).edgeLength = 0
            else {
              var limit = 0
              if (n.hasLeft) {
                (0 until (nodeInfo(n.left.key).textHeight min renderMax)).foreach(rightProfile(_) = -infty)
                computeProfiles(n.left, RightDir)
                limit = nodeInfo(n.left.key).textHeight
              }
              if (n.hasRight) {
                (0 until (nodeInfo(n.right.key).textHeight min renderMax)).foreach(leftProfile(_) = infty)
                computeProfiles(n.right, LeftDir)
                limit = limit min nodeInfo(n.right.key).textHeight
              } else
                limit = 0
              nodeInfo(n.key).edgeLength = (0 until limit).foldLeft(4) { (res,i) =>
                res max (rightProfile(i) - leftProfile(i) + 3)
              }/2
            }
            var h = 1
            nodeInfo(n.key).textHeight = 1
            if (n.hasLeft)
              h = h max (nodeInfo(n.left.key).textHeight + nodeInfo(n.key).edgeLength + 1)
            if (n.hasRight)
              h = h max (nodeInfo(n.right.key).textHeight + nodeInfo(n.key).edgeLength + 1)
            nodeInfo(n.key).textHeight = h
            pd match {
              case LeftDir => { stack.pop(); stack.top.rightDone = true }
              case RightDir => { stack.pop(); stack.top.leftDone = true }
              case _ => stack.pop()
            }
          }
        }
      }
    }
  }
  private def computeProfiles(node: Node[Int,Int], dir: Direction): Unit = {
    require(dir == LeftDir || dir == RightDir)
    case class Frame(n: Node[Int,Int], l: Int, c: Int)
    val stack = ArrayStack[Frame](Frame(node, 0, 0))
    while (stack.nonEmpty) {
      val f = stack.pop()
      if (f.n != null) {
        dir match {
          case LeftDir => {
            val adj = nodeInfo(f.n.key).parentDir match { case LeftDir => 1; case _ => 0 }
            val idx = f.l min renderMax - 1
            leftProfile(idx) = 
              leftProfile(idx) min (f.c - (nodeInfo(f.n.key).asString.length() - adj)/2)
            if (f.n.hasLeft) {
              (1 to (nodeInfo(f.n.key).edgeLength min renderMax - f.l - 1)).foreach { i =>
                leftProfile(f.l + i) = leftProfile(f.l + i) min (f.c - i)
              }
            }
          }
          case RightDir => {
            val adj = nodeInfo(f.n.key).parentDir match { case LeftDir => 0; case _ => 1 }
            val idx = f.l min renderMax - 1
            rightProfile(idx) = 
              rightProfile(idx) max (f.c + (nodeInfo(f.n.key).asString.length() - adj)/2)
            if (f.n.hasRight) {
              (1 to (nodeInfo(f.n.key).edgeLength min renderMax - f.l - 1)).foreach { i =>
                rightProfile(f.l + i) = rightProfile(f.l + i) max (f.c + i)
              }
            }
          }
        }
        stack.push(Frame(f.n.right,
                         f.l + nodeInfo(f.n.key).edgeLength + 1,
                         f.c + nodeInfo(f.n.key).edgeLength + 1))
        stack.push(Frame(f.n.left,
                         f.l + nodeInfo(f.n.key).edgeLength + 1,
                         f.c - nodeInfo(f.n.key).edgeLength - 1))
      }
    }
  }

  private val feedbackField = new Label {
    font = uiFont
    val defaultText = " "*20
    text = defaultText
    def outputSucc(message: String) = {
      foreground = colorSucc
      text = message
    }
    def outputErr(message: String) = {
      foreground = colorErr
      text = message
    }
  }
  case class SoftException(msg: String) extends Exception
  private def handleThrowable(t: Throwable): Unit = {
    feedbackField.outputErr {
      t match {
        case SoftException(msg) => msg
        case _ => { t.printStackTrace(); "Unclassified exception, see stack trace" }
      }
    }
  }
  
  private val insertButton = new Button("Insert") {
    font = uiFont
    reactions += {
      case event: ButtonClicked => {
        parseInput match {
          case Left(t) => handleThrowable(t)
          case Right((k,v)) => checkRenderingLimits(k) match {
            case Left(t) => handleThrowable(t)
            case Right(_) => attemptInsertion(k, v) match {
              case Left(t) => handleThrowable(t)
              case Right(msg) => {
                renderTree()
                feedbackField.outputSucc(msg)
              }
            }
          }
        }
        inputField.reset()
      }
    }
    def checkRenderingLimits(k: Int): Either[Throwable, Unit] = {
      try {
        tree.get(k) match {
          case None => {
            if (tree.root != null && nodeInfo(tree.root.key).textHeight >= renderMax) {
              throw SoftException("The resulting tree would violate rendering limits")
            }
            Right()
          }
          case _ => Right()
        }
      } catch {
        case e: NotImplementedError => Left(SoftException("Implementation related to TreeMap.get missing"))
        case e: Throwable => Left(e)
      }
    }
    def attemptInsertion(k: Int, v: Int): Either[Throwable, String] = {
      try {
        tree.insert(k,v) match {
          case Some(_) => {
            nodeInfo(k).asString = s"($k,$v)"
            Right("Successfully updated node " + k + "'s value with " + v)
          }
          case None => {
            nodeInfo += ((k, NodeAux(s"($k,$v)")))
            Right("Successfully added node " + k + " with value " + v)
          }
        }
      } catch {
        case e: NotImplementedError => Left(SoftException("Implementation related to TreeMap.insert missing"))
        case e: Throwable => Left(e)
      }
    }
  }
  private val removeButton = new Button("Remove") {
    font = uiFont
    reactions += {
      case event: ButtonClicked => {
        parseInput match {
          case Left(t) => handleThrowable(t)
          case Right((k,v)) => attemptRemoval(k) match {
            case Left(t) => handleThrowable(t)
            case Right(msg) => {
              renderTree()
              feedbackField.outputSucc(msg)
            }
          }
        }
        inputField.reset()
      }
    }
    def attemptRemoval(k: Int): Either[Throwable, String] = {
      try {
        tree.remove(k) match {
          case Some(_) => {
            nodeInfo -= k
            Right("Successfully removed node with key " + k)
          }
          case None => throw SoftException("No node with key " + k + " exists")
        }
      } catch {
        case e: NotImplementedError => Left(SoftException("Implementation related to TreeMap.remove missing"))
        case e: Throwable => Left(e)
      }
    }
  }
  
  contents = new GridBagPanel {
    def constraints(x: Int, y: Int,
                    gridwidth: Int = 1, gridheight: Int = 1,
                    weightx: Double = 0.0, weighty: Double = 0.0,
                    fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None)
    : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }
    add(prevLabel, constraints(1, 0, gridwidth=2, weighty=0.1, fill=GridBagPanel.Fill.Vertical))
    add(currentLabel, constraints(4, 0, gridwidth=2, weighty=0.1, fill=GridBagPanel.Fill.Vertical))
    add(new BoxPanel(Orientation.Horizontal) {
      contents += new ScrollPane(treeTextAreaPrev)
      border = Swing.EmptyBorder(0, 10, 0, 10)
    }, constraints(0, 1, gridwidth=3, weightx=1.0, weighty=1.0, fill=GridBagPanel.Fill.Both))
    add(new BoxPanel(Orientation.Horizontal) {
      contents += new ScrollPane(treeTextArea)
      border = Swing.EmptyBorder(0, 10, 0, 10)
    }, constraints(3, 1, gridwidth=3, weightx=1.0, weighty=1.0, fill=GridBagPanel.Fill.Both))
    add(Swing.VStrut(7), constraints(0, 2, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(infoField, constraints(0, 3, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(Swing.VStrut(5), constraints(0, 4, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(infoField2, constraints(0, 5, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(Swing.VStrut(5), constraints(0, 6, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HStrut(130)
        contents += insertButton
        contents += inputField
        contents += removeButton
        contents += Swing.HStrut(110)
      }
      contents += Swing.VStrut(5)
    }, constraints(0, 7, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(feedbackField, constraints(0, 8, gridwidth=6, fill=GridBagPanel.Fill.Both))
    add(Swing.VStrut(7), constraints(0, 9, gridwidth=6, fill=GridBagPanel.Fill.Both))
  }
  listenTo(inputField.keys)
  reactions += {
    case KeyPressed(_, Key.Enter, _, _) => insertButton.doClick()
    case KeyPressed(_, Key.Delete, _, _) => removeButton.doClick()
  }
}
