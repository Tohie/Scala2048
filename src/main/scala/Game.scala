package twothousand

import javax.swing._
import java.awt._
import scala.List
import java.awt.event.{KeyEvent, KeyListener}

object Colors {
  // blatantly stolen from the original 2048 game
  val points = Map(
    0 -> new Color(0x3c3a32),
    2 -> new Color(0xfffdf8),
    4 -> new Color(0xeee4da),
    8 -> new Color(0xf2b179),
    16 -> new Color(0xf5956b),
    32 -> new Color(0xf67c5f),
    64 -> new Color(0xf65e3b),
    128 -> new Color(0xedcf72),
    256 -> new Color(0xedcc61),
    512 -> new Color(0xedc850),
    1024 -> new Color(0xedc53f),
    2048 -> new Color(0xedc22e)
  )

  val red = new Color(255, 0, 0)
}

class Game extends JPanel with KeyListener {
  val grid = List.fill(4, 4)(None)
  var board = new Board(grid)
  for (i <- 0 to 1) board = board.placeRandomPiece

  setFocusable(true)
  addKeyListener(this)
  val ked = new DefaultFocusManager()
  KeyboardFocusManager.setCurrentKeyboardFocusManager(ked)

  override def getPreferredSize() = new Dimension(500, 500)

  override def paintComponent(g: Graphics)  = {
    val g2 = g.asInstanceOf[Graphics2D]
    val size = getSize()

    val rows = board.rows
    val cols = board.cols

    val rowSize = (size.getWidth / rows).toInt
    val colSize = (size.getHeight / cols).toInt

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        val value = board.get(j, i).getOrElse(0)
        val color = Colors.points.getOrElse(value, Colors.red)
        val (x, y) = (i * rowSize, j * colSize)

        g2.setColor(color)
        g2.fillRect(x, y, rowSize, colSize)

        drawBorder(g2, x, y, rowSize, colSize)

        g2.setColor(new Color(0, 0, 0))
        val text: String = board.get(j, i).map(_.toString).getOrElse(" ")
        g2.drawString(text, x + (rowSize / 2), y + (colSize / 2))
      }
    }
  }

  def drawBorder(g2: Graphics2D, x: Int, y: Int, width: Int, height: Int) = {
    val oldStroke = g2.getStroke
    g2.setColor(new Color(0, 0, 0))
    val rect = new Rectangle(x, y, width, height)
    g2.setStroke(new BasicStroke(2))
    g2.draw(rect)
    g2.setStroke(oldStroke)
  }

  def keyPressed(e: KeyEvent): Unit = {
    if (!board.canMove) {
      JOptionPane.showMessageDialog(this, "You lose!")
      board = new Board(grid)
      return
    }
    var uselessKey = false
    e.getKeyCode match {
      case KeyEvent.VK_DOWN => board = board.move("down")
      case KeyEvent.VK_UP => board = board.move("up")
      case KeyEvent.VK_LEFT => board = board.move("left")
      case KeyEvent.VK_RIGHT => board = board.move("right")
      case _ => uselessKey = true
    }
    if (!uselessKey) {
      board = board.placeRandomPiece
      this.repaint()
    }
  }

  def keyReleased(e: KeyEvent): Unit = {}
  def keyTyped(e: KeyEvent): Unit = {}
}

object GameUI extends JFrame("2048") {
  def main(args: Array[String]) = {
    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName())
    val game = new Game()

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setLocationRelativeTo(null)

    getContentPane add game
    pack
    setVisible(true)
  }
}
