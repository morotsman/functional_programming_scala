package game_of_life

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import scala.swing.Panel
import java.awt.{ Graphics2D, Color }

object SimpleGUI extends SimpleSwingApplication {

  def top = new MainFrame { // top is a required method
    title = "Game of life"

    // declare Components here




    val grid = new Grid(GridDimension(50,50)) {
      preferredSize = new Dimension(100, 100)
    }


    // choose a top-level Panel and put components in it
    // Components may include other Panels
    contents = new BorderPanel {
      layout(grid) = Center
    }
    size = new Dimension(500, 522)




  }
}

case class GridDimension(rows: Int, columns: Int)


class Grid(dimension: GridDimension) extends Panel{
  var centerColor = Color.yellow
  
  var cells = List[Pos]()
  
  

  override def paintComponent(g: Graphics2D) {
    
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
   
    def getLines(size: Int): IndexedSeq[Int] = {
      for {
        row <- 1 until (dimension.rows+1)
      } yield (row*size)  
    }

    // Draw background here
    g.drawRect(0, 0, size.width, size.height)
    getLines(size.height/dimension.rows).foreach(row => g.drawLine(0,row,size.width,row))
    getLines(size.width/dimension.columns).foreach(column => g.drawLine(column,0,column,size.height))
    

  }


}

