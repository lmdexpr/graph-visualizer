package graph

import java.io.IOException
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafxml.core.{NoDependencyResolver, FXMLView}

object Main extends JFXApp {
  val fxmlFile = "View.fxml"

  val resource = getClass.getClassLoader.getResource(fxmlFile)
  if (resource == null) {
    throw new IOException("Cannot load resource: " + fxmlFile)
  }

  val root = FXMLView(resource, NoDependencyResolver)

  stage = new PrimaryStage() {
    title = "Graph Visualizer"
    scene = new Scene(root)
  }

}
