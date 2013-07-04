import sbt._
import Keys._

object UpdatableBuild extends Build{
  
  var root = Project(id = "UPDATABLE",
                          base = file("."))
  
  val projectVersion = SettingKey[(String,Publication)]("projectVersion", "Project version for this building.")

  def revision : String = {
      import java.util.Calendar
      val c = new java.util.GregorianCalendar()
      "-" + 
        c.get(Calendar.YEAR).toString + {
        (c.get(Calendar.MONTH)+1).toString match{
          case m if m.length < 2 => "0" + m
          case m => m
        }} + {
        c.get(Calendar.DAY_OF_MONTH).toString match{
          case dom if dom.length < 2 => "0" + dom
          case dom => dom
        }} + {
        c.get(Calendar.HOUR_OF_DAY).toString match{
          case hod if hod.length < 2 => "0" + hod
          case hod => hod
        }} + {
        c.get(Calendar.MINUTE).toString match{
          case m if m.length < 2 => "0" + m
          case m => m
        }} + {
        c.get(Calendar.SECOND).toString match{
          case s if s.length < 2 => "0" + s
          case s => s
        }}
    }

  trait Publication
  case object RELEASE extends Publication
  case class BRANCH(val name : String) extends Publication
  object BRANCH { def of(name: String): BRANCH = BRANCH(name) }
  case object SNAPSHOT extends Publication

  def publishExtra {
    Thread.sleep(3000)
    println("Publishing repo status ...")
    "ssh repo-publisher@andromeda\r/var/www/private-repo/publish-status" !;
  }
  
}

