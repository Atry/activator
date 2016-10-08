/**
 * Copyright (C) 2016 Lightbend <http://www.lightbend.com/>
 */
package activator

import jline.TerminalFactory
import xsbti.AppConfiguration
import activator.properties.ActivatorProperties.SCRIPT_NAME
import activator.cache.Actions.cloneTemplate
import java.io.File
import sbt.complete.{ Parser, Parsers }
import scala.concurrent.Await
import scala.concurrent.duration._
import activator.cache.{ TemplateMetadata, TemplateCache }
import scala.util.control.NonFatal

object ActivatorCli extends ActivatorCliHelper {
  case class ProjectInfo(projectName: String = "N/A", templateName: String = "N/A", file: Option[File] = None)

  def withJLineShutdownHooks[A](f: => A): A = {
    // We've disabled jline shutdown hooks to prevent classloader leaks, and have been careful to always restore
    // the jline terminal in finally blocks, but hitting ctrl+c prevents finally blocks from being executed, in that
    // case the only way to restore the terminal is in a shutdown hook.
    val shutdownHook = new Thread(new Runnable {
      def run() {
        TerminalFactory.get().restore()
      }
    })
    try {
      Runtime.getRuntime.addShutdownHook(shutdownHook)
      f
    } finally {
      Runtime.getRuntime.removeShutdownHook(shutdownHook)
    }
  }

  def apply(configuration: AppConfiguration): Int = withJLineShutdownHooks {
    withContextClassloader {
      // TODO - move this into a common shared location between CLI and GUI.
      val cache = UICacheHelper.makeDefaultCache(ActivatorCliHelper.system)
      val metadata = TemplateHandler.downloadTemplates(cache, ActivatorCliHelper.defaultDuration)

      val possible = metadata.map(_.name).toSeq.distinct
      val featured = metadata.filter(_.featured)
      val suggestedSeeds = featured.filter(_.tags.contains("seed")).map(_.name).toSeq.distinct
      val suggested = if (suggestedSeeds.nonEmpty) suggestedSeeds else featured.map(_.name).toSeq.distinct

      def validateTemplateName(tNameOption: Option[String]): Option[String] = {
        val validated = {
          if (tNameOption.isEmpty) {
            System.err.println("Please enter a template name.")
            None
          } else {
            tNameOption flatMap { tName =>
              if (metadata.exists(_.name == tName)) {
                Some(tName)
              } else {
                System.err.println(s"Template name '${tName}' wasn't found in the template catalog.")
                None
              }
            }
          }
        }

        if (validated.isEmpty) {
          System.err.println(s"Try these template names: ${suggested.mkString(", ")}")
          System.err.println(s"or see all templates at http://lightbend.com/activator/templates or with 'activator list-templates'")
        }

        validated
      }

      def getTemplateName(): Option[String] = {
        val tNameOption = try TemplateHandler.getTemplateName(possible, suggested)
        catch {
          case NonFatal(e) =>
            None
        }

        validateTemplateName(tNameOption)
      }

      // Handling input based on length (yes, it is brittle to do argument parsing like this...):
      // length = 2 : "new" and "project name" => generate project automatically, but query for template to use
      // length = 3 : "new", "project name" and "template name" => generate project and template automatically
      // length != (2 || 3) : query for both project name and template
      val projectInfo =
        configuration.arguments().length match {
          case 2 =>
            val pName = configuration.arguments()(1)
            createFile(pName) match {
              case f @ Some(_) =>
                (for (tName <- getTemplateName) yield ProjectInfo(
                  projectName = pName,
                  templateName = tName,
                  file = f)) getOrElse ProjectInfo()
              case None => ProjectInfo()
            }
          case 3 =>
            val pName = configuration.arguments()(1)
            validateTemplateName(Some(configuration.arguments()(2))) map { tName =>
              ProjectInfo(
                projectName = pName,
                templateName = tName,
                file = createFile(pName))
            } getOrElse ProjectInfo()
          case _ =>
            (for (tName <- getTemplateName()) yield {
              val pName = getApplicationName(tName)
              createFile(pName) match {
                case f @ Some(_) =>
                  ProjectInfo(
                    projectName = pName,
                    templateName = tName,
                    file = f)
                case None => ProjectInfo()
              }
            }) getOrElse ProjectInfo()
        }

      val result = for {
        f <- projectInfo.file
        t <- TemplateHandler.findTemplate(metadata, projectInfo.templateName)
      } yield generateProjectTemplate(t, projectInfo.templateName, projectInfo.projectName, cache, f)

      result.getOrElse(1)
    }
  }

  private def createFile(name: String): Option[File] = {
    val file = new File(name)
    if (!file.exists()) Some(file.getAbsoluteFile)
    else {
      System.err.println(s"There already is a project with name: $name. Either remove the existing project or create one with a unique name. ")
      None
    }
  }

  private def generateProjectTemplate(template: TemplateMetadata, tName: String, pName: String, cache: TemplateCache, projectDir: File): Int = {
    System.out.println(s"""OK, application "$pName" is being created using the "${template.name}" template.""")
    System.out.println()
    import scala.concurrent.ExecutionContext.Implicits.global

    // record stats in parallel while we are cloning
    val statsRecorded = TemplatePopularityContest.recordClonedIgnoringErrors(template.name)

    // TODO - Is this duration ok?
    val result = Await.result(
      cloneTemplate(
        cache,
        template.id,
        projectDir,
        Some(pName),
        filterMetadata = !template.templateTemplate,
        additionalFiles = UICacheHelper.scriptFilesForCloning),
      Duration(5, MINUTES))

    result match {
      case _: ProcessSuccess[_] => printUsage(pName, projectDir)
      case f: ProcessFailure =>
        f.failures.foreach { System.err.println(_) }
        System.err.println("Failed to clone the template. Make sure you have internet access and please try again.")
    }

    // don't wait too long on this remote call, we ignore the
    // result anyway; just don't want to exit the JVM too soon.
    try Await.result(statsRecorded, Duration(5, SECONDS))
    catch {
      case _: Exception =>
    }
    0
  }

  private def printUsage(name: String, dir: File): Unit = {
    // TODO - Cross-platform-ize these strings! Possibly keep script name in SnapProperties.
    System.out.println(s"""|To run "$name" from the command line, "cd $name" then:
                           |${dir.getAbsolutePath}/bin/${SCRIPT_NAME} run
                           |
                           |To run the test for "$name" from the command line, "cd $name" then:
                           |${dir.getAbsolutePath}/bin/${SCRIPT_NAME} test
                           |
                           |To run the Activator UI for "$name" from the command line, "cd $name" then:
                           |${dir.getAbsolutePath}/bin/${SCRIPT_NAME} ui
                           |""".stripMargin)
  }

  private def getApplicationName(default: String): String = {
    System.out.println(s"Enter a name for your application (just press enter for '${default}')")
    val appNameParser: Parser[String] = {
      import Parser._
      import Parsers._
      token(any.* map { _ mkString "" }, "<application name>")
    }

    readLine(appNameParser) map (_.trim) filterNot (_.isEmpty) getOrElse default
  }

  def withContextClassloader[A](f: => A): A = {
    val current = Thread.currentThread
    val old = current.getContextClassLoader
    current setContextClassLoader getClass.getClassLoader
    try f
    finally current setContextClassLoader old
  }
}
