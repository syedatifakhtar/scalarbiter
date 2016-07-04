package com.syedatifakhtar
import net.jcazevedo.moultingyaml._

object Arbiter extends App{

  trait ActionAttributes
  case class ActionAttributeString(val value: String) extends ActionAttributes

  case class ActionAttributeObject(val name: String, val value: ActionAttributes) extends ActionAttributes

  case class ActionAttributeList[T<: ActionAttributes](val value: List[T]) extends  ActionAttributes

  case class Workflow(name: String,xmlns: String,actions: List[Action])

  case class Action(name: String,actionType: String,attributes: List[ActionAttributeObject])

  implicit object WorkflowFormat extends YamlFormat[Workflow]{

    import net.jcazevedo.moultingyaml.DefaultYamlProtocol.StringYamlFormat
    override def write(obj: Workflow): YamlValue = ???

    override def read(value: YamlValue): Workflow = value match {

      case YamlObject(workflowAttributes)=>
        val name = workflowAttributes.get(YamlString("name")).get.convertTo[String]
        val xmlns = workflowAttributes.get(YamlString("xmlns")).get.convertTo[String]
        val actions= workflowAttributes.get(YamlString("actions")).get
        val actionList: List[Action] = actions match {
          case YamlArray(actions)=>
            actions.map{
              node =>
                node match {
                  case action: YamlObject =>
                    parseActions(action)
                  case _ => throw new Exception("Node was not an action")
              }
            }.toList
          case _=> throw new Exception("Unexpected node")
        }
        Workflow(name,xmlns.toString,actionList)
      case _ => throw new Exception("Unknown oozie workflow definition")
    }

    def parseActionAttributes(attribute: YamlValue): ActionAttributes = attribute match {
      case YamlArray(xs) =>
        ActionAttributeList(xs.map(x=>parseActionAttributes(x)).toList)
      case YamlString(value) => ActionAttributeString(value)
      case YamlObject(fields)=>
        ActionAttributeList(fields.map {
          case (name,value)=>
            ActionAttributeObject(
              name.convertTo[String],parseActionAttributes(value))
        }.toList)
      case e@_ => throw new Exception("Unidentified action attribute type: " + e)
    }


    def parseActions(actionYaml: YamlObject): Action = {

      val actionName = actionYaml.fields.get(YamlString("name")).get.convertTo[String]
      val actionType = actionYaml.fields.get(YamlString("type")).get.convertTo[String]
      val actionAttributesMap = actionYaml.fields.filterKeys(key=> key!=YamlString("name") && key!=YamlString("type"))
      val actionAttributes = actionAttributesMap.map{
        case (name,value) =>
          ActionAttributeObject(name.convertTo[String],parseActionAttributes(value))
      }


      val action = Action(actionName,actionType,actionAttributes.toList)
     action
    }
  }

 def parseYaml(yaml: String) = {
   import net.jcazevedo.moultingyaml._

  yaml.parseYaml.convertTo[Workflow]
 }
}
