package com.syedatifakhtar
import net.jcazevedo.moultingyaml.DefaultYamlProtocol.StringYamlFormat
import net.jcazevedo.moultingyaml._


package actions {
  object implicits {

    implicit class WrappedYamlValues(yamlValues: YamlObject) {
      def get[T](colName: String)(implicit toT: (YamlObject,String) => T): T
      =
      toT(yamlValues,colName)


    }
    implicit class YamlValuesMap(yamlValuesMap: Map[YamlValue,YamlValue]){
      def filterKeys[T](predicate: T=>Boolean)
                       (implicit applyFilter: (Map[YamlValue,YamlValue],T=>Boolean)
                         => Map[YamlValue,YamlValue]): Map[YamlValue,YamlValue]
      =
        applyFilter(yamlValuesMap,predicate)
    }


    implicit def filterOnYamlObjectKeys: (Map[YamlValue,YamlValue],String=>Boolean) => Map[YamlValue,YamlValue]  = {
      (yamlObj: Map[YamlValue,YamlValue],predicate: String=>Boolean) =>
        yamlObj.filterKeys(x=>predicate(x.convertTo[String]))
    }


    implicit def wrappedYamlValuesToString: (YamlObject,String) => String =
      (yamlObject: YamlObject, colName: String) =>
        yamlObject
          .fields
          .get(YamlString(colName))
              .get
              .convertTo[String]

    implicit def wrappedYamlValue: (YamlObject,String) => YamlValue =
      (yamlObject: YamlObject, colName: String) =>
        yamlObject
          .fields
          .get(YamlString(colName))
          .get
  }


}


object Arbiter extends App{

  trait ActionAttributes

  case class ActionAttributeString(val value: String) extends ActionAttributes

  case class ActionAttributeObject(val name: String, val value: ActionAttributes) extends ActionAttributes

  case class ActionAttributeList[T<: ActionAttributes](val value: List[T]) extends  ActionAttributes

  case class Workflow(name: String,xmlns: String,actions: List[Action])

  trait WorkflowMetaNode

  case class GlobalConfiguration() extends WorkflowMetaNode

  case class Action(name: String,actionType: String,attributes: List[ActionAttributeObject])

  implicit object WorkflowFormat extends YamlFormat[Workflow]{


    import actions.implicits._

    override def write(obj: Workflow): YamlValue = YamlNull

    override def read(value: YamlValue): Workflow = value match {

      case workflowAttributes: YamlObject=>
        val name = workflowAttributes.get[String]("name")
        val xmlns = workflowAttributes.get[String]("xmlns")
        val actions= workflowAttributes.get[YamlValue]("actions")
        val actionList: List[Action] = actions match {
          case YamlArray(actions)=>
            actions.map{
              node =>
                node match {
                  case action: YamlObject =>
                    parseActions(action)
                  case _ => throw new Exception("Incorrect Type Structure for Action Type")
              }
            }.toList
          case _=> throw new Exception("Unexpected node")
        }
        Workflow(name,xmlns.toString,actionList)
      case _ => throw new Exception("Unknown oozie workflow definition")
    }

    def parseActionAttributes(attribute: YamlValue): ActionAttributes = attribute match {
      case YamlArray(xs) =>
        ActionAttributeList(
          xs.map(parseActionAttributes).toList)
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
      val actionName = actionYaml.get[String]("name")
      val actionType = actionYaml.get[String]("type")
      val actionAttributesMap = actionYaml
        .fields
        .filterKeys(key=>key!="name" && key!="type")
      val actionAttributes = actionAttributesMap.map{
        case (name,value) =>
          ActionAttributeObject(name
            .convertTo[String],parseActionAttributes(value))
      }


      val action = Action(actionName,actionType,actionAttributes.toList)
     action
    }
  }

 def parseYaml(yaml: String) = {
    yaml.parseYaml.convertTo[Workflow]
 }
}
