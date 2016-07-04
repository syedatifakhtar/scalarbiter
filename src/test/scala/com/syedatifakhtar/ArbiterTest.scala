package com.syedatifakhtar

import org.scalatest.{Matchers, FlatSpec}

class ArbiterTestSpec extends FlatSpec with Matchers {

  "Arbiter" should "be able to parse valid oozie yaml" in {
    val yamlInputStream = getClass().getResourceAsStream("/test.yaml")
    val yamlContent = scala.io.Source.fromInputStream(yamlInputStream).getLines.mkString("\n")
    val parsedYaml = Arbiter.parseYaml(yamlContent)
    println(parsedYaml)
  }


}
