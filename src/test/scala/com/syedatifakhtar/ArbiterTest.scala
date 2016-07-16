package com.syedatifakhtar

import org.scalatest.{FlatSpec, Matchers}


class ArbiterTestSpec extends FlatSpec with Matchers {
 
  val expectedOutput =
    <workflow-app xmlns="uri:oozie:workflow:0.4" name="populate-impala-/${olgaUser}/dealercode=${dealercode}">
      <global>
        <job-tracker>${{jobTracker}}</job-tracker>
        <name-node>${{nameNode}}</name-node>
        <configuration>
          <property>
            <name>oozie.launcher.mapred.job.queue.name</name>
            <value>${{launcherQueueName}}</value>
          </property>
          <property>
            <name>mapred.job.queue.name</name>
            <value>${{queueName}}</value>
          </property>
          <property>
            <name>mapred.reduce.tasks</name>
            <value>${{mediumReducers}}</value>
          </property>
          <property>
            <name>oozie.launcher.fs.permissions.umask-mode</name>
            <value>007</value>
          </property>
        </configuration>
      </global>
      <credentials>
        <credential name="hive2-cred" type="hive2">
          <property>
            <name>hive2.server.principal</name>
            <value>${{hive2Principal}}</value>
          </property>
          <property>
            <name>hive2.jdbc.url</name>
            <value>${{hiveServer2Url}}</value>
          </property>
        </credential>
      </credentials>
      <start to="set-last-cat-import-partition-month"/>
      <action name="set-last-cat-import-partition-month" retry-interval="1" retry-max="3">
        <shell xmlns="uri:oozie:shell-action:0.3">
          <exec>../shell-scripts/shell-executor.sh</exec>
          <env-var>DATABASE=${{olgaUser}}</env-var>
          <env-var>QUEUE_NAME=${{queueName}}</env-var>
          <env-var>HS2_SERVER_URL=${{hiveServer2Url}}</env-var>
          <env-var>HS2_PRINCIPAL=${{hive2Principal}}</env-var>
          <env-var>KERBEROS_ENABLED=${{kerberosEnabled}}</env-var>
          <env-var>KEYTAB_FILE=olgadatt.keytab</env-var>
          <env-var>KEYTAB_USER=${{keytabUser}}</env-var>
          <env-var>SCRIPT=script-to-run.sh</env-var>
          <env-var>query=select max(currentmonth) from ${{olgaUser}}.cat_builder_family</env-var>
          <env-var>key=lastCatImportPartitionMonth</env-var>
          <file>../shell-scripts/shell-executor.sh#shell-executor.sh</file>
          <file>../shell-scripts/get-query-result.sh#script-to-run.sh</file>
          <file>${{keytabOnClusterHDFS}}#olgadatt.keytab</file>
          <capture-output/>
        </shell>
        <ok to="fork-0"/>
        <error to="kill"/>
      </action>
      <fork name="fork-0">
        <path start="populate-opportunities"/>
        <path start="populate-sales-events"/>
        <path start="populate-others"/>
        <path start="duplicate-serial-number-report-generation"/>
      </fork>
      <action name="duplicate-serial-number-report-generation">
        <sub-workflow>
          <app-path>${{subworkflowPath}}/populate-impala/populate-duplicate-serial-numbers/populate-duplicate-serial-numbers.xml</app-path>
          <propagate-configuration/>
        </sub-workflow>
        <ok to="join-0"/>
        <error to="join-0"/>
      </action>
      <join name="join-0" to="impala-refresh-status-start"/>
      <action name="impala-refresh-status-start">
        <java>
          <main-class>com.cat.olga.utils.ImpalaRefreshStatus</main-class>
          <arg>-parentWorkflowId</arg>
          <arg>${{parentWorkflowId}}</arg>
          <arg>-timestamp</arg>
          <arg>${{timestamp}}</arg>
          <arg>-currentMonth</arg>
          <arg>${{currentMonth}}</arg>
          <arg>-status</arg>
          <arg>RUNNING</arg>
          <file>${{subworkflowPath}}/resources/webapp.properties#webapp.properties</file>
        </java>
        <ok to="move-latest-dealer-impala-data"/>
        <error to="kill"/>
      </action>
      <action name="move-latest-dealer-impala-data" retry-interval="1" retry-max="3">
        <fs>
          <delete path="${{nameNode}}${{tableBasePath}}/impala/${{dealercode}}"/>
          <move source="${{nameNode}}${{tableBasePath}}/impala/${{dealercode}}-${{timestamp}}" target="${{nameNode}}${{tableBasePath}}/impala/${{dealercode}}"/>
        </fs>
        <ok to="refresh-impala-tables"/>
        <error to="kill"/>
      </action>
      <action name="refresh-impala-tables" retry-interval="1" retry-max="3">
        <shell xmlns="uri:oozie:shell-action:0.3">
          <exec>../shell-scripts/shell-executor.sh</exec>
          <env-var>DATABASE=${{olgaUser}}</env-var>
          <env-var>QUEUE_NAME=${{queueName}}</env-var>
          <env-var>HS2_SERVER_URL=${{hiveServer2Url}}</env-var>
          <env-var>HS2_PRINCIPAL=${{hive2Principal}}</env-var>
          <env-var>KERBEROS_ENABLED=${{kerberosEnabled}}</env-var>
          <env-var>KEYTAB_FILE=olgadatt.keytab</env-var>
          <env-var>KEYTAB_USER=${{keytabUser}}</env-var>
          <env-var>SCRIPT=script-to-run.sh</env-var>
          <env-var>IMPALA_NODE=${{impalaDaemonNode}}</env-var>
          <env-var>SSL_ENABLED=${{sslEnabled}}</env-var>
          <file>../shell-scripts/shell-executor.sh#shell-executor.sh</file>
          <file>${{subworkflowPath}}/shell-scripts/refresh-impala.sh#script-to-run.sh</file>
          <file>${{keytabOnClusterHDFS}}#olgadatt.keytab</file>
          <capture-output/>
        </shell>
        <ok to="impala-refresh-status-finish"/>
        <error to="kill"/>
      </action>
      <action name="impala-refresh-status-finish">
        <java>
          <main-class>com.cat.olga.utils.ImpalaRefreshStatus</main-class>
          <arg>-parentWorkflowId</arg>
          <arg>${{parentWorkflowId}}</arg>
          <arg>-timestamp</arg>
          <arg>${{timestamp}}</arg>
          <arg>-currentMonth</arg>
          <arg>${{currentMonth}}</arg>
          <arg>-status</arg>
          <arg>SUCCEEDED</arg>
        </java>
        <ok to="end"/>
        <error to="kill"/>
      </action>
      <action name="populate-others">
        <sub-workflow>
          <app-path>${{subworkflowPath}}/populate-impala/populate-others/populate-others.xml</app-path>
          <propagate-configuration/>
        </sub-workflow>
        <ok to="join-0"/>
        <error to="kill"/>
      </action>
      <action name="populate-sales-events">
        <sub-workflow>
          <app-path>${{subworkflowPath}}/populate-impala/populate-sales-events/populate-sales-events.xml</app-path>
          <propagate-configuration/>
          <configuration>
            <property>
              <name>catConfigMonth</name>
              <value>${{wf:actionData('set-last-cat-import-partition-month')['lastCatImportPartitionMonth']}}</value>
  </property>
  </configuration>
  </sub-workflow>
    <ok to="join-0"/>
      <error to="kill"/>
  </action>
  <action name="populate-opportunities">
    <sub-workflow>
      <app-path>${{subworkflowPath}}/populate-impala/populate-opportunities/populate-opportunities.xml</app-path>
      <propagate-configuration/>
    </sub-workflow>
    <ok to="join-0"/>
    <error to="kill"/>
  </action>
    <kill name="kill">
      <message>error message[${{wf:errorMessage(wf:lastErrorNode())}}]</message>
  </kill>
    <end name="end"/>
  </workflow-app>

  val partialOutput = 
  <workflow-app name="populate-impala-/${olgaUser}/dealercode=${dealercode}" xmlns="uri:oozie:workflow:0.4">
    <global>
      <job-tracker>${{jobTracker}}</job-tracker>
      <name-node>${{nameNode}}</name-node>
      <configuration>
        <property>
          <name>oozie.launcher.mapred.job.queue.name</name>
          <value>${{launcherQueueName}}</value>
        </property>
        <property>
          <name>mapred.job.queue.name</name>
          <value>${{queueName}}</value>
        </property>
        <property>
          <name>mapred.reduce.tasks</name>
          <value>${{mediumReducers}}</value>
        </property>
        <property>
          <name>oozie.launcher.fs.permissions.umask-mode</name>
          <value>007</value>
        </property>
      </configuration>
    </global>
  </workflow-app>.map(scala.xml.Utility.trim(_)).toString

  "Arbiter" should "be able to parse valid oozie yaml" in {
    val yamlInputStream = getClass().getResourceAsStream("/test.yaml")
    val yamlContent = scala.io.Source.fromInputStream(yamlInputStream).getLines.mkString("\n")
    val workflow = Arbiter.parseYaml(yamlContent)
    val firstAction = workflow.actions.head
    firstAction.name should be ("set-last-cat-import-partition-month")
    firstAction.actionType should be ("secured-shell")

    val generatedXML = WorkflowGenerator.generateRootXML(workflow).map(scala.xml.Utility.trim(_)).toString
    generatedXML.toString.shouldEqual(partialOutput)
  }


}
