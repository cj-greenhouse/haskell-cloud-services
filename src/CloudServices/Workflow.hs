module CloudServices.Workflow where
--
-- https://hackage.haskell.org/package/amazonka-swf
--
-- import Data.Text (Text)
-- import Network.AWS.SWF

-- represents :
--      SWF Activity Worker - takes a single task and acts on it providing result back to workflow
class WorkflowTask m where

-- represents:
--      SWF Workflow Starter
--      SWF Workflow Worker (Decider) - replays event history and decide what to do
--          focuses on WorkflowExecutionStarted and ActivityTaskCompleted events
--          responds with decision task completed when approrpiate
class Workflow m where


--
-- module SWF where
{-
    SWF Workflow Execution
    -----------------------------------------
    * Register the domain, workflow and activity type
    * Start the activity and workflow workers
    * Start the workflow execution

    SWF Workflow Components
    -----------------------------------------
    * Domain - used as a logical container for your workflow execution data.
    * Workflow Starter - begins your workflow execution.
    * Workflow - represents code components that define logical order of execution of your workflow's activities and child workflows.
    * Workflow Worker / Decider - polls for decision tasks and schedules activities or child workflows in response.
    * Activity - represents a unit of work in the workflow.
    * Activity Worker - that polls for activity tasks and runs activity methods in response.
    * Task list - queues maintained by Amazon SWF used to issue requests to the workflow and activity workers. Tasks on a task list meant for workflow workers are called decision tasks. Those meant for activity workers are called activity tasks.

    SWF Workflow Functions
    ------------------------------------------
    * Java example  https://docs.aws.amazon.com/sdk-for-java/v1/developer-guide/swf-hello.html
    * Provisioning
        RegisterDomain          https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-RegisterDomain.html
        RegisterWorkflowType    https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-RegisterWorkflowType.html
    * SWF Workflow Starter
        StartWorkflowExecution  https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-StartWorkflowExecution.html
    * SWF Workflow Worker (Decider)
        PollForDecisionTask     https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-PollForDecisionTask.html
            RespondDecisionTaskCompleted    https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-RespondDecisionTaskCompleted.html
    * SWF Activity Worker
        PollForActivityTask     https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-PollForActivityTask.html
            RespondActivityTaskCanceled     https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-RespondActivityTaskCanceled.html
            RespondActivityTaskCompleted    https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-RespondActivityTaskCompleted.html
            RespondActivityTaskFailed       https://hackage.haskell.org/package/amazonka-swf-1.6.1/docs/Network-AWS-SWF-RespondActivityTaskFailed.html
-}

