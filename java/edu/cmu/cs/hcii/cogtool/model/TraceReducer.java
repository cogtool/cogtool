package edu.cmu.cs.hcii.cogtool.model;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

// TODO this appears never to be used; should we get rid of it?
public class TraceReducer
{
    public static List<ResultStep> simplifyTrace(List<ResultStep> steps)
    {
        List<ResultStep> simpleSteps = new ArrayList<ResultStep>();
        ResultStep newStep = null;
        ResultStep moveProductionStep = null;
        ResultStep clickProductionStep = null;
        ResultStep keyProductionStep = null;
        ResultStep homeProductionStep = null;

        Iterator<ResultStep> sIt = steps.iterator();

        while (sIt.hasNext()) {
            ResultStep step = sIt.next();

            if (step.resource.equals(ResultStep.FRAME_RESOURCE)) {
                newStep = new ResultStep(step.startTime,
                                         step.duration,
                                         step.resource,
                                         step.operation,
                                         null,
                                         step.traceStart,
                                         step.traceEnd);

                simpleSteps.add(newStep);
            }
            else if (step.resource.equals(ResultStep.PRODUCTIONS_RESOURCE))
            {
                if (step.operation.startsWith("THINK-"))
                {
                    newStep = new ResultStep(step.startTime,
                                             step.duration,
                                             "Cognition",
                                             "M",
                                             null,
                                             step.traceStart,
                                             step.traceEnd);
                    simpleSteps.add(newStep);
                }
                else if (step.operation.startsWith("PRESS-"))
                {
                    keyProductionStep = step;
                }
                else if (step.operation.startsWith("PREPARE-CLICK-"))
                {
                    clickProductionStep = step;
                }
                else if (step.operation.startsWith("MOVE-HAND-"))
                {
                    homeProductionStep = step;
                }
                else if (step.operation.startsWith("FIND-"))
                {
                    moveProductionStep = step;
                }

            }
            else if (step.resource.equals(ResultStep.MOTOR_RIGHT_EXEC_RESOURCE))
            {
                if (step.operation.equals("Executing HAND-TO-HOME"))
                {
                    newStep = new ResultStep(homeProductionStep.startTime,
                                             step.startTime + step.duration - homeProductionStep.startTime,
                                             "Hands",
                                             "H",
                                             null,
                                             homeProductionStep.traceStart,
                                             step.traceEnd);
                    simpleSteps.add(newStep);
                }
                else if (step.operation.equals("Executing CLICK-MOUSE"))
                {
                    newStep = new ResultStep(clickProductionStep.startTime,
                                             step.startTime + step.duration - clickProductionStep.startTime,
                                             "Hands",
                                             "BB",
                                             null,
                                             clickProductionStep.traceStart,
                                             step.traceEnd);
                    simpleSteps.add(newStep);
                }
                else if (step.operation.startsWith("Executing PRESS-KEY"))
                {
                    newStep = new ResultStep(keyProductionStep.startTime,
                                             step.startTime + step.duration - keyProductionStep.startTime,
                                             "Hands",
                                             "K",
                                             null,
                                             keyProductionStep.traceStart,
                                             step.traceEnd);
                    simpleSteps.add(newStep);
                }
                else if (step.operation.startsWith("Executing MOVE-CURSOR") && (moveProductionStep != null))
                {

                    newStep = new ResultStep(moveProductionStep.startTime,
                                             step.startTime + step.duration - moveProductionStep.startTime,
                                             "Hands",
                                             "P",
                                             null,
                                             moveProductionStep.traceStart,
                                             step.traceEnd);
                    simpleSteps.add(newStep);
                }
            }
        }



        return simpleSteps;
    }
}
