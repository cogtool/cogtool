package edu.cmu.cs.hcii.cogtool.model;

import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class SNIFACTPredictionResult extends TimePredictionResult
{
    public static final int edu_cmu_cs_hcii_cogtool_model_SNIFACTPredictionResult_version = 0;

    protected static final String taskAppVAR = "taskApp";

    private static ObjectSaver.IDataSaver<SNIFACTPredictionResult> SAVER =
        new ObjectSaver.ADataSaver<SNIFACTPredictionResult>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_SNIFACTPredictionResult_version;
            }

            @Override
            public void saveData(SNIFACTPredictionResult v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.taskApp, taskAppVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(SNIFACTPredictionResult.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<SNIFACTPredictionResult> LOADER =
        new ObjectLoader.AObjectLoader<SNIFACTPredictionResult>() {
            @Override
            public SNIFACTPredictionResult createObject()
            {
                return new SNIFACTPredictionResult();
            }

            @Override
            public void set(SNIFACTPredictionResult target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(taskAppVAR)) {
                        target.taskApp = (TaskApplication) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(SNIFACTPredictionResult.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_SNIFACTPredictionResult_version,
                                    LOADER);
    }

    protected TaskApplication taskApp;

    // For loading
    protected SNIFACTPredictionResult() { }

    public SNIFACTPredictionResult(String resName,
                                   IPredictionAlgo predictionAlg,
                                   List<String> traces,
                                   List<String> errors,
                                   List<ResultStep> steps,
                                   double time,
                                   TaskApplication ta)
    {
        super(resName, null, predictionAlg, traces, errors, steps, time);

        taskApp = ta;
    }

    @Override
    public APredictionResult duplicate(TaskApplication ta)
    {
        // TODO Auto-generated method stub
        return this;
    }

    public TaskApplication getTaskApp()
    {
        return taskApp;
    }

    @Override
    public boolean canBeRecomputed()
    {
        return false;
    }
}
