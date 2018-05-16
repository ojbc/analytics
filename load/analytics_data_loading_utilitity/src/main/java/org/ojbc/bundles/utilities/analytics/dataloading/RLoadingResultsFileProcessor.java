package org.ojbc.bundles.utilities.analytics.dataloading;

import java.io.File;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.joda.time.DateTime;

public class RLoadingResultsFileProcessor {

	private String loadingOutputDirectory;
	private String loadingWorkingDirectory;
	private String buildDashboardWorkingDirectory;
	
	private static final Log log = LogFactory.getLog(RLoadingResultsFileProcessor.class);
	
	public void processResultsFileAndAddAttachment(Exchange ex) throws Exception
	{
		Message in = ex.getIn();
		
		if (StringUtils.isBlank(loadingOutputDirectory) || StringUtils.isBlank(loadingWorkingDirectory))
		{
			throw new IllegalStateException("Working directory or loading output directory path not set.  Unable to process loading results file");		
		}	
		
		File loadingScriptResultsFile = new File(loadingWorkingDirectory + File.separator + "LoadDimensional.Rout");
		File buildDashboardResultsFile = new File(buildDashboardWorkingDirectory + File.separator + "BuildDashboardData.Rout");
		
		if (!loadingScriptResultsFile.exists())
		{
			throw new Exception("Loading output file does not exist!");
		}	

		if (!buildDashboardResultsFile.exists())
		{
			log.info("Build Dashboard output file does not exist!");
		}	

		File loadingOutputDirectoryPath = new File(loadingOutputDirectory);
		
		if (!loadingOutputDirectoryPath.exists())
		{	
			loadingOutputDirectoryPath.mkdirs();
		}	
		
		DateTime today = new DateTime();
		
		String dateTimeStamp = today.toString("yyyyMMdd_HHmmss");
		
		//Create the Main ADS load output attachment
		String loadingScriptOuputFileName = dateTimeStamp + "LoadDimensionalRout.txt";
		String loadingScriptOutputFileWithPath = loadingOutputDirectory + File.separator + loadingScriptOuputFileName;
		
		File loadingScriptOutputFileRenamedWithDateStamp = new File(loadingScriptOutputFileWithPath);
		
		if(loadingScriptResultsFile.renameTo(loadingScriptOutputFileRenamedWithDateStamp)){
			log.debug("Rename succesful to: " + loadingScriptOutputFileWithPath);
		}else{
			log.error("Rename failed using filename: " + loadingScriptOutputFileWithPath);
			throw new Exception("Unable to rename file");
		}
		
		in.addAttachment(loadingScriptOuputFileName, new DataHandler(new FileDataSource(loadingScriptOutputFileRenamedWithDateStamp)));
		
		if (buildDashboardResultsFile.exists())
		{

			//Create the Build Dashboard output attachment
			String buildDashboardScriptOutputFileName = dateTimeStamp + "BuildDashboardDataRout.txt";
			String buildDashboardScriptOutputFileNameWithPath = loadingOutputDirectory + File.separator + buildDashboardScriptOutputFileName;
			
			File buildDashboardScriptOutputRenamedWithDateStamp = new File(buildDashboardScriptOutputFileNameWithPath);
			
			if(buildDashboardResultsFile.renameTo(buildDashboardScriptOutputRenamedWithDateStamp)){
				log.debug("Rename succesful to: " + buildDashboardScriptOutputFileNameWithPath);
			}else{
				log.error("Rename failed using filename: " + buildDashboardScriptOutputFileNameWithPath);
				throw new Exception("Unable to rename file");
			}
			
			in.addAttachment(buildDashboardScriptOutputFileName, new DataHandler(new FileDataSource(buildDashboardScriptOutputRenamedWithDateStamp)));
		}	
		
	}

	public String getLoadingOutputDirectory() {
		return loadingOutputDirectory;
	}

	public void setLoadingOutputDirectory(String loadingOutputDirectory) {
		this.loadingOutputDirectory = loadingOutputDirectory;
	}

	public String getBuildDashboardWorkingDirectory() {
		return buildDashboardWorkingDirectory;
	}

	public void setBuildDashboardWorkingDirectory(
			String buildDashboardWorkingDirectory) {
		this.buildDashboardWorkingDirectory = buildDashboardWorkingDirectory;
	}

	public String getLoadingWorkingDirectory() {
		return loadingWorkingDirectory;
	}

	public void setLoadingWorkingDirectory(String loadingWorkingDirectory) {
		this.loadingWorkingDirectory = loadingWorkingDirectory;
	}

	
}
