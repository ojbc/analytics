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
	private String workingDirectory;
	
	private static final Log log = LogFactory.getLog(RLoadingResultsFileProcessor.class);
	
	public void processResultsFileAndAddAttachment(Exchange ex) throws Exception
	{
		if (StringUtils.isBlank(loadingOutputDirectory) || StringUtils.isBlank(workingDirectory))
		{
			throw new IllegalStateException("Working directory or loading output directory path not set.  Unable to process loading results file");		
		}	
		
		File resultsFile = new File(workingDirectory + File.separator + "MainADSLoad.Rout");
		
		if (!resultsFile.exists())
		{
			throw new Exception("Loading output file does not exist!");
		}	

		File loadingOutputDirectoryPath = new File(loadingOutputDirectory);
		
		if (!loadingOutputDirectoryPath.exists())
		{	
			loadingOutputDirectoryPath.mkdirs();
		}	
		
		DateTime today = new DateTime();
		
		String dateTimeStamp = today.toString("yyyyMMdd_HHmmss");
		
		String updatedFileName = dateTimeStamp + "MainADSLoadRout.txt";
		String newFileNameWithPath = loadingOutputDirectory + File.separator + updatedFileName;
		
		File resultsFileRenamedWithDateStamp = new File(newFileNameWithPath);
		
		if(resultsFile.renameTo(resultsFileRenamedWithDateStamp)){
			log.debug("Rename succesful to: " + newFileNameWithPath);
		}else{
			log.error("Rename failed using filename: " + newFileNameWithPath);
			throw new Exception("Unable to rename file");
		}
		
		Message in = ex.getIn();
		in.addAttachment(updatedFileName, new DataHandler(new FileDataSource(resultsFileRenamedWithDateStamp)));
	}

	public String getLoadingOutputDirectory() {
		return loadingOutputDirectory;
	}

	public void setLoadingOutputDirectory(String loadingOutputDirectory) {
		this.loadingOutputDirectory = loadingOutputDirectory;
	}

	public String getWorkingDirectory() {
		return workingDirectory;
	}

	public void setWorkingDirectory(String workingDirectory) {
		this.workingDirectory = workingDirectory;
	}

	
}
