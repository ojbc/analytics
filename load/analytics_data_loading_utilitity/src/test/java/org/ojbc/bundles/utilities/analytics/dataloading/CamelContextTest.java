package org.ojbc.bundles.utilities.analytics.dataloading;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.annotation.Resource;
import javax.mail.BodyPart;
import javax.mail.Message;
import javax.mail.Multipart;
import javax.mail.Part;

import org.apache.camel.Produce;
import org.apache.camel.ProducerTemplate;
import org.apache.camel.model.ModelCamelContext;
import org.apache.camel.test.spring.CamelSpringJUnit4ClassRunner;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;

@RunWith(CamelSpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {
        "classpath:META-INF/spring/camel-context.xml",
        "classpath:META-INF/spring/properties-context.xml"
        })
public class CamelContextTest {
	
	private static final Log log = LogFactory.getLog( CamelContextTest.class );
	
    @Resource
    private ModelCamelContext context;
    
    @Produce
    protected ProducerTemplate template;
    
	@Test
	public void testSendEmailAttachmentWithLoadingOutput() throws Exception{
		
		context.start();
		
		template.sendBody("direct:processAnalyticsLoading", "Start");
		
		Thread.sleep(5000);
		
		List<Message> inbox = org.jvnet.mock_javamail.Mailbox.get("testimap@localhost");

		assertFalse(inbox.isEmpty());

		Message message = inbox.get(0);
		assertEquals("ADS Loading Results", message.getSubject());
		
	     log.info(message.getSize() + " bytes long.");
	     log.info(message.getLineCount() + " lines.");
	     
	     Multipart multipart = (Multipart) message.getContent();
	     log.info(multipart.getCount());

         for (int i = 0; i < multipart.getCount(); i++) {
             BodyPart bodyPart = multipart.getBodyPart(i);
             if(!Part.ATTACHMENT.equalsIgnoreCase(bodyPart.getDisposition()) &&
                    !StringUtils.isNotBlank(bodyPart.getFileName())) {
                 continue; // dealing with attachments only
             } 
             
             if (i==1)
             {	
            	 assertTrue(StringUtils.endsWith(bodyPart.getFileName(), "BuildDashboardDataRout.txt"));
             }
             
             if (i==2)
             {	
            	 assertTrue(StringUtils.endsWith(bodyPart.getFileName(), "MainADSLoadRout.txt"));
             }	 

         }
	      
	      
	}


	
}
