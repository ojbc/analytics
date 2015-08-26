package org.ojbc.bundles.utilities.analytics.dataloading;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.annotation.Resource;
import javax.mail.Message;
import javax.mail.Part;

import org.apache.camel.Produce;
import org.apache.camel.ProducerTemplate;
import org.apache.camel.model.ModelCamelContext;
import org.apache.camel.test.spring.CamelSpringJUnit4ClassRunner;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
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
	public void contextStartup() {
		assertTrue(true);
	}

	@Before
	public void setUp() throws Exception {
		
	}	
	
	@Test
	public void testSendEmailAttachmentWithLoadingOutput() throws Exception{
		
		context.start();
		
		template.sendBody("direct:processAnalyticsLoading", "Start");
		
		Thread.sleep(5000);
		
		List<Message> inbox = org.jvnet.mock_javamail.Mailbox.get("testimap@localhost");

		assertFalse(inbox.isEmpty());

		Message message = inbox.get(0);
		assertEquals("ADS Loading Results", message.getSubject());
		
	     log.debug(message.getSize() + " bytes long.");
	     log.debug(message.getLineCount() + " lines.");
	     
	      String disposition = message.getDisposition();
	      if (disposition == null){
	        ; // do nothing
	      }else if (disposition.equals(Part.INLINE)) {
	        log.debug("This part should be displayed inline");
	      } else if (disposition.equals(Part.ATTACHMENT)) {
	        log.debug("This part is an attachment");
	        String fileName = message.getFileName();
	        log.debug("The file name of this attachment is " + fileName);
	      }
	      String description = message.getDescription();
	      if (description != null) {
	        log.debug("The description of this message is " + description);
	      }
	}


	
}
