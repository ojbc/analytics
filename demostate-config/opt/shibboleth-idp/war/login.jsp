<%@ taglib uri="urn:mace:shibboleth:2.0:idp:ui" prefix="idpui" %>

<html>

    <head>
        <title>Demostate Identity Provider Login</title>
    </head>
    
    <body style="width: 50%; height: 100%; margin-left: auto; margin-right: auto; margin-top: 40px">
        
        <div id="main_header" style="background: #deebf7;">
	        <table><tr><td style="padding: 20px"><img src="<%= request.getContextPath()%>/images/ojbc-logo.png"></td><td><h1 style="padding-left: 40px;">Demostate Identity Provider Login</h1></td></tr></table>
	    </div>
	    
	    <div id="instructions">
	        <p><i>This is the login page for the demostate identity provider, which mimics a user's login at their "home" agency (i.e., not a centralized user directory).</i></p>
	    </div>
    
        <div id="main_form">
            <h3>Please authenticate with a Demostate user's credentials:</h3>
            <% if(request.getAttribute("actionUrl") != null){ %>
              <form id="login" action="<%=request.getAttribute("actionUrl")%>" method="post">
            <% }else{ %>
              <form id="login" action="j_security_check" method="post">
            <% } %>

              <% if ("true".equals(request.getAttribute("loginFailed"))) { %>
                <section>
                  <p>Login has failed. Double-check your username and password.</p>
                </section>
              <% } %>
              
                <table cellspacing="15px">
                <tr><td style="font-weight: bold">
                <label for="username">User ID:</label>
                </td><td>
                <input name="j_username" type="text" value="">
                </td></tr>
                <tr><td style="font-weight: bold">
                <label for="password">Password:</label>
                </td>
                <td>
                <input name="j_password" type="password" value="">
                </td></tr>
                <tr><td>
                <button type="submit">Login</button>
                </td></tr>
                </table>
                
            </form>
            
            <p><i>Authentication request is from: <idpui:serviceName/></i></p>
        
        </div>
    
    </body>

</html>
