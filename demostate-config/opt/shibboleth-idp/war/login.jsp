<%@ taglib uri="urn:mace:shibboleth:2.0:idp:ui" prefix="idpui" %>

<html>

    <head>
        <title>Demostate Identity Provider Login</title>
        <style>
.form-element {
  width: 100%;
  padding: 13px 12px;  
  border: none;
  font-size: 14px;
  border-radius: 4px;
  -webkit-border-radius: 4px;
  -moz-border-radius: 4px;
}
.form-field {
  color: #B7B7B7;
  border: 1px solid #B7B7B7;
}
.form-field-focus {
  color: #333333;
  border-color: #333;
}
.form-button {
  background: #deebf7;
  font-weight: bold;
  cursor: pointer;
}
        </style>
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
                <input name="j_username" type="text" value="" class="form-element form-field">
                </td></tr>
                <tr><td style="font-weight: bold">
                <label for="password">Password:</label>
                </td>
                <td>
                <input name="j_password" type="password" value="" class="form-element form-field">
                </td></tr>
                <tr><td align="center" colspan="2">
                <button type="submit" class="form-button">Login</button>
                </td></tr>
                </table>
                
            </form>
            
            <p><i>Authentication request is from: <idpui:serviceName/></i></p>
        
        </div>
    
    </body>

</html>
