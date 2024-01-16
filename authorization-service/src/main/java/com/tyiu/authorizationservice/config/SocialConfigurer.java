package com.tyiu.authorizationservice.config;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;

@Setter
@Accessors(chain = true, fluent = true)
public class SocialConfigurer extends AbstractHttpConfigurer<SocialConfigurer, HttpSecurity> {

    private AuthenticationFailureHandler failureHandler;
    private AuthenticationSuccessHandler successHandler = new SavedRequestAwareAuthenticationSuccessHandler();

    @Override
    public void init(HttpSecurity http) throws Exception {
        http.oauth2Login(oauth2Login -> {
            if (this.successHandler != null) {
                oauth2Login.successHandler(this.successHandler);
            }
            if (this.failureHandler != null) {
                oauth2Login.failureHandler(this.failureHandler);
            }
        });
    }
}
