package com.tyiu.authorizationservice.config.security;

import com.tyiu.authorizationservice.model.dto.AuthorizedUser;
import com.tyiu.authorizationservice.model.dto.IntrospectionPrincipal;
import com.tyiu.authorizationservice.model.dto.TokenInfoDto;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.http.server.ServletServerHttpResponse;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.server.authorization.OAuth2Authorization;
import org.springframework.security.oauth2.server.authorization.OAuth2AuthorizationService;
import org.springframework.security.oauth2.server.authorization.OAuth2TokenIntrospection;
import org.springframework.security.oauth2.server.authorization.OAuth2TokenType;
import org.springframework.security.oauth2.server.authorization.authentication.OAuth2TokenIntrospectionAuthenticationToken;
import org.springframework.security.oauth2.server.authorization.config.annotation.web.configurers.OAuth2AuthorizationServerConfigurer;
import org.springframework.security.oauth2.server.authorization.settings.AuthorizationServerSettings;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.LoginUrlAuthenticationEntryPoint;
import org.springframework.security.web.util.matcher.RequestMatcher;

import java.io.IOException;

@RequiredArgsConstructor
@Configuration(proxyBeanMethods = false)
public class AuthorizationServiceConfig {

    private @Value("${spring.security.oauth2.authorizationserver.issuer-url}") String issuer;
    private @Value("${spring.security.oauth2.authorizationserver.introspection-endpoint}") String tokenIntrospectionEndpoint;

    private final static String principalAttributeKey = "java.security.Principal";

    private final MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter;
    private final OAuth2AuthorizationService oAuth2AuthorizationService;

    private void introspectionResponse(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException {
        OAuth2TokenIntrospectionAuthenticationToken introspectionAuthenticationToken = (OAuth2TokenIntrospectionAuthenticationToken) authentication;
        TokenInfoDto.TokenInfoDtoBuilder tokenInfoDtoBuilder = TokenInfoDto.builder().active(false);
        if (introspectionAuthenticationToken.getTokenClaims().isActive()) {
            OAuth2TokenIntrospection claims = introspectionAuthenticationToken.getTokenClaims();
            tokenInfoDtoBuilder.active(true)
                    .sub(claims.getSubject())
                    .aud(claims.getAudience())
                    .nbf(claims.getNotBefore())
                    .scopes(claims.getScopes())
                    .iss(claims.getIssuer())
                    .exp(claims.getExpiresAt())
                    .iat(claims.getIssuedAt())
                    .jti(claims.getId())
                    .clientId(claims.getClientId())
                    .tokenType(claims.getTokenType());


            String token = introspectionAuthenticationToken.getToken();
            OAuth2Authorization tokenAuth = oAuth2AuthorizationService.findByToken(token, OAuth2TokenType.ACCESS_TOKEN);
            if (tokenAuth != null) {
                Authentication attributeAuth = tokenAuth.getAttribute(principalAttributeKey);
                if (attributeAuth != null) {
                    if (attributeAuth.getPrincipal() instanceof AuthorizedUser authorizedUser) {
                        tokenInfoDtoBuilder.principal(IntrospectionPrincipal.build(authorizedUser));
                    } else {
                        throw new RuntimeException("Principal class = " + attributeAuth.getPrincipal().getClass().getSimpleName() + " is not supported");
                    }
                }
            }
        }

        ServletServerHttpResponse httpResponse = new ServletServerHttpResponse(response);
        mappingJackson2HttpMessageConverter.write(tokenInfoDtoBuilder.build(), null, httpResponse);
    }

    @Bean
    @Order(Ordered.HIGHEST_PRECEDENCE)
    public SecurityFilterChain authServerSecurityFilterChain(HttpSecurity http) throws Exception {
        OAuth2AuthorizationServerConfigurer authorizationServerConfigurer = new OAuth2AuthorizationServerConfigurer();
        authorizationServerConfigurer.tokenIntrospectionEndpoint((config) -> {
            config.introspectionResponseHandler(this::introspectionResponse);
        });

        RequestMatcher endpointsMatcher = authorizationServerConfigurer.getEndpointsMatcher();
        http.securityMatcher(endpointsMatcher)
                .authorizeHttpRequests(authorize -> authorize.anyRequest().authenticated())
                .csrf(csrf -> csrf.ignoringRequestMatchers(endpointsMatcher))
                .exceptionHandling(exceptions -> exceptions.authenticationEntryPoint(new LoginUrlAuthenticationEntryPoint("/login")))
                .apply(authorizationServerConfigurer);
        return http.build();
    }

    @Bean
    public AuthorizationServerSettings authorizationServerSettings() {
        return AuthorizationServerSettings.builder()
                .issuer(issuer)
                .tokenIntrospectionEndpoint(tokenIntrospectionEndpoint)
                .build();
    }
}
