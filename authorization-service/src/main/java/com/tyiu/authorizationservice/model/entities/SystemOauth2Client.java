package com.tyiu.authorizationservice.model.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.ClientAuthenticationMethod;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

@Getter
@Setter
@Entity
@Accessors(chain = true)
@Table(name = "system_oauth2_clients")
public class SystemOauth2Client {

    @Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private String id;
    private String clientId;
    private LocalDateTime clientIdIssueAt;
    private String clientSecret;
    private LocalDateTime clientSecretExpiresAt;
    private String clientName;
    private String clientAuthenticationMethods;
    private String authorizationGrantTypes;
    private String redirectUris;
    private String scopes;
    private String clientSettings;
    private String tokenSettings;

    public SystemOauth2Client() {
    }

    @Transient
    public Set<ClientAuthenticationMethod> clientAuthenticationMethods() {
        if (this.clientAuthenticationMethods == null) {
            return Collections.emptySet();
        }
        return Arrays.stream(this.clientAuthenticationMethods.split(","))
                .map(item -> new ClientAuthenticationMethod(item.trim()))
                .collect(Collectors.toSet());
    }

    @Transient
    public Set<AuthorizationGrantType> authorizationGrantTypes() {
        if (this.authorizationGrantTypes == null) {
            return Collections.emptySet();
        }
        return Arrays.stream(this.authorizationGrantTypes.split(","))
                .map(item -> new AuthorizationGrantType(item.trim()))
                .collect(Collectors.toSet());
    }

    @Transient
    public Set<String> redirectUris() {
        if (this.redirectUris == null) {
            return Collections.emptySet();
        }
        return Arrays.stream(this.redirectUris.split(","))
                .collect(Collectors.toSet());
    }

    @Transient
    public Set<String> scopes() {
        if (this.scopes == null) {
            return Collections.emptySet();
        }
        return Arrays.stream(this.scopes.split(","))
                .collect(Collectors.toSet());
    }
}