package com.tyiu.authorizationservice.model.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.oauth2.core.user.OAuth2User;

import java.util.Collection;
import java.util.Map;

@Getter
@Setter
public class AuthorizedUser extends User implements OAuth2User {

    private String id;
    private String firstName;
    private String lastName;

    private Map<String, Object> oauthAttributes;

    public AuthorizedUser(String username, String password, Collection<? extends GrantedAuthority> authorities) {
        super(username, password, authorities);
    }

    public AuthorizedUser(
            String username,
            String password,
            boolean enabled,
            boolean accountNonExpired,
            boolean credentialsNonExpired,
            boolean accountNonLocked,
            Collection<? extends GrantedAuthority> authorities
    ) {
        super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
    }

    public static AuthorizedUserBuilder builder(String username, String password, Collection<? extends GrantedAuthority> authorities) {
        return new AuthorizedUserBuilder(username, password, authorities);
    }

    public static AuthorizedUserBuilder builder(
            String username,
            String password,
            boolean enabled,
            boolean accountNonExpired,
            boolean credentialsNonExpired,
            boolean accountNonLocked,
            Collection<? extends GrantedAuthority> authorities
    ) {
        return new AuthorizedUserBuilder(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
    }

    public String getEmail() {
        return this.getUsername();
    }

    @Override
    public Map<String, Object> getAttributes() {
        return oauthAttributes;
    }

    @Override
    public String getName() {
        return this.getUsername();
    }

    public static class AuthorizedUserBuilder {

        private final AuthorizedUser entity;

        AuthorizedUserBuilder(String username, String password, Collection<? extends GrantedAuthority> authorities) {
            if (password == null) {
                password = "";
            }
            this.entity = new AuthorizedUser(username, password, authorities);
        }

        AuthorizedUserBuilder(
                String username,
                String password,
                boolean enabled,
                boolean accountNonExpired,
                boolean credentialsNonExpired,
                boolean accountNonLocked,
                Collection<? extends GrantedAuthority> authorities
        ) {
            this.entity = new AuthorizedUser(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
        }

        public AuthorizedUserBuilder id(String id) {
            this.entity.setId(id);
            return this;
        }

        public AuthorizedUserBuilder firstName(String firstName) {
            this.entity.setFirstName(firstName);
            return this;
        }

        public AuthorizedUserBuilder secondName(String secondName) {
            this.entity.setLastName(secondName);
            return this;
        }

        public AuthorizedUserBuilder oauthAttributes(Map<String, Object> userSasInfo) {
            this.entity.setOauthAttributes(userSasInfo);
            return this;
        }

        public AuthorizedUser build() {
            return this.entity;
        }
    }
}