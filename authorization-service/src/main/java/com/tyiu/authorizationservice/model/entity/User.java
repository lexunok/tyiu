package com.tyiu.authorizationservice.model.entity;

import com.tyiu.client.models.Role;
import jakarta.persistence.*;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.*;
import org.hibernate.validator.constraints.Length;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;

@Table(name = "users")
@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class User implements UserDetails {
    @GeneratedValue(strategy = GenerationType.UUID)
    @Id
    private String id;
    private String studyGroup;
    private String telephone;
    @Enumerated(EnumType.STRING)
    private List<Role> roles;
    @Column(unique = true)
    @NotBlank(message = "Почта не может быть пустым полем")
    @Email
    private String email;
    @Length(min = 8, message = "Пароль должен быть больше 8 символов")
    @NotBlank(message = "Пароль не может быть пустым полем")
    private String password;
    @NotBlank(message = "Фамилия не может быть пустым полем")
    private String lastName;
    @NotBlank(message = "Имя не может быть пустым полем")
    private String firstName;
    private LocalDateTime createdAt;
    @Builder.Default
    private Boolean isDeleted = Boolean.FALSE;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return roles.stream().map(r -> new SimpleGrantedAuthority(r.toString())).toList();
    }

    @Override
    public String getUsername() {
        return email;
    }

    @Override
    public boolean isAccountNonExpired() {
        return !isDeleted;
    }

    @Override
    public boolean isAccountNonLocked() {
        return !isDeleted;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return !isDeleted;
    }

    @Override
    public boolean isEnabled() {
        return !isDeleted;
    }
}
