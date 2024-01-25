package com.tyiu.authorizationservice.model.entities;

import com.tyiu.authorizationservice.model.enums.Role;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "users")
public class User {

    @Id
    @GeneratedValue
    private String id;
    @Column(unique = true)
    private String email;
    private String lastName;
    private String firstName;
    private String password;
    private LocalDateTime createdAt;
    @Enumerated(EnumType.STRING)
    private List<Role> roles;
}
