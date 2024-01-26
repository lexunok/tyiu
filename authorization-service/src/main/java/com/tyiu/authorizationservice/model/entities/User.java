package com.tyiu.authorizationservice.model.entities;

import com.tyiu.authorizationservice.model.enums.Role;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "users")
public class User {

    @Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private String id;
    private String email;
    private String lastName;
    private String firstName;
    private String password;
    private LocalDateTime createdAt;
    private List<Role> roles;
}
