package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.Role;
import jakarta.persistence.*;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.*;

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
    private Long id;
    private String email;
    private String lastName;
    private String firstName;
    @Enumerated(EnumType.STRING)
    private List<Role> roles;
    private String password;
}
