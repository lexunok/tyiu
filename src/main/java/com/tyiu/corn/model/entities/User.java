package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.Role;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import java.util.List;


@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "users")
public class User {
    @Id
    private Long id;
    private String email;
    @Column("last_name")
    private String lastName;
    @Column("first_name")
    private String firstName;
    private List<Role> roles;
    private String password;
}
