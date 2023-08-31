package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.Role;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import java.util.List;


@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document
public class User {
    @Id
    private String id;
    private String email;
    private String lastName;
    private String firstName;
    private List<Role> roles;
    private String password;
}
