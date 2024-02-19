package com.tyiu.client.models;


import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
@Data
public class UserDTO {
    private String id;
    private String email;
    private String lastName;
    private String firstName;
    private String password;
    private List<Role> roles;
    private LocalDateTime createdAt;

    public UserDTO(String id, String email, String firstName, String lastName) {
        this.id = id;
        this.email = email;
        this.firstName = firstName;
        this.lastName = lastName;
    }
}
