package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.enums.Role;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.List;

@Data
@AllArgsConstructor
public class RegisterRequest {
    private String email;
    private String lastName;
    private String firstName;
    private String password;
    private List<Role> roles;
}
