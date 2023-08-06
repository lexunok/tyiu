package com.tyiu.corn.model.requests;

import java.util.List;

import com.tyiu.corn.model.enums.Role;

import lombok.Data;

@Data
public class RegisterRequest {
    private String email;
    private String lastName;
    private String firstName;
    private String password;
    private List<Role> roles;
}
