package com.tyiu.corn.model.responses;

import java.util.List;

import com.tyiu.corn.model.enums.Role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AuthenticationResponse {
    private String token;
    private String email;
    private String lastName;
    private String firstName;
    private List<Role> roles;
}
