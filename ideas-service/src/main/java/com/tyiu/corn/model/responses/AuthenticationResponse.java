package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.enums.Role;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class AuthenticationResponse {
    private String id;
    private String token;
    private String email;
    private String lastName;
    private String firstName;
    private List<Role> roles;
    private LocalDateTime createdAt;
}
