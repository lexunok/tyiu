package com.tyiu.corn.model.responses;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AuthenticationResponse {
    private String jwt;
    private String email;
    private String lastName;
    private String firstName;
}
