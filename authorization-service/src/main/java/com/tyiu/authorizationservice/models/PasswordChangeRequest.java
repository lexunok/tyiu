package com.tyiu.authorizationservice.models;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PasswordChangeRequest {
    private String id;
    private String code;
    private String password;
}
