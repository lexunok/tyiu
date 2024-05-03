package com.tyiu.authorizationservice.model.request;

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
