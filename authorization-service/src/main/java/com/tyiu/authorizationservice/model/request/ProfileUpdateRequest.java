package com.tyiu.authorizationservice.model.request;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class ProfileUpdateRequest {
    private String firstName;
    private String lastName;
    private String studyGroup;
    private String telephone;
}
