package com.tyiu.ideas.model.requests;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProfileUpdateRequest {
    private String studyGroup;
    private String telephone;
    private String lastName;
    private String firstName;
}
