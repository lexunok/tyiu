package com.tyiu.corn.model.requests;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ProfileUpdateRequest {
    private String lastName;
    private String firstName;
}
