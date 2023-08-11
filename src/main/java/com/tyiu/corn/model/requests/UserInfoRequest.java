package com.tyiu.corn.model.requests;

import java.util.List;

import com.tyiu.corn.model.enums.Role;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserInfoRequest {
    private String email;
    private String newEmail;
    private String newFirstName;
    private String newLastName;
    private List<Role> newRoles;
}
