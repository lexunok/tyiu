package com.tyiu.corn.model.responses;

import lombok.Builder;
import lombok.Data;

import java.util.List;

import com.tyiu.corn.model.enums.Role;

@Data
@Builder
public class UserInfoResponse {
    private String id;
    private String email;
    private String firstName;
    private String lastName;
    private List<Role> roles;
}
