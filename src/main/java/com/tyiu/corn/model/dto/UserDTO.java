package com.tyiu.corn.model.dto;

import java.util.List;

import com.tyiu.corn.model.enums.Role;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserDTO {
    private String email;
    private String lastName;
    private String firstName;
    private List<Role> roles;
}
