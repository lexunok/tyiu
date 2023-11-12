package com.tyiu.corn.model.dto;

import java.util.List;

import com.tyiu.corn.model.enums.Role;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserDTO {
    private String id;
    private String email;
    private String lastName;
    private String firstName;
    private List<Role> roles;
}
