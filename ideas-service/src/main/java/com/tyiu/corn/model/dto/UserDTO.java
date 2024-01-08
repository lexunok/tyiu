package com.tyiu.corn.model.dto;

import java.time.LocalDateTime;
import java.util.List;

import com.tyiu.corn.model.enums.Role;
import lombok.*;

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
    private LocalDateTime createdAt;

    public UserDTO(String id, String email, String firstName, String lastName) {
        this.id = id;
        this.email = email;
        this.firstName = firstName;
        this.lastName = lastName;
    }
}
