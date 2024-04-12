package com.tyiu.ideas.model.dto;

import com.tyiu.ideas.model.enums.Role;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserDTO {
    private String id;
    private String email;
    private String studyGroup;
    private String telephone;
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

    public String getId() { return id; }
    public String getEmail() { return email; }
    public String getLastName() { return lastName; }
    public String getFirstName() { return firstName; }

    public void setId(String data) { id = data; }
}
