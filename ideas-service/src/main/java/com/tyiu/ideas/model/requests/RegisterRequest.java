package com.tyiu.ideas.model.requests;

import com.tyiu.ideas.model.enums.Role;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RegisterRequest {
    private String studyGroup;
    private String telephone;
    private String email;
    private String lastName;
    private String firstName;
    private String password;
    private List<Role> roles;
    public RegisterRequest(String email,String lastName, String firstName, String password, List<Role> roles){
        this.email = email;
        this.lastName = lastName;
        this.firstName = firstName;
        this.password = password;
        this.roles = roles;
    }
}
