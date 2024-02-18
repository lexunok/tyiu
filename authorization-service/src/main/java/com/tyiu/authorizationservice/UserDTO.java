package com.tyiu.authorizationservice;


import lombok.Data;

import java.util.List;
@Data
public class UserDTO {
    private String id;
    private List<User.Role> roles;
    private String username;
    private String password;
}
