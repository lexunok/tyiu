package com.tyiu.authorizationservice;

import com.tyiu.client.models.Role;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Invitation {
    private String email;
    private List<Role> roles;
}
