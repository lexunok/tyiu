package com.tyiu.ideas.model.dto;

import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import lombok.*;

import java.util.List;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GroupDTO {
    private String id;
    private String name;
    private List<UserDTO> users;
    private List<Role> roles;
}
