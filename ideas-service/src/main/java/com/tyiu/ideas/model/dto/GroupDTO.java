package com.tyiu.ideas.model.dto;

import lombok.*;
import com.tyiu.ideas.model.enums.Role;
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
