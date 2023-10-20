package com.tyiu.corn.model.dto;

import lombok.*;
import com.tyiu.corn.model.enums.Role;
import java.util.List;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GroupDTO {
    private Long id;
    private String name;
    private List<UserDTO> users;
    private List<Role> roles;
}
