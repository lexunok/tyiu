package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.dto.UserDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import com.tyiu.corn.model.enums.Role;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class GroupDTO {
    private String id;
    private String name;
    private List<UserDTO> users;
    private List<Role> roles;
}
