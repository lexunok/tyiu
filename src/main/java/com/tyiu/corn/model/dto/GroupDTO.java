package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class GroupDTO {
    private Long id;
    private String name;
    //TODO: сделать вместо User - UserDTO
    private List<User> users;
}
