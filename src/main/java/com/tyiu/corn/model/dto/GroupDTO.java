package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.User;
import lombok.Data;

import java.util.List;

@Data
public class GroupDTO {
    private Long id;
    private String name;
    //TODO: сделать вместо User - UserDTO
    private List<User> users;
}
