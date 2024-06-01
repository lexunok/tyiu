package com.tyiu.ideas.model.dto;

import com.tyiu.client.models.UserDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CompanyDTO {
    private String id;
    private String name;
    private UserDTO owner;
    private List<UserDTO> users;
}
