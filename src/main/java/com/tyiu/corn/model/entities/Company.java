package com.tyiu.corn.model.entities;

import java.util.List;

import com.tyiu.corn.model.dto.UserDTO;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Company {
    @Id
    private String id;
    private String name;
    private List<UserDTO> staff;
}
