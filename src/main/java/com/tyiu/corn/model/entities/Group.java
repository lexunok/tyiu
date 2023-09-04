package com.tyiu.corn.model.entities;

import java.util.List;

import com.tyiu.corn.model.dto.UserDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;


@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Group {
    @Id
    private String id;
    private String name;
    private List<UserDTO> users;
    
}