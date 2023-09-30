package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.entities.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProfileDTO {
    private String id;
    private User user;
    private List<Skill> Skills;
//    private List<Project> Projects;
}
