package com.tyiu.corn.model.dto;

import com.mongodb.client.gridfs.model.GridFSFile;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.entities.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProfileDTO {
    private GridFSFile avatar;
    private String userEmail;
    private List<Skill> userSkills;
    private List<String> userProjects;
    private List<String> userIdeas;

    //TODO: декомпозировать профиль, удалить дто, подгружать каждый компонент отдельно
}
