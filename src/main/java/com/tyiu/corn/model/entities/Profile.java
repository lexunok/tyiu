package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@Document
public class Profile {
    @Id
    private String id;
    @Indexed
    private String userEmail;
    private String avatarId;
    private List<String> userSkillsId;
    private List<String> userProjectsId;
    private List<String> userIdeasId;

    public Profile(String userEmail){
        this.userEmail = userEmail;
    }
}
