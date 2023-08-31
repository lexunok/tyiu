package com.tyiu.corn.model.entities;
import lombok.*;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Date;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document
public class Comment {
    @Id
    private String id;
    private String comment;
    private String sender;
    private List<String> checkedBy;
    private Date dateCreated;
    private Idea idea;
}