package com.tyiu.corn.model.entities;
import lombok.*;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.util.Date;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Table
public class Comment {
    @Id
    private Long id;
    private String comment;
    private String sender;
    private List<String> checkedBy;
    private Date dateCreated;
    private Idea idea;
}