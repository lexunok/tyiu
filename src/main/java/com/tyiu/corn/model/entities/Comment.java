package com.tyiu.corn.model.entities;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Date;

@Entity
@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Comment {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    private String comment;
    private String sender;
    private List<String> checkedBy;
    private Date dateCreated;
    
    @JoinColumn(name = "IDEA_ID")
    @ManyToOne(targetEntity = Idea.class, fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private Idea idea;
}