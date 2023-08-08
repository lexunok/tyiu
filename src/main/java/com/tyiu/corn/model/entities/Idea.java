package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import jakarta.persistence.*;
import lombok.*;

import java.util.Date;



@Entity
@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Idea {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String initiator;
    private String name;
    private ProjectType projectType;
    private String experts;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String description;
    private String realizability;
    private String suitability;
    private Long budget;
    private StatusIdea status;
    private double rating;
    private double risk;
    private Date dateCreated;
    private Date dateModified;

    //@ManyToMany
    //private List<Profile> profiles;

    //@OneToMany
    //private List<Comment> comments;



}
