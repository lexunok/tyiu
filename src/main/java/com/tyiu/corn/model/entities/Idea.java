package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import jakarta.persistence.*;
import lombok.*;

import java.util.Date;
import java.util.List;


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

    @Enumerated(EnumType.STRING)
    private ProjectType projectType;
    private String experts;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private String realizability;
    private Long suitability;
    private Long budget;
    private Long preAssessment;

    @Enumerated(EnumType.STRING)
    private StatusIdea status;
    private double rating;
    private double risk;
    private Date dateCreated;
    private Date dateModified;
    private String marketValue;
    private String originality;
    private String technicalFeasibility;
    private String understanding;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private List<Comment> comments;
}
