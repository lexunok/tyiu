package com.tyiu.corn.model.entities;

import com.fasterxml.jackson.annotation.JsonIgnore;
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
    @Column(name = "IDEA_ID")
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
    private String suitability;
    private Long budget;

    @Enumerated(EnumType.STRING)
    private StatusIdea status;
    private double rating;
    private double risk;
    private Date dateCreated;
    private Date dateModified;
    private String price;
    private String originality;
    private String technicalFeasibility;
    private String understanding;

    @OneToMany(mappedBy = "idea", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private List<Comment> comments;
}
