package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.util.Date;
import java.util.List;



@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Idea {
    @Id
    private Long id;
    private String initiator;
    private String name;
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
    private StatusIdea status;
    private double rating;
    private double risk;
    private Date dateCreated;
    private Date dateModified;
    private String marketValue;
    private String originality;
    private String technicalFeasibility;
    private String understanding;
    private List<Comment> comments;
}
