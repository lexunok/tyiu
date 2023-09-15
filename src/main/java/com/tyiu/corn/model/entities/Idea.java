package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Date;



@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Idea {
    @Id
    private String id;
    private String initiator;
    private String name;
    private Group experts;
    private Group projectOffice;
    private StatusIdea status;
    private Date createdAt;
    private Date modifiedAt;

    private ProjectType projectType;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private Long suitability;
    private Long originality;
    private Long technicalRealizability;
    private double preAssessment;
    private double rating;
    private double risk;
}
