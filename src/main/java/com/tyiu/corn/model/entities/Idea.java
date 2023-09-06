package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.Instant;
import java.util.Date;
import java.util.List;



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
    private List<String> confirmedBy;
    private ProjectType projectType;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private Long technicalRealizability;
    private Long suitability;
    private Long budget;
    private Long preAssessment;
    private StatusIdea status;
    private double rating;
    private double risk;
    private Instant createdAt;
    private Instant modifiedAt;
    private String marketValue;
    private String originality;
    private String understanding;
    //private List<Comment> comments;
}
