package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class IdeaDTO {

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
    private String suitability;
    private Long budget;
    private StatusIdea status;
    private double rating;
    private double risk;
    private Date dateCreated;
    private Date dateModified;
    private String price;
    private String originality;
    private String technicalFeasibility;
    private String understanding;

}
