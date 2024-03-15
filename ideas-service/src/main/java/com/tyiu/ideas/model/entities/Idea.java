package com.tyiu.ideas.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;
import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Idea {
    @Id
    private String id;
    private String initiatorId;
    private String name;
    private String groupExpertId;
    private String groupProjectOfficeId;
    private Status status;
    private LocalDateTime createdAt;
    private LocalDateTime modifiedAt;
    private Boolean isActive;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private Long suitability;
    private Long budget;
    private Short maxTeamSize;
    private Short minTeamSize;
    private Double preAssessment;
    private Double rating;

    public enum Status {
        ON_EDITING,
        ON_APPROVAL,
        ON_CONFIRMATION,
        NEW,
        CONFIRMED,
        ON_MARKET
    }
    public String getName() {
        return name;
    }
    public String getId() { return id; }
    public String getDescription() { return description; }
    public String getCustomer() { return customer; }
    public String getInitiatorId() { return initiatorId; }


}