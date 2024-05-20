package com.tyiu.ideas.model.dto;

import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.entities.Idea;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class IdeaDTO {
    private String id;
    private UserDTO initiator;
    private String name;
    private GroupDTO experts;
    private GroupDTO projectOffice;
    private Boolean isChecked;
    private Idea.Status status;
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
    private Double preAssessment;
    private Double rating;
    private Short maxTeamSize;
    private Short minTeamSize;

    public IdeaDTO(String id, String name, String description, String customer, String initiatorId) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.customer = customer;
        this.initiator = initiator;
    }
}
