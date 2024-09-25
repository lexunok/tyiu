package com.tyiu.ideas.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;import com.tyiu.client.models.UserDTO;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamDTO {
    private String id;
    private String marketId;

    private String name;
    private String description;

    private Boolean closed;
    private Boolean hasActiveProject;
    private Boolean isAcceptedToIdea;
    private Boolean isRefused;
    private Integer membersCount;
    private LocalDate createdAt;

    private UserDTO owner;
    private UserDTO leader;

    private List<UserDTO> members;
    private List<SkillDTO> skills;
    private List<SkillDTO> wantedSkills;

    public TeamDTO(String id, String name, Integer membersCount) {
        this.id = id;
        this.name = name;
        this.membersCount = membersCount;
    }

    public String getId() { return id; }
    public UserDTO getLeader() { return leader; }
    public Integer getMembersCount() { return membersCount; }
    public void setMembersCount(Integer data) { membersCount = data; }
}
