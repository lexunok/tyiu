package com.tyiu.ideas.model.dto;

import com.tyiu.ideas.model.enums.RequestStatus;
import lombok.*;

import java.util.List;
import java.util.Set;
import com.tyiu.client.models.UserDTO;
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IdeaInvitationDTO {
    private String id;
    private String ideaId;
    private String ideaName;
    private String initiatorId;
    private String teamName;
    private Short teamMembersCount;
    private Set<SkillDTO> skills;
    private String teamId;
    private RequestStatus status;
}
