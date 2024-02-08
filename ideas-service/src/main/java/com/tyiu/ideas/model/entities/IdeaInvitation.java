package com.tyiu.ideas.model.entities;

import com.tyiu.ideas.model.enums.IdeaInvitationStatus;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class IdeaInvitation {
    @Id
    private String id;
    private String ideaId;
    private String teamId;
    private IdeaInvitationStatus status;
}
