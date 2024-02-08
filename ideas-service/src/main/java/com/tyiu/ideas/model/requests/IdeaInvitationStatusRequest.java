package com.tyiu.ideas.model.requests;

import com.tyiu.ideas.model.enums.IdeaInvitationStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class IdeaInvitationStatusRequest {
    private String id;
    private String teamId;
    private String ideaId;
    private IdeaInvitationStatus status;
}
