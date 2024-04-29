package com.tyiu.ideas.model.dto;

import lombok.*;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TeamWithMembersDTO {
    private String teamId;
    private List<String> userIds;
}
