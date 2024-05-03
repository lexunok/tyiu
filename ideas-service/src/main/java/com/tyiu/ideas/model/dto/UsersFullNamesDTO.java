package com.tyiu.ideas.model.dto;

import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UsersFullNamesDTO {
    private Integer resultId;
    private String fullNameToUser;
    private String fullNameFromUser;
}
