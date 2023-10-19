package com.tyiu.corn.model.dto;

import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamMemberDTO {
    private Long userId;
    private String email;
    private String firstName;
    private String lastName;
}
