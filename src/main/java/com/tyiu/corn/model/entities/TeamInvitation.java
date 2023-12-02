package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.RequestStatus;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;
import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class TeamInvitation {
    @Id
    private String id;
    private String teamId;
    private String userId;
    private RequestStatus status;
    //private List<Skill> skills;
    private String firstName;
    private String lastName;

}
