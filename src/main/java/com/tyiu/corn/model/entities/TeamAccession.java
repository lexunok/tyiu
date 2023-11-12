package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.enums.AccessionStage;
import com.tyiu.corn.model.enums.RequestType;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class TeamAccession {
    @Id
    private String id;
    private String teamId;
    private LocalDate updateAt;
    private String text;
    private RequestType requestType; // тип заявки
    private AccessionStage accessionStage; // статус присоединения (принято, ожидает, принято или отклонено)
    private Boolean targetRegistered; // зарегана цель или нет
    private String targetEmail; // email пользователя - цель, кто присоединяется в команду или уходит из команды
    private String targetId;
    private TeamMemberDTO inviter;
}
