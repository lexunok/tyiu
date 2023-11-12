package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.AccessionStage;
import com.tyiu.corn.model.enums.RequestType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDate;
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamAccessionDTO {
    private String id;
    private LocalDate updateAt;
    private String text;
    private RequestType requestType; // тип заявки
    private AccessionStage accessionStage; // статус присоединения (принято, ожидает, принято или отклонено)
    private Boolean targetRegistered; // зарегана цель или нет
    private String targetEmail; // email пользователя - цель, кто присоединяется в команду или уходит из команды
    private Long targetId;
    private TeamMemberDTO inviter;
    private TeamDTO teamDTO;
}
