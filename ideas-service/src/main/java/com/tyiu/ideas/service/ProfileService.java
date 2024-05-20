package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.dto.TeamExperienceDTO;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.entities.mappers.ProfileMapper;
import com.tyiu.ideas.model.entities.relations.User2Skill;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.responses.ProfileIdeaResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;


@Service
@RequiredArgsConstructor
public class ProfileService {

    private final ProfileMapper mapper;
    private final R2dbcEntityTemplate template;

    public Mono<ProfileDTO> getUserProfile(String userId, String currentUserId) {
        String query = "SELECT u.id u_id, u.roles u_roles, u.email u_email, u.last_name u_last_name, u.first_name u_first_name, u.created_at u_created_at, u.study_group u_study_group, u.telephone u_telephone, " +
                "s.id s_id, s.name s_name, s.type s_type, i.id i_id, i.name i_name, i.solution i_solution, i.status i_status, " +
                "ut.user_tag ut_user_tag, ut.is_visible ut_is_visible, " +
                "t.name t_name, t.id t_id, t.has_active_project t_has_active_project, " +
                "tm.start_date tm_start_date, tm.finish_date tm_finish_date " +
                "FROM users u " +
                "LEFT JOIN team_member tm ON tm.member_id = u.id " +
                "LEFT JOIN team t ON t.id = tm.team_id " +
                "LEFT JOIN idea i ON i.initiator_id = u.id " +
                "LEFT JOIN user_skill us ON us.user_id = u.id " +
                "LEFT JOIN skill s ON s.id = us.skill_id " +
                "LEFT JOIN users_telegram ut ON ut.user_email = u.email " +
                "WHERE u.id = :userId";
        return template.getDatabaseClient().sql(query)
                .bind("userId", userId)
                .flatMap(p -> {
                    ConcurrentHashMap<String,SkillDTO> skills = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileIdeaResponse> ideas = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileDTO> profiles = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,TeamExperienceDTO> teams = new ConcurrentHashMap<>();
                    return p.map((row,rowMetadata) -> {
                        String ideaId = row.get("i_id",String.class);
                        String skillId = row.get("s_id",String.class);
                        String teamId = row.get("t_id",String.class);
                        if (ideaId!=null) {
                            ideas.putIfAbsent(ideaId,
                                    ProfileIdeaResponse.builder()
                                            .id(ideaId)
                                            .status(Idea.Status.valueOf(row.get("i_status",String.class)))
                                            .name(row.get("i_name",String.class))
                                            .solution(row.get("i_solution",String.class))
                                            .build());
                        }
                        if (skillId!=null) {
                            skills.putIfAbsent(skillId,
                                    SkillDTO.builder()
                                            .id(skillId)
                                            .name(row.get("s_name",String.class))
                                            .type(SkillType.valueOf(row.get("s_type",String.class)))
                                            .build());
                        }
                        if (teamId!=null) {
                            teams.putIfAbsent(teamId,
                                    TeamExperienceDTO.builder()
                                            .teamId(teamId)
                                            .teamName(row.get("t_name",String.class))
                                            .userId(row.get("u_id",String.class))
                                            .firstName(row.get("u_first_name",String.class))
                                            .lastName(row.get("u_last_name",String.class))
                                            .startDate(row.get("tm_start_date", LocalDate.class))
                                            .finishDate(row.get("tm_finish_date",LocalDate.class))
                                            .hasActiveProject(row.get("t_has_active_project",Boolean.class))
                                            .build());
                        }
                        profiles.putIfAbsent(userId,mapper.apply(row,rowMetadata));
                        ProfileDTO profileDTO = profiles.get(userId);
                        if (Boolean.TRUE.equals(profileDTO.getIsUserTagVisible()) || currentUserId.equals(userId))
                            profileDTO.setUserTag(row.get("ut_user_tag", String.class));
                        profileDTO.setIdeas(ideas.values().stream().toList());
                        profileDTO.setSkills(skills.values().stream().toList());
                        profileDTO.setTeams(teams.values().stream().toList());
                        return profileDTO;
                    });
                }).last();
    }


    public Flux<SkillDTO> saveSkills(String userId, Flux<SkillDTO> skills) {
        return template.delete(query(where("user_id").is(userId)), User2Skill.class)
                .thenMany(skills.flatMap(s ->
                        template.insert(new User2Skill(userId,s.getId()))
                                .then().thenReturn(s)
                        )
                );
    }
}
