package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.RatingDTO;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.entities.Rating;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.entities.mappers.RatingMapper;
import com.tyiu.ideas.model.entities.relations.Group2User;
import enums.PortalLinks;
import com.tyiu.ideas.publisher.NotificationPublisher;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "ratings")
public class RatingService {
    private final R2dbcEntityTemplate template;
    private final RatingMapper ratingMapper;
    private final NotificationPublisher notificationPublisher;
    private final ModelMapper mapper;

    @Cacheable
    public Flux<RatingDTO> getRatings(String ideaId) {
        String query = """
                SELECT r.*, u.first_name expert_first_name, u.last_name expert_last_name FROM rating r
                LEFT JOIN users u ON u.id = r.expert_id WHERE idea_id = :ideaId""";
        return template.getDatabaseClient().sql(query)
                .bind("ideaId", ideaId)
                .map(ratingMapper::apply)
                .all();
    }

    @Cacheable
    public Mono<RatingDTO> getExpertRating(String ideaId, String expertId) {
        String query = """
                SELECT r.*, u.first_name expert_first_name, u.last_name expert_last_name FROM rating r
                LEFT JOIN users u ON u.id = r.expert_id WHERE idea_id = :ideaId AND expert_id = :expertId""";
        return template.getDatabaseClient().sql(query)
                .bind("ideaId", ideaId)
                .bind("expertId", expertId)
                .map(ratingMapper::apply)
                .one();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> confirmRating(RatingDTO ratingDTO, String id) {
        Rating rating = mapper.map(ratingDTO, Rating.class);
        rating.setExpertId(id);
        rating.setIsConfirmed(true);
        return template.update(rating).flatMap(r ->
                    template.select(query(where("idea_id").is(r.getIdeaId())), Rating.class)
                            .collectList()
                            .flatMap(list -> {
                                if (list.size() == list.stream().filter(Rating::getIsConfirmed).count()) {
                                    double number = list.stream().mapToDouble(Rating::getRating).sum();
                                    Double result = number / list.size();
                                    return template.update(query(where("id").is(ratingDTO.getIdeaId())),
                                            update("rating", result).set("status", Idea.Status.CONFIRMED), Idea.class)
                                            .then(Mono.fromRunnable(() ->
                                                    template.selectOne(query(where("id").is(rating.getIdeaId())), Idea.class)
                                                            .flatMap(idea ->
                                                                template.selectOne(query(where("id").is(id)), User.class)
                                                                    .flatMap(expert -> template.selectOne(query(where("id")
                                                                            .is(idea.getInitiatorId())), User.class)
                                                                    .flatMap(initiator -> notificationPublisher
                                                                            .makeNotification(NotificationRequest.builder()
                                                                                    .consumerEmail(initiator.getEmail())
                                                                                    .buttonName("Перейти к идее")
                                                                                    .title("Утвержденная идея")
                                                                                    .message(String
                                                                                            .format("Ваша идея \"%s\" была утверждена экспертами. " +
                                                                                            "Перейдите по ссылке, чтобы перейти к идее",
                                                                                                    idea.getName()))
                                                                                    .publisherEmail(expert.getEmail())
                                                                                    .link(PortalLinks.IDEAS_LIST + idea.getId())
                                                                                    .build()))
                                                                            .thenReturn(template.select(query(where("group_id")
                                                                                        .is(idea.getGroupProjectOfficeId())), Group2User.class)
                                                                                .flatMap(projectOffice ->
                                                                                        template.selectOne(query(where("id")
                                                                                                .is(projectOffice.getUserId())), User.class))
                                                                                .flatMap(projectOfficeUser ->
                                                                                        notificationPublisher
                                                                                            .makeNotification(NotificationRequest
                                                                                                    .builder()
                                                                                                    .publisherEmail(expert.getEmail())
                                                                                                    .consumerEmail(projectOfficeUser.getEmail())
                                                                                                    .link(PortalLinks.IDEAS_LIST + idea.getId())
                                                                                                    .buttonName("Перейти к идее")
                                                                                                    .title("Утвержденная идея")
                                                                                                    .message(String
                                                                                                            .format("Идея \"%s\" была утверждена экспертами. " +
                                                                                                                            "Перейдите по ссылке, чтобы перейти к идее",
                                                                                                                    idea.getName()))
                                                                                                    .build())
                                                                                        ))
                                                                    )
                                                            ).subscribe()
                                            ));
                                }
                                template.selectOne(query(where("id").is(id)), User.class).flatMap(
                                        expert -> template.selectOne(query(where("id").is(rating.getIdeaId())), Idea.class)
                                                .flatMap(idea -> template.selectOne(query(where("id").is(idea.getInitiatorId())), User.class)
                                                        .flatMap(initiator -> notificationPublisher.makeNotification(
                                                                NotificationRequest.builder()
                                                                        .consumerEmail(initiator.getEmail())
                                                                        .buttonName("Перейти к идее")
                                                                        .title("Ваша идея была оценена")
                                                                        .message(String
                                                                                .format("Эксперт %s %s оценил важу идею \"%s\" на оценку - %.2f" +
                                                                                        "Перейдите по ссылке, чтобы посмотреть подробности",
                                                                                        expert.getFirstName(),
                                                                                        expert.getLastName(),
                                                                                        idea.getName(),
                                                                                        rating.getRating())
                                                                        )
                                                                        .publisherEmail(expert.getEmail())
                                                                        .link(PortalLinks.IDEAS_LIST + idea.getId())
                                                                        .build())
                                                        ))).subscribe();
                                return Mono.empty();
                            })).then();
    }


    @CacheEvict(allEntries = true)
    public Mono<Void> saveRating(RatingDTO ratingDTO, String id){
        Rating rating = mapper.map(ratingDTO, Rating.class);
        rating.setExpertId(id);
        rating.setIsConfirmed(false);
        return template.update(rating).then();
    }
}