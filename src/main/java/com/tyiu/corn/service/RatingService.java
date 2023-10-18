package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
public class RatingService {
    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    public Flux<RatingDTO> getRatings(Long ideaId) {
        return template.select(query(where("idea_id").is(ideaId)), Rating.class)
                .flatMap(r -> Flux.just(mapper.map(r, RatingDTO.class)))
                .switchIfEmpty(Mono.error(new ErrorException("Failed to get all ratings")));
    }

    public Mono<RatingDTO> getExpertRating(Long ideaId, Long expertId) {
        return template.selectOne(query(where("expert_id").is(expertId)
                .and(where("idea_id").is(ideaId))), Rating.class)
                .flatMap(r -> Mono.just(mapper.map(r, RatingDTO.class)))
                .switchIfEmpty(Mono.error(new ErrorException("Failed to get a rating")));
    }

    public Mono<Void> confirmRating(RatingDTO ratingDTO, Long expertId) {
        template.update(query(where("expert_id").is(expertId)
                .and(where("idea_id").is(ratingDTO.getIdeaId()))),
                update("market_value",ratingDTO.getMarketValue())
                        .set("originality",ratingDTO.getOriginality())
                        .set("technicalRealizability", ratingDTO.getTechnicalRealizability())
                        .set("suitability", ratingDTO.getSuitability())
                        .set("budget", ratingDTO.getBudget())
                        .set("rating", ratingDTO.getRating())
                        .set("confirmed", true), Rating.class).subscribe();
        return template.select(query(where("idea_id").is(ratingDTO.getIdeaId())), Rating.class)
                .collectList()
                .flatMap(list ->{
                    if (list.size() == list.stream().filter(Rating::getConfirmed).count()) {
                        double number = list.stream().mapToDouble(Rating::getRating).sum();
                        Double rating = number / list.size();
                        template.update(query(where("id").is(ratingDTO.getIdeaId())),
                                update("rating",rating)
                                        .set("status", StatusIdea.CONFIRMED), Idea.class);
                    }
                    return Mono.empty();
                }).then().onErrorResume(e -> Mono.error(new ErrorException("Not confirmed")));
    }
    public Mono<Void> saveRating(RatingDTO ratingDTO, Long expertId){
        return template.update(query(where("expert_id")
                .is(expertId).and("idea_id")
                .is(ratingDTO.getIdeaId())),
                        update("market_value",ratingDTO.getMarketValue())
                                .set("originality",ratingDTO.getOriginality())
                                .set("technical_realizability", ratingDTO.getTechnicalRealizability())
                                .set("suitability", ratingDTO.getSuitability())
                                .set("budget", ratingDTO.getBudget())
                                .set("rating", ratingDTO.getRating())
                                .set("confirmed",false), Rating.class)
                .then().onErrorResume(e -> Mono.error(new ErrorException("Failed to save rating")));
    }
}