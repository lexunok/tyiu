package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class RatingService {
    private final ReactiveMongoTemplate mongoTemplate;
    private final ModelMapper mapper;

    public Flux<RatingDTO> getRatings(String id) {
        return mongoTemplate.find(Query.query(Criteria.where("ideaId").is(id)), Rating.class)
                .flatMap(r -> Flux.just(mapper.map(r, RatingDTO.class)))
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to get all ratings")));
    }

    public Mono<RatingDTO> getExpertRating(String ideaId, String expert) {
        return mongoTemplate.findOne(Query.query(Criteria.where("email").is(expert)), User.class)
                .flatMap(u ->
                        mongoTemplate.findOne(Query.query(Criteria.where("ideaId").is(ideaId)
                                        .and("expert").is(u.getId())), Rating.class)
                .flatMap(r ->
                        Mono.just(mapper.map(r, RatingDTO.class))))
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to get a rating")));
    }

    public Mono<Void> confirmRating(RatingDTO ratingDTO, String expert, String ideaId){
        return mongoTemplate.findOne(Query.query(Criteria.where("email").is(expert)), User.class)
                .flatMap(u ->
                        mongoTemplate.findOne(Query.query(Criteria.where("ideaId").is(ideaId).and("expert").is(u.getId())), Rating.class)
                                .flatMap(r -> {
                                    r.setMarketValue(ratingDTO.getMarketValue());
                                    r.setOriginality(ratingDTO.getOriginality());
                                    r.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
                                    r.setSuitability(ratingDTO.getSuitability());
                                    r.setBudget(ratingDTO.getBudget());
                                    r.setRating(ratingDTO.getRating());
                                    r.setConfirmed(true);
                                    return mongoTemplate.save(r);
                                }).then(mongoTemplate.find(Query.query(Criteria.where("ideaId").is(ideaId)), Rating.class)
                                        .collectList()
                                        .flatMap(ratings -> {
                                            long totalRatings = ratings.size();
                                            if (ratings.stream().filter(Rating::getConfirmed).count() == totalRatings) {
                                                return mongoTemplate.findById(ideaId, Idea.class)
                                                        .flatMap(idea -> {
                                                            double num = Math.round((ratings.stream()
                                                                    .mapToDouble(Rating::getRating)
                                                                    .sum() / totalRatings) * 10);
                                                            idea.setStatus(StatusIdea.CONFIRMED);
                                                            idea.setRating(num / 10);

                                                            return mongoTemplate.save(idea).then(Mono.empty());
                                                        });
                                            }
                                            return Mono.empty();
                                        })))
                .then().onErrorResume(ex -> Mono.error(new ErrorException("The rating could not be approved")));
    }

    public Mono<Void> saveRating(RatingDTO ratingDTO, String expert, String ideaId){
        return mongoTemplate.findOne(Query.query(Criteria.where("email").is(expert)), User.class)
                .flatMap(u ->
                        mongoTemplate.findOne(Query.query(Criteria.where("ideaId").is(ideaId).and("expert").is(u.getId())), Rating.class)
                                .flatMap(r -> {
                                    r.setMarketValue(ratingDTO.getMarketValue());
                                    r.setOriginality(ratingDTO.getOriginality());
                                    r.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
                                    r.setSuitability(ratingDTO.getSuitability());
                                    r.setBudget(ratingDTO.getBudget());
                                    r.setRating(ratingDTO.getRating());
                                    r.setConfirmed(false);
                                    return mongoTemplate.save(r).then(Mono.empty());
                                }))
                .then().onErrorResume(ex -> Mono.error(new ErrorException("Failed to save rating")));
    }
}
