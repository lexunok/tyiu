package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.entities.mappers.RatingMapper;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "ratings")
public class RatingService {
    private final R2dbcEntityTemplate template;
    private final RatingMapper ratingMapper;
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
    public Mono<Void> confirmRating(RatingDTO ratingDTO) {
        Rating rating = mapper.map(ratingDTO, Rating.class);
        rating.setIsConfirmed(true);
        return template.update(rating).flatMap(r ->
                    template.select(query(where("idea_id").is(r.getIdeaId())), Rating.class)
                            .collectList()
                            .flatMap(list -> {
                                if (list.size() == list.stream().filter(Rating::getIsConfirmed).count()) {
                                    double number = list.stream().mapToDouble(Rating::getRating).sum();
                                    Double result = number / list.size();
                                    return template.update(query(where("id").is(ratingDTO.getIdeaId())),
                                            update("rating", result).set("status", Idea.Status.CONFIRMED), Idea.class);
                                }
                                return Mono.empty();
                            })).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> saveRating(RatingDTO ratingDTO){
        Rating rating = mapper.map(ratingDTO, Rating.class);
        rating.setIsConfirmed(false);
        return template.update(rating).then();
    }
}