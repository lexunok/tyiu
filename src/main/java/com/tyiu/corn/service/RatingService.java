package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.IdeaRepository;
import com.tyiu.corn.repository.RatingRepository;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class RatingService {
    private final RatingRepository ratingRepository;
    private final IdeaRepository ideaRepository;
    private final ModelMapper mapper;

    public Flux<RatingDTO> getRatings(String id) {
        return ratingRepository.findAllByIdeaId(id).flatMap(rating -> Flux.just(mapper.map(rating, RatingDTO.class)));
    }

    public Mono<RatingDTO> getExpertRating(String id, String expert) {
        return ratingRepository.findFirstByExpertAndIdeaId(expert, id)
                .flatMap(r -> Mono.just(mapper.map(r, RatingDTO.class)));
    }

    public Mono<Void> confirmRating(RatingDTO ratingDTO, String expert, String ideaId){
        return ratingRepository.findFirstByExpertAndIdeaId(expert, ideaId).flatMap(r -> {
            r.setMarketValue(ratingDTO.getMarketValue());
            r.setOriginality(ratingDTO.getOriginality());
            r.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
            r.setSuitability(ratingDTO.getSuitability());
            r.setBudget(ratingDTO.getBudget());
            r.setRating(ratingDTO.getRating());
            r.setConfirmed(true);
            return ratingRepository.save(r);
        }).then(ratingRepository.findAllByIdeaId(ideaId)
                        .collectList()
                        .flatMap(ratings -> {
                            long totalRatings = ratings.size();
                            long confirmedRatings = ratings.stream().filter(Rating::getConfirmed).count();

                            if (confirmedRatings == totalRatings) {
                                return ideaRepository.findById(ideaId)
                                        .flatMap(idea -> {
                                            idea.setStatus(StatusIdea.CONFIRMED);

                                            double sum = ratings.stream()
                                                    .mapToDouble(Rating::getRating)
                                                    .sum();
                                            double averageRating = sum / totalRatings;

                                            idea.setRating(averageRating);

                                            ideaRepository.save(idea).subscribe();
                                            return Mono.empty();
                                        });
                            }
                            return Mono.empty();
                        })
        );
    }

    public Mono<Void> saveRating(RatingDTO ratingDTO, String expert, String ideaId){
        return ratingRepository.findFirstByExpertAndIdeaId(expert, ideaId).flatMap(r -> {
            r.setMarketValue(ratingDTO.getMarketValue());
            r.setOriginality(ratingDTO.getOriginality());
            r.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
            r.setSuitability(ratingDTO.getSuitability());
            r.setBudget(ratingDTO.getBudget());
            r.setRating(ratingDTO.getRating());
            r.setConfirmed(false);
            ratingRepository.save(r).subscribe();
            return Mono.empty();
        });
    }
}
