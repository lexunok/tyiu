package com.tyiu.corn.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.cache.annotation.CacheConfig;
import org.springframework.stereotype.Service;


@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = {"comments"})
@Slf4j
public class CommentService {
//    private final CommentRepository commentRepository;
//    private final IdeaRepository ideaRepository;
//
//    private final ModelMapper mapper;
//
//    private boolean containsEmail(List<String> list, String email){
//        return list.stream().filter(listEmail -> listEmail == email).findFirst().isPresent();
//    }
//
//    @Cacheable
//    public List<CommentDTO> getAllIdeaComments(Long ideaId){
//        if (ideaRepository.existsById(ideaId)){
//            List<Comment> ideaComments = commentRepository.findAllByIdea_Id(ideaId);
//            return mapper.map(ideaComments, new TypeToken<List<CommentDTO>>(){}.getType());
//        }
//        throw new NotFoundException(String.format("Идеи с id %d не существует", ideaId));
//    }
//
//    @CacheEvict(allEntries = true)
//    public CommentDTO createComment(Long ideaId, CommentDTO commentDTO, String email) {
//        commentDTO.setIdeaId(ideaId);
//        commentDTO.setDateCreated(new Date());
//        commentDTO.setSender(email);
//        commentDTO.setCheckedBy(List.of(email));
//        Comment comment = mapper.map(commentDTO, Comment.class);
//        comment.setIdea(ideaRepository.findById(ideaId).orElseThrow(
//            () -> new NotFoundException(String.format("Идеи с id %d не существует", ideaId)))
//        );
//        comment = commentRepository.save(comment);
//        return mapper.map(comment, CommentDTO.class);
//    }
//
//    @CacheEvict(allEntries = true)
//    @Transactional
//    public void deleteComment(Long commentId, String email) {
//        if (commentRepository.findById(commentId).get().getSender().equals(email)){
//            commentRepository.deleteById(commentId);
//        }
//        else {
//            throw new AccessException("Доступ запрещен");
//        }
//    }
//
//    @CacheEvict(allEntries = true)
//    public void checkCommentByUser(Long commentId, String email) {
//        Comment currentComment = commentRepository.findById(commentId).orElseThrow(
//            () -> new NotFoundException("Комментария не существует")
//        );
//        currentComment.getCheckedBy().add(email);
//        commentRepository.save(currentComment);
//    }
}