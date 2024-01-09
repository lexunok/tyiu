package com.tyiu.ideas.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.nio.file.Path;
import java.nio.file.Paths;


@Service
@RequiredArgsConstructor
public class StorageService {

    @Value("${file.path}")
    String path;

    public Mono<Resource> uploadFileIdea(String ideaId,FilePart file){
        Path basePath = Paths.get(path, ideaId + "_idea.txt");
        file.transferTo(basePath).subscribe();
        try {
            Resource resource = new UrlResource(basePath.toUri());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }
    public Mono<Resource> getFileIdea(String ideaId){
        Path basePath = Paths.get(path, ideaId + "_idea.txt");
        try {
            Resource resource = new UrlResource(basePath.toUri());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }

}
