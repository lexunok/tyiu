package com.tyiu.corn.controller;

import com.tyiu.corn.service.StorageService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;


@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/storage")
public class FilesStorageController {
    private final StorageService storage;

    @GetMapping("/idea/get/{ideaId}")
    public Mono<Resource> getFileFromIdea(@PathVariable Long ideaId) {
        return storage.getFileIdea(ideaId);
    }
    @PostMapping("/idea/upload/{ideaId}")
    public Mono<Resource> uploadFileToIdea(@PathVariable Long ideaId, @RequestPart("file") FilePart file) {
        return storage.uploadFileIdea(ideaId, file);
    }
}
