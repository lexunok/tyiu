package com.tyiu.ideas.controller;

import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.ideas.model.dto.TestAnswerDTO;
import com.tyiu.ideas.model.dto.TestDTO;
import com.tyiu.ideas.model.dto.TestQuestionDTO;
import com.tyiu.ideas.model.dto.TestResultDTO;
import com.tyiu.ideas.model.enums.TestFilter;
import com.tyiu.ideas.model.responses.TestAllResponse;
import com.tyiu.ideas.service.TestService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/test")
@RequiredArgsConstructor
public class TestController {

    private final TestService testService;

    //get

    @GetMapping("/all")
    public Flux<TestDTO> getAllTest(){
        return testService.getAllTest();
    }

    @GetMapping("/general")
    public Flux<TestAllResponse> getTestGeneral(){
        return testService.getTestGeneral();
    }

    @GetMapping("/{testName}")
    public Mono<TestDTO> getTest(@PathVariable String testName){
        return testService.getTest(testName);
    }

    @GetMapping("/{testName}/questions/{moduleNumber}")
    public Flux<TestQuestionDTO> getTestQuestions(@PathVariable String testName, @PathVariable Integer moduleNumber){
        return testService.getTestQuestions(testName, moduleNumber);
    }

    @GetMapping("/{testName}/result/all")
    public Flux<TestResultDTO> getAllTestResult(@PathVariable String testName){
        return testService.getAllResult(testName);
    }

    @GetMapping("/{testName}/result/download")
    public Mono<ResponseEntity<InputStreamResource>> downloadResults(@PathVariable String testName){
        try {
            return testService.generateFile(testName).flatMap(r -> {
                HttpHeaders headers = new HttpHeaders();
                headers.add(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + testName + ".txt");
                headers.setContentType(MediaType.valueOf(MediaType.TEXT_PLAIN_VALUE + "; charset=UTF-8"));

                return Mono.just(new ResponseEntity<>(r, headers, HttpStatus.OK));
            });
        } catch (NotFoundException e) {
            return Mono.just(new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR));
        }
    }

    @GetMapping("/{testName}/result/{userId}")
    public Mono<TestResultDTO> getTestResult(@PathVariable String testName, @PathVariable String userId){
        return testService.getResult(testName, userId);
    }

    @GetMapping("/{testName}/answers/{userId}")
    public Flux<TestAnswerDTO> getTestAnswers(@PathVariable String testName, @PathVariable String userId){
        return testService.getAnswers(testName, userId);
    }

    //post

    @PostMapping("/result/belbin")
    public Mono<TestResultDTO> testBelbinResult(@AuthenticationPrincipal Jwt user, @RequestBody Flux<TestAnswerDTO> answers){
        return testService.testBelbinResult(user.getId(), answers);
    }

    @PostMapping("/result/temper")
    public Mono<TestResultDTO> testTemperResult(@AuthenticationPrincipal Jwt user, @RequestBody Flux<TestAnswerDTO> answers){
        return testService.testTemperResult(user.getId(), answers);
    }

    @PostMapping("/result/mind")
    public Mono<TestResultDTO> testMindResult(@AuthenticationPrincipal Jwt user, @RequestBody Flux<TestAnswerDTO> answers){
        return testService.testMindResult(user.getId(), answers);
    }
}
