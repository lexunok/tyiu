package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.TestAnswerDTO;
import com.tyiu.ideas.model.dto.TestDTO;
import com.tyiu.ideas.model.dto.TestQuestionDTO;
import com.tyiu.ideas.model.dto.TestResultDTO;
import com.tyiu.ideas.service.TestService;
import lombok.RequiredArgsConstructor;
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

    @GetMapping("/{testName}/answers/{userId}")
    public Flux<TestAnswerDTO> getTestAnswers(@PathVariable String testName, @PathVariable String userId){
        return testService.getAnswers(testName, userId);
    }

    @GetMapping("/{testName}/result/{userId}")
    public Mono<TestResultDTO> getTestResult(@PathVariable String testName, @PathVariable String userId){
        return testService.getResult(testName, userId);
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
