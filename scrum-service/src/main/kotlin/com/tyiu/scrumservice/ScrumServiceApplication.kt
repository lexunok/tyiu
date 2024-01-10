package com.tyiu.scrumservice

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class ScrumServiceApplication

fun main(args: Array<String>) {
	runApplication<ScrumServiceApplication>(*args)
}
