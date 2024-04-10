package com.tyiu.ideas.util

fun List<String>.roleCheck(roles: List<String>): Boolean {
    return this.any { it in roles }
}