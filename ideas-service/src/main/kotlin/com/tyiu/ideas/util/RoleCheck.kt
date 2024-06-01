package com.tyiu.ideas.util

import com.tyiu.client.models.Role

fun List<String>.roleCheck(roles: List<Role>): Boolean {
    return this.any { Role.valueOf(it) in roles }
}