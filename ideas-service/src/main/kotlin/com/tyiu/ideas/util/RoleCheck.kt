package com.tyiu.ideas.util

import com.tyiu.ideas.model.enums.Role

fun List<Role>.roleCheck(roles: List<Role>): Boolean {
    return this.any { it in roles }
}