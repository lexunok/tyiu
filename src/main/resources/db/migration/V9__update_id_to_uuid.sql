ALTER TABLE users
ADD COLUMN uuid_id TEXT;

UPDATE users
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE user_skill
ADD COLUMN user_uuid TEXT;
ALTER TABLE team_member
ADD COLUMN member_uuid TEXT;
ALTER TABLE team_market_request
ADD COLUMN owner_uuid TEXT,
ADD COLUMN leader_uuid TEXT;
ALTER TABLE team_accession
ADD COLUMN user_uuid TEXT;
ALTER TABLE team
ADD COLUMN owner_uuid TEXT,
ADD COLUMN leader_uuid TEXT;
ALTER TABLE skill
ADD COLUMN creator_uuid TEXT,
ADD COLUMN updater_uuid TEXT,
ADD COLUMN deleter_uuid TEXT;
ALTER TABLE rating
ADD COLUMN expert_uuid TEXT;
ALTER TABLE project_request
ADD COLUMN user_uuid TEXT;
ALTER TABLE project_invitation
ADD COLUMN receiver_uuid TEXT;
ALTER TABLE notification
ADD COLUMN user_uuid TEXT;
ALTER TABLE idea
ADD COLUMN initiator_uuid TEXT;
ALTER TABLE group_user
ADD COLUMN user_uuid TEXT;
ALTER TABLE favorite_idea
ADD COLUMN user_uuid TEXT;
ALTER TABLE company_user
ADD COLUMN user_uuid TEXT;
ALTER TABLE company
ADD COLUMN owner_uuid TEXT;

UPDATE user_skill
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = user_skill.user_id);
ALTER TABLE user_skill
DROP COLUMN user_id;
ALTER TABLE user_skill
RENAME COLUMN user_uuid TO user_id;

UPDATE team_member
SET member_uuid =
(SELECT uuid_id FROM users WHERE users.id = team_member.member_id);
ALTER TABLE team_member
DROP COLUMN member_id;
ALTER TABLE team_member
RENAME COLUMN member_uuid TO member_id;

UPDATE team_market_request
SET owner_uuid =
(SELECT uuid_id FROM users WHERE users.id = team_market_request.owner_id);
UPDATE team_market_request
SET leader_uuid =
(SELECT uuid_id FROM users WHERE users.id = team_market_request.leader_id);
ALTER TABLE team_market_request
DROP COLUMN owner_id;
ALTER TABLE team_market_request
DROP COLUMN leader_id;
ALTER TABLE team_market_request
RENAME COLUMN owner_uuid TO owner_id;
ALTER TABLE team_market_request
RENAME COLUMN leader_uuid TO leader_id;

UPDATE team_accession
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = team_accession.user_id);
ALTER TABLE team_accession
DROP COLUMN user_id;
ALTER TABLE team_accession
RENAME COLUMN user_uuid TO user_id;

UPDATE team
SET owner_uuid =
(SELECT uuid_id FROM users WHERE users.id = team.owner_id);
UPDATE team
SET leader_uuid =
(SELECT uuid_id FROM users WHERE users.id = team.leader_id);
ALTER TABLE team
DROP COLUMN owner_id;
ALTER TABLE team
DROP COLUMN leader_id;
ALTER TABLE team
RENAME COLUMN owner_uuid TO owner_id;
ALTER TABLE team
RENAME COLUMN leader_uuid TO leader_id;

UPDATE skill
SET creator_uuid =
(SELECT uuid_id FROM users WHERE users.id = skill.creator_id);
UPDATE skill
SET updater_uuid =
(SELECT uuid_id FROM users WHERE users.id = skill.updater_id);
UPDATE skill
SET deleter_uuid =
(SELECT uuid_id FROM users WHERE users.id = skill.deleter_id);
ALTER TABLE skill
DROP COLUMN creator_id;
ALTER TABLE skill
DROP COLUMN updater_id;
ALTER TABLE skill
DROP COLUMN deleter_id;
ALTER TABLE skill
RENAME COLUMN creator_uuid TO creator_id;
ALTER TABLE skill
RENAME COLUMN updater_uuid TO updater_id;
ALTER TABLE skill
RENAME COLUMN deleter_uuid TO deleter_id;

UPDATE rating
SET expert_uuid =
(SELECT uuid_id FROM users WHERE users.id = rating.expert_id);
ALTER TABLE rating
DROP COLUMN expert_id;
ALTER TABLE rating
RENAME COLUMN expert_uuid TO expert_id;

UPDATE project_request
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = project_request.user_id);
ALTER TABLE project_request
DROP COLUMN user_id;
ALTER TABLE project_request
RENAME COLUMN user_uuid TO user_id;

UPDATE project_invitation
SET receiver_uuid =
(SELECT uuid_id FROM users WHERE users.id = project_invitation.receiver_id);
ALTER TABLE project_invitation
DROP COLUMN receiver_id;
ALTER TABLE project_invitation
RENAME COLUMN receiver_uuid TO receiver_id;

UPDATE notification
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = notification.user_id);
ALTER TABLE notification
DROP COLUMN user_id;
ALTER TABLE notification
RENAME COLUMN user_uuid TO user_id;

UPDATE idea
SET initiator_uuid =
(SELECT uuid_id FROM users WHERE users.id = idea.initiator_id);
ALTER TABLE idea
DROP COLUMN initiator_id;
ALTER TABLE idea
RENAME COLUMN initiator_uuid TO initiator_id;

UPDATE group_user
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = group_user.user_id);
ALTER TABLE group_user
DROP COLUMN user_id;
ALTER TABLE group_user
RENAME COLUMN user_uuid TO user_id;

UPDATE favorite_idea
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = favorite_idea.user_id);
ALTER TABLE favorite_idea
DROP COLUMN user_id;
ALTER TABLE favorite_idea
RENAME COLUMN user_uuid TO user_id;

UPDATE company_user
SET user_uuid =
(SELECT uuid_id FROM users WHERE users.id = company_user.user_id);
ALTER TABLE company_user
DROP COLUMN user_id;
ALTER TABLE company_user
RENAME COLUMN user_uuid TO user_id;

UPDATE company
SET owner_uuid =
(SELECT uuid_id FROM users WHERE users.id = company.owner_id);
ALTER TABLE company
DROP COLUMN owner_id;
ALTER TABLE company
RENAME COLUMN owner_uuid TO owner_id;

ALTER TABLE users
DROP COLUMN id;
ALTER TABLE users
RENAME COLUMN uuid_id TO id;
ALTER TABLE users
ADD CONSTRAINT users_pk
PRIMARY KEY (id);
ALTER TABLE users
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;


ALTER TABLE skill
ADD COLUMN uuid_id TEXT;

UPDATE skill
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE user_skill
ADD COLUMN skill_uuid TEXT;
ALTER TABLE team_skill
ADD COLUMN skill_uuid TEXT;
ALTER TABLE idea_skill
ADD COLUMN skill_uuid TEXT;

UPDATE user_skill
SET skill_uuid =
(SELECT uuid_id FROM skill WHERE skill.id = user_skill.skill_id);
ALTER TABLE user_skill
DROP COLUMN skill_id;
ALTER TABLE user_skill
RENAME COLUMN skill_uuid TO skill_id;

UPDATE team_skill
SET skill_uuid =
(SELECT uuid_id FROM skill WHERE skill.id = team_skill.skill_id);
ALTER TABLE team_skill
DROP COLUMN skill_id;
ALTER TABLE team_skill
RENAME COLUMN skill_uuid TO skill_id;

UPDATE idea_skill
SET skill_uuid =
(SELECT uuid_id FROM skill WHERE skill.id = idea_skill.skill_id);
ALTER TABLE idea_skill
DROP COLUMN skill_id;
ALTER TABLE idea_skill
RENAME COLUMN skill_uuid TO skill_id;

ALTER TABLE skill
DROP COLUMN id;
ALTER TABLE skill
RENAME COLUMN uuid_id TO id;
ALTER TABLE skill
ADD CONSTRAINT skill_pk
PRIMARY KEY (id);
ALTER TABLE skill
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE user_skill
ADD CONSTRAINT skill_fk FOREIGN KEY (skill_id) REFERENCES skill (id) ON DELETE CASCADE;
ALTER TABLE user_skill
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;


ALTER TABLE temporary
ADD COLUMN uuid_id TEXT;

UPDATE temporary
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE temporary
DROP COLUMN id;
ALTER TABLE temporary
RENAME COLUMN uuid_id TO id;
ALTER TABLE temporary
ADD CONSTRAINT temporary_pk
PRIMARY KEY (id);
ALTER TABLE temporary
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;


ALTER TABLE team
ADD COLUMN uuid_id TEXT;

UPDATE team
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE team_skill
ADD COLUMN team_uuid TEXT;
ALTER TABLE team_member
ADD COLUMN team_uuid TEXT;
ALTER TABLE team_market_request
ADD COLUMN team_uuid TEXT;
ALTER TABLE team_accession
ADD COLUMN team_uuid TEXT;
ALTER TABLE project
ADD COLUMN team_uuid TEXT;

UPDATE team_skill
SET team_uuid =
(SELECT uuid_id FROM team WHERE team.id = team_skill.team_id);
ALTER TABLE team_skill
DROP COLUMN team_id;
ALTER TABLE team_skill
RENAME COLUMN team_uuid TO team_id;

UPDATE team_member
SET team_uuid =
(SELECT uuid_id FROM team WHERE team.id = team_member.team_id);
ALTER TABLE team_member
DROP COLUMN team_id;
ALTER TABLE team_member
RENAME COLUMN team_uuid TO team_id;

UPDATE team_market_request
SET team_uuid =
(SELECT uuid_id FROM team WHERE team.id = team_market_request.team_id);
ALTER TABLE team_market_request
DROP COLUMN team_id;
ALTER TABLE team_market_request
RENAME COLUMN team_uuid TO team_id;

UPDATE team_accession
SET team_uuid =
(SELECT uuid_id FROM team WHERE team.id = team_accession.team_id);
ALTER TABLE team_accession
DROP COLUMN team_id;
ALTER TABLE team_accession
RENAME COLUMN team_uuid TO team_id;

UPDATE project
SET team_uuid =
(SELECT uuid_id FROM team WHERE team.id = project.team_id);
ALTER TABLE project
DROP COLUMN team_id;
ALTER TABLE project
RENAME COLUMN team_uuid TO team_id;

ALTER TABLE team
DROP COLUMN id;
ALTER TABLE team
RENAME COLUMN uuid_id TO id;
ALTER TABLE team
ADD CONSTRAINT team_pk
PRIMARY KEY (id);
ALTER TABLE team
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE team_skill
ADD CONSTRAINT skill_fk FOREIGN KEY (skill_id) REFERENCES skill (id) ON DELETE CASCADE;
ALTER TABLE team_skill
ADD CONSTRAINT team_fk FOREIGN KEY (team_id) REFERENCES team (id) ON DELETE CASCADE;

ALTER TABLE team_member
ADD CONSTRAINT member_fk FOREIGN KEY (member_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE team_member
ADD CONSTRAINT team_fk FOREIGN KEY (team_id) REFERENCES team (id) ON DELETE CASCADE;


ALTER TABLE team_market_request
DROP COLUMN idea_id;

ALTER TABLE idea
ADD COLUMN uuid_id TEXT;

UPDATE idea
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE rating
ADD COLUMN idea_uuid TEXT;
ALTER TABLE idea_skill
ADD COLUMN idea_uuid TEXT;
ALTER TABLE idea_market
ADD COLUMN idea_uuid TEXT;
ALTER TABLE comment
ADD COLUMN idea_uuid TEXT;

UPDATE rating
SET idea_uuid =
(SELECT uuid_id FROM idea WHERE idea.id = rating.idea_id);
ALTER TABLE rating
DROP COLUMN idea_id;
ALTER TABLE rating
RENAME COLUMN idea_uuid TO idea_id;

UPDATE idea_skill
SET idea_uuid =
(SELECT uuid_id FROM idea WHERE idea.id = idea_skill.idea_id);
ALTER TABLE idea_skill
DROP COLUMN idea_id;
ALTER TABLE idea_skill
RENAME COLUMN idea_uuid TO idea_id;

UPDATE idea_market
SET idea_uuid =
(SELECT uuid_id FROM idea WHERE idea.id = idea_market.idea_id);
ALTER TABLE idea_market
DROP COLUMN idea_id;
ALTER TABLE idea_market
RENAME COLUMN idea_uuid TO idea_id;

UPDATE comment
SET idea_uuid =
(SELECT uuid_id FROM idea WHERE idea.id = comment.idea_id);
ALTER TABLE comment
DROP COLUMN idea_id;
ALTER TABLE comment
RENAME COLUMN idea_uuid TO idea_id;

ALTER TABLE idea
DROP COLUMN id;
ALTER TABLE idea
RENAME COLUMN uuid_id TO id;
ALTER TABLE idea
ADD CONSTRAINT idea_pk
PRIMARY KEY (id);
ALTER TABLE idea
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;


ALTER TABLE team_market_request
ADD COLUMN uuid_id TEXT;

UPDATE team_market_request
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE team_market_request
DROP COLUMN id;
ALTER TABLE team_market_request
RENAME COLUMN uuid_id TO id;
ALTER TABLE team_market_request
ADD CONSTRAINT team_market_request_pk
PRIMARY KEY (id);
ALTER TABLE team_market_request
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE team_market_request
ADD CONSTRAINT team_fk FOREIGN KEY (team_id) REFERENCES team (id) ON DELETE CASCADE;
ALTER TABLE team_market_request
ADD CONSTRAINT owner_fk FOREIGN KEY (owner_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE team_market_request
ADD CONSTRAINT leader_fk FOREIGN KEY (leader_id) REFERENCES users (id) ON DELETE CASCADE;


ALTER TABLE team_accession
ADD COLUMN uuid_id TEXT;

UPDATE team_accession
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE team_accession
DROP COLUMN id;
ALTER TABLE team_accession
RENAME COLUMN uuid_id TO id;
ALTER TABLE team_accession
ADD CONSTRAINT team_accession_pk
PRIMARY KEY (id);
ALTER TABLE team_accession
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE team_accession
ADD CONSTRAINT team_fk FOREIGN KEY (team_id) REFERENCES team (id) ON DELETE CASCADE;
ALTER TABLE team_accession
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;

ALTER TABLE team
ADD CONSTRAINT owner_fk FOREIGN KEY (owner_id) REFERENCES users (id);
ALTER TABLE team
ADD CONSTRAINT leader_fk FOREIGN KEY (leader_id) REFERENCES users (id);

ALTER TABLE skill
ADD CONSTRAINT creator_fk FOREIGN KEY (creator_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE skill
ADD CONSTRAINT updater_fk FOREIGN KEY (updater_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE skill
ADD CONSTRAINT deleter_fk FOREIGN KEY (deleter_id) REFERENCES users (id) ON DELETE CASCADE;


ALTER TABLE rating
ADD COLUMN uuid_id TEXT;

UPDATE rating
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE rating
DROP COLUMN id;
ALTER TABLE rating
RENAME COLUMN uuid_id TO id;
ALTER TABLE rating
ADD CONSTRAINT rating_pk
PRIMARY KEY (id);
ALTER TABLE rating
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE rating
ADD CONSTRAINT idea_fk FOREIGN KEY (idea_id) REFERENCES idea (id) ON DELETE CASCADE;
ALTER TABLE rating
ADD CONSTRAINT expert_fk FOREIGN KEY (expert_id) REFERENCES users (id) ON DELETE CASCADE;


ALTER TABLE project
ADD COLUMN uuid_id TEXT;

UPDATE project
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE project_request
ADD COLUMN project_uuid TEXT;
ALTER TABLE project_invitation
ADD COLUMN project_uuid TEXT;

UPDATE project_request
SET project_uuid =
(SELECT uuid_id FROM project WHERE project.id = project_request.project_id);
ALTER TABLE project_request
DROP COLUMN project_id;
ALTER TABLE project_request
RENAME COLUMN project_uuid TO project_id;

UPDATE project_invitation
SET project_uuid =
(SELECT uuid_id FROM project WHERE project.id = project_invitation.project_id);
ALTER TABLE project_invitation
DROP COLUMN project_id;
ALTER TABLE project_invitation
RENAME COLUMN project_uuid TO project_id;

ALTER TABLE project
DROP COLUMN id;
ALTER TABLE project
RENAME COLUMN uuid_id TO id;
ALTER TABLE project
ADD CONSTRAINT project_pk
PRIMARY KEY (id);
ALTER TABLE project
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;


ALTER TABLE project_request
ADD COLUMN uuid_id TEXT;

UPDATE project_request
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE project_request
DROP COLUMN id;
ALTER TABLE project_request
RENAME COLUMN uuid_id TO id;
ALTER TABLE project_request
ADD CONSTRAINT project_request_pk
PRIMARY KEY (id);
ALTER TABLE project_request
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE project_request
ADD CONSTRAINT project_fk FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE;
ALTER TABLE project_request
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;


ALTER TABLE project_invitation
ADD COLUMN uuid_id TEXT;

UPDATE project_invitation
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE project_invitation
DROP COLUMN id;
ALTER TABLE project_invitation
RENAME COLUMN uuid_id TO id;
ALTER TABLE project_invitation
ADD CONSTRAINT project_invitation_pk
PRIMARY KEY (id);
ALTER TABLE project_invitation
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE project_invitation
ADD CONSTRAINT project_fk FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE;
ALTER TABLE project_invitation
ADD CONSTRAINT receiver_fk FOREIGN KEY (receiver_id) REFERENCES users (id) ON DELETE CASCADE;

ALTER TABLE project
ADD CONSTRAINT team_fk FOREIGN KEY (team_id) REFERENCES team (id) ON DELETE CASCADE;


ALTER TABLE notification
ADD COLUMN uuid_id TEXT;

UPDATE notification
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE notification
DROP COLUMN id;
ALTER TABLE notification
RENAME COLUMN uuid_id TO id;
ALTER TABLE notification
ADD CONSTRAINT notification_pk
PRIMARY KEY (id);
ALTER TABLE notification
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE notification
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;

ALTER TABLE idea_skill
ADD CONSTRAINT idea_fk FOREIGN KEY (idea_id) REFERENCES idea (id) ON DELETE CASCADE;
ALTER TABLE idea_skill
ADD CONSTRAINT skill_fk FOREIGN KEY (skill_id) REFERENCES skill (id) ON DELETE CASCADE;


ALTER TABLE idea_market
ADD COLUMN uuid_id TEXT;

UPDATE idea_market
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE favorite_idea
ADD COLUMN idea_market_uuid TEXT;

UPDATE favorite_idea
SET idea_market_uuid =
(SELECT uuid_id FROM idea_market WHERE idea_market.id = favorite_idea.idea_market_id);
ALTER TABLE favorite_idea
DROP COLUMN idea_market_id;
ALTER TABLE favorite_idea
RENAME COLUMN idea_market_uuid TO idea_market_id;

ALTER TABLE idea_market
DROP COLUMN id;
ALTER TABLE idea_market
RENAME COLUMN uuid_id TO id;
ALTER TABLE idea_market
ADD CONSTRAINT idea_market_pk
PRIMARY KEY (id);
ALTER TABLE idea_market
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE idea_market
ADD CONSTRAINT idea_fk FOREIGN KEY (idea_id) REFERENCES idea (id) ON DELETE CASCADE;

ALTER TABLE team_market_request
ADD COLUMN idea_market_id TEXT;
ALTER TABLE team_market_request
ADD CONSTRAINT idea_market_fk FOREIGN KEY (idea_market_id) REFERENCES idea_market (id) ON DELETE CASCADE;


ALTER TABLE groups
ADD COLUMN uuid_id TEXT;

UPDATE groups
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE group_user
ADD COLUMN group_uuid TEXT;
ALTER TABLE idea
ADD COLUMN group_expert_uuid TEXT,
ADD COLUMN group_project_office_uuid TEXT;

UPDATE group_user
SET group_uuid =
(SELECT uuid_id FROM groups WHERE groups.id = group_user.group_id);
ALTER TABLE group_user
DROP COLUMN group_id;
ALTER TABLE group_user
RENAME COLUMN group_uuid TO group_id;

UPDATE idea
SET group_expert_uuid =
(SELECT uuid_id FROM groups WHERE groups.id = idea.group_expert_id);
ALTER TABLE idea
DROP COLUMN group_expert_id;
ALTER TABLE idea
RENAME COLUMN group_expert_uuid TO group_expert_id;

UPDATE idea
SET group_project_office_uuid =
(SELECT uuid_id FROM groups WHERE groups.id = idea.group_project_office_id);
ALTER TABLE idea
DROP COLUMN group_project_office_id;
ALTER TABLE idea
RENAME COLUMN group_project_office_uuid TO group_project_office_id;

ALTER TABLE groups
DROP COLUMN id;
ALTER TABLE groups
RENAME COLUMN uuid_id TO id;
ALTER TABLE groups
ADD CONSTRAINT group_pk
PRIMARY KEY (id);
ALTER TABLE groups
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE idea
ADD CONSTRAINT group_expert_fk FOREIGN KEY (group_expert_id) REFERENCES groups (id);
ALTER TABLE idea
ADD CONSTRAINT group_project_office_fk FOREIGN KEY (group_project_office_id) REFERENCES groups (id);
ALTER TABLE idea
ADD CONSTRAINT initiator_fk FOREIGN KEY (initiator_id) REFERENCES users (id);

ALTER TABLE group_user
ADD CONSTRAINT group_fk FOREIGN KEY (group_id) REFERENCES groups (id) ON DELETE CASCADE;
ALTER TABLE group_user
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;

ALTER TABLE favorite_idea
ADD CONSTRAINT idea_market_fk FOREIGN KEY (idea_market_id) REFERENCES idea_market (id) ON DELETE CASCADE;
ALTER TABLE favorite_idea
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;


ALTER TABLE comment
ADD COLUMN uuid_id TEXT;

UPDATE comment
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE comment
DROP COLUMN id;
ALTER TABLE comment
RENAME COLUMN uuid_id TO id;
ALTER TABLE comment
ADD CONSTRAINT comment_pk
PRIMARY KEY (id);
ALTER TABLE comment
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE comment
ADD CONSTRAINT idea_fk FOREIGN KEY (idea_id) REFERENCES idea (id) ON DELETE CASCADE;


ALTER TABLE company
ADD COLUMN uuid_id TEXT;

UPDATE company
SET uuid_id = gen_random_uuid()::TEXT;

ALTER TABLE company_user
ADD COLUMN company_uuid TEXT;

UPDATE company_user
SET company_uuid =
(SELECT uuid_id FROM company WHERE company.id = company_user.company_id);
ALTER TABLE company_user
DROP COLUMN company_id;
ALTER TABLE company_user
RENAME COLUMN company_uuid TO company_id;

ALTER TABLE company
DROP COLUMN id;
ALTER TABLE company
RENAME COLUMN uuid_id TO id;
ALTER TABLE company
ADD CONSTRAINT company_pk
PRIMARY KEY (id);
ALTER TABLE company
ALTER COLUMN id SET DEFAULT gen_random_uuid()::TEXT;

ALTER TABLE company_user
ADD CONSTRAINT company_fk FOREIGN KEY (company_id) REFERENCES company (id) ON DELETE CASCADE;
ALTER TABLE company_user
ADD CONSTRAINT user_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;

