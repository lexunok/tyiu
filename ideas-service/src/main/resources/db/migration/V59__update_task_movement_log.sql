ALTER TABLE task_movement_log
DROP CONSTRAINT task_movement_log_task_id_fkey,
ADD CONSTRAINT task_movement_log_task_id_fkey FOREIGN KEY (task_id) REFERENCES task (id) ON DELETE CASCADE;