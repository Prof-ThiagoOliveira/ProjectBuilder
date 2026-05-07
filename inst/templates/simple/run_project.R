# Start here

# 1. Set up the project
project <- projectSetupR::setup_project()

# 2. Check project status
print(projectSetupR::project_status())

# 3. Run registered analysis steps
projectSetupR::run_project()

# 4. Render reports
projectSetupR::render_project_reports()

# Helpful examples:
# projectSetupR::new_project_object("clean_trial_data", type = "data_cleaning")
# projectSetupR::run_project_object("clean_trial_data")
