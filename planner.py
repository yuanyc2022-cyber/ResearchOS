class Planner:

    def create_plan(self, objective):
        return {
            "objective": objective,
            "stages": [
                "discovery",
                "analysis",
                "validation",
                "review"
            ]
        }
