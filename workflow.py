from agents.planner import Planner
from agents.discovery_agent import DiscoveryAgent
from agents.analysis_agent import AnalysisAgent
from agents.validation_agent import ValidationAgent
from agents.review_agent import ReviewAgent


class Workflow:

    def run(self, objective):

        plan = Planner().create_plan(objective)

        discovery = DiscoveryAgent().execute(objective)

        analysis = AnalysisAgent().execute(discovery)

        validation = ValidationAgent().execute(analysis)

        review = ReviewAgent().execute(validation)

        return {
            "plan": plan,
            "discovery": discovery,
            "analysis": analysis,
            "validation": validation,
            "review": review
        }
