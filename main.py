from fastapi import FastAPI
from pydantic import BaseModel
from workflows.workflow import Workflow

app = FastAPI(title="ResearchOS")


class TaskRequest(BaseModel):
    objective: str


@app.get("/")
def home():
    return {
        "project": "ResearchOS",
        "status": "running"
    }


@app.post("/execute")
def execute(request: TaskRequest):
    workflow = Workflow()
    return workflow.run(request.objective)
