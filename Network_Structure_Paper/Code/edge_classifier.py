import requests
import os
from dotenv import load_dotenv


env_path = os.path.join(os.path.dirname(os.path.abspath(__name__)), '.env')
load_dotenv(env_path)

# Retrieve the Bearer token from the .env file
bearer_token = os.getenv('BEARER_TOKEN')

# Fill in the headers with the retrieved Bearer token
headers = {"Authorization": f"Bearer {bearer_token}"}
API_URL = "https://api-inference.huggingface.co/models/facebook/bart-large-mnli"

def query(payload):
	response = requests.post(API_URL, headers=headers, json=payload)
	return response.json()

output = query({
    "inputs": "Hi, I recently bought a device from your company but it is not working as advertised and I would like to get reimbursed!",
    "parameters": {"candidate_labels": ["refund", "legal", "faq"]},
})

print('hi')
