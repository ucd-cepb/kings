#!/usr/bin/env python3
"""
SPO (Subject-Predicate-Object) Triplet Extraction from Raw Text
Using Hugging Face Transformers

This script demonstrates multiple approaches for extracting knowledge graph triplets
from unstructured text using state-of-the-art transformer models.
"""

import json
import re
from typing import List, Dict, Tuple, Any
from transformers import (
    AutoModelForCausalLM, 
    AutoTokenizer, 
    pipeline,
    AutoConfig
)
import torch

class TripletExtractor:
    """Base class for triplet extraction models"""
    
    def __init__(self, model_name: str):
        self.model_name = model_name
        self.model = None
        self.tokenizer = None
        
    def extract(self, text: str) -> List[Dict[str, Any]]:
        """Extract triplets from raw text"""
        raise NotImplementedError

class TriplexExtractor(TripletExtractor):
    """
    SciPhi/Triplex: Specialized model for knowledge graph construction
    Best for: Customizable entity types and predicates
    """
    
    def __init__(self, model_name: str = "sciphi/triplex"):
        super().__init__(model_name)
        print(f"Loading {model_name}...")
        
        # Check if CUDA is available
        self.device = "cuda" if torch.cuda.is_available() else "cpu"
        print(f"Using device: {self.device}")
        
        try:
            self.model = AutoModelForCausalLM.from_pretrained(
                model_name, 
                trust_remote_code=True
            ).to(self.device).eval()
            
            self.tokenizer = AutoTokenizer.from_pretrained(
                model_name, 
                trust_remote_code=True
            )
            print("Triplex model loaded successfully!")
            
        except Exception as e:
            print(f"Error loading Triplex model: {e}")
            print("Note: Triplex may require specific licenses or access permissions")
    
    def extract(self, text: str, entity_types: List[str] = None, predicates: List[str] = None) -> List[Dict[str, Any]]:
        """
        Extract triplets with customizable entity types and predicates
        
        Args:
            text: Raw input text
            entity_types: List of entity types to extract (e.g., ["PERSON", "LOCATION", "ORGANIZATION"])
            predicates: List of relation types to extract (e.g., ["BORN_IN", "WORKS_FOR", "LOCATED_IN"])
        """
        if not self.model:
            return []
            
        # Default entity types and predicates
        if entity_types is None:
            entity_types = [
                "PERSON", "ORGANIZATION", "LOCATION", "DATE", 
                "PRODUCT", "EVENT", "COUNTRY", "CITY"
            ]
            
        if predicates is None:
            predicates = [
                "BORN_IN", "WORKS_FOR", "LOCATED_IN", "FOUNDED_BY", 
                "CEO_OF", "PART_OF", "HAPPENED_IN", "CREATED_BY"
            ]
        
        # Format prompt
        input_format = """Perform Named Entity Recognition (NER) and extract knowledge graph triplets from the text.
NER identifies named entities of given entity types, and triple extraction identifies relationships between entities using specified predicates.

**Entity Types:** {entity_types}
**Predicates:** {predicates}
**Text:** {text}
"""
        
        message = input_format.format(
            entity_types=json.dumps({"entity_types": entity_types}),
            predicates=json.dumps({"predicates": predicates}),
            text=text
        )
        
        messages = [{'role': 'user', 'content': message}]
        
        try:
            input_ids = self.tokenizer.apply_chat_template(
                messages, 
                add_generation_prompt=True, 
                return_tensors="pt"
            ).to(self.device)
            
            with torch.no_grad():
                output = self.model.generate(
                    input_ids=input_ids, 
                    max_length=2048,
                    temperature=0.1,
                    do_sample=True,
                    pad_token_id=self.tokenizer.eos_token_id
                )
            
            response = self.tokenizer.decode(output[0], skip_special_tokens=True)
            
            # Parse the response to extract structured triplets
            triplets = self._parse_triplex_output(response)
            return triplets
            
        except Exception as e:
            print(f"Error during extraction: {e}")
            return []
    
    def _parse_triplex_output(self, output: str) -> List[Dict[str, Any]]:
        """Parse Triplex model output into structured triplets"""
        triplets = []
        
        # Try to extract JSON from the output
        try:
            # Look for JSON-like structures in the output
            json_match = re.search(r'\[.*\]', output, re.DOTALL)
            if json_match:
                json_str = json_match.group(0)
                parsed = json.loads(json_str)
                
                for item in parsed:
                    if isinstance(item, dict) and all(key in item for key in ['subject', 'predicate', 'object']):
                        triplets.append({
                            'subject': item['subject'],
                            'predicate': item['predicate'], 
                            'object': item['object'],
                            'confidence': item.get('confidence', 1.0)
                        })
            
        except json.JSONDecodeError:
            # Fallback: try to parse line by line
            lines = output.split('\n')
            for line in lines:
                # Look for triplet patterns
                if '->' in line or '|' in line:
                    parts = re.split(r'[->|]+', line.strip())
                    if len(parts) >= 3:
                        triplets.append({
                            'subject': parts[0].strip(),
                            'predicate': parts[1].strip(),
                            'object': parts[2].strip(),
                            'confidence': 1.0
                        })
        
        return triplets

class REBELExtractor(TripletExtractor):
    """
    Babelscape/REBEL: End-to-end relation extraction for 200+ relation types
    Best for: General-purpose relation extraction with broad coverage
    """
    
    def __init__(self, model_name: str = "Babelscape/rebel-large"):
        super().__init__(model_name)
        print(f"Loading {model_name}...")
        
        try:
            self.pipeline = pipeline(
                'text2text-generation', 
                model=model_name, 
                tokenizer=model_name,
                device=0 if torch.cuda.is_available() else -1
            )
            print("REBEL model loaded successfully!")
            
        except Exception as e:
            print(f"Error loading REBEL model: {e}")
            self.pipeline = None
    
    def extract(self, text: str) -> List[Dict[str, Any]]:
        """Extract triplets using REBEL model"""
        if not self.pipeline:
            return []
            
        try:
            # Generate triplets
            generated = self.pipeline(
                text, 
                return_tensors=True, 
                return_text=False,
                max_length=512
            )
            
            # Decode the output
            decoded = self.pipeline.tokenizer.batch_decode(
                [generated[0]["generated_token_ids"]]
            )[0]
            
            # Parse the decoded output
            triplets = self._parse_rebel_output(decoded)
            return triplets
            
        except Exception as e:
            print(f"Error during REBEL extraction: {e}")
            return []
    
    def _parse_rebel_output(self, output: str) -> List[Dict[str, Any]]:
        """Parse REBEL model output into structured triplets"""
        triplets = []
        
        # Clean the output
        text = output.replace("<s>", "").replace("<pad>", "").replace("</s>", "").strip()
        
        # Parse triplets
        current_triplet = {}
        tokens = text.split()
        
        i = 0
        while i < len(tokens):
            token = tokens[i]
            
            if token == "<triplet>":
                # Start new triplet
                if current_triplet and all(k in current_triplet for k in ['head', 'type', 'tail']):
                    triplets.append({
                        'subject': current_triplet['head'],
                        'predicate': current_triplet['type'],
                        'object': current_triplet['tail'],
                        'confidence': 1.0
                    })
                current_triplet = {}
                
            elif token == "<subj>":
                # Extract subject
                i += 1
                subj_tokens = []
                while i < len(tokens) and tokens[i] not in ["<obj>", "<subj>", "<triplet>"]:
                    subj_tokens.append(tokens[i])
                    i += 1
                current_triplet['head'] = " ".join(subj_tokens).strip()
                i -= 1  # Back up one since the loop will increment
                
            elif token == "<obj>":
                # Extract object  
                i += 1
                obj_tokens = []
                while i < len(tokens) and tokens[i] not in ["<subj>", "<obj>", "<triplet>"]:
                    obj_tokens.append(tokens[i])
                    i += 1
                current_triplet['tail'] = " ".join(obj_tokens).strip()
                i -= 1
                
            else:
                # Check if this is a relation
                if not current_triplet.get('type') and token not in ["<triplet>", "<subj>", "<obj>"]:
                    current_triplet['type'] = token
            
            i += 1
        
        # Add the last triplet
        if current_triplet and all(k in current_triplet for k in ['head', 'type', 'tail']):
            triplets.append({
                'subject': current_triplet['head'],
                'predicate': current_triplet['type'], 
                'object': current_triplet['tail'],
                'confidence': 1.0
            })
        
        return triplets

class SimpleExtractor:
    """
    Simple wrapper for other HuggingFace models that might work for triplet extraction
    """
    
    def __init__(self, model_name: str = "sapienzanlp/relik-relation-extraction-nyt-large"):
        self.model_name = model_name
        print(f"Attempting to load {model_name}...")
        
        try:
            # Try to load as a general pipeline
            self.pipeline = pipeline(
                "text-generation",
                model=model_name,
                device=0 if torch.cuda.is_available() else -1
            )
            print(f"{model_name} loaded successfully!")
        except Exception as e:
            print(f"Could not load {model_name}: {e}")
            self.pipeline = None
    
    def extract(self, text: str) -> List[Dict[str, Any]]:
        """Simple extraction - may need customization based on specific model"""
        if not self.pipeline:
            return []
            
        try:
            result = self.pipeline(f"Extract triplets from: {text}")
            # This would need to be customized based on the specific model's output format
            return []
        except Exception as e:
            print(f"Error in simple extraction: {e}")
            return []

def demo_extraction():
    """Demonstrate triplet extraction with different models and texts"""
    
    # Sample texts for testing
    test_texts = [
        "Apple Inc. was founded by Steve Jobs in Cupertino, California. Tim Cook is the current CEO of Apple.",
        "Barack Obama was born in Hawaii. He served as the 44th President of the United States from 2009 to 2017.",
        "Microsoft was founded by Bill Gates and Paul Allen in 1975. The company is headquartered in Redmond, Washington.",
        "The Eiffel Tower is located in Paris, France. It was designed by Gustave Eiffel and completed in 1889."
    ]
    
    # Initialize extractors
    extractors = []
    
    # Try REBEL first (most likely to work out of the box)
    print("="*60)
    print("INITIALIZING EXTRACTORS")
    print("="*60)
    
    rebel_extractor = REBELExtractor()
    if rebel_extractor.pipeline:
        extractors.append(("REBEL", rebel_extractor))
    
    # Try Triplex (might need special access)
    triplex_extractor = TriplexExtractor()
    if triplex_extractor.model:
        extractors.append(("Triplex", triplex_extractor))
    
    # Run extraction on test texts
    print("\n" + "="*60)
    print("RUNNING TRIPLET EXTRACTION")
    print("="*60)
    
    for text in test_texts:
        print(f"\nInput Text: {text}")
        print("-" * 50)
        
        for extractor_name, extractor in extractors:
            print(f"\n{extractor_name} Results:")
            
            try:
                triplets = extractor.extract(text)
                
                if triplets:
                    for i, triplet in enumerate(triplets, 1):
                        print(f"  {i}. Subject: {triplet['subject']}")
                        print(f"     Predicate: {triplet['predicate']}")
                        print(f"     Object: {triplet['object']}")
                        if 'confidence' in triplet:
                            print(f"     Confidence: {triplet['confidence']:.2f}")
                        print()
                else:
                    print("  No triplets extracted")
                    
            except Exception as e:
                print(f"  Error: {e}")
        
        print("="*60)

def extract_from_file(file_path: str, extractor_type: str = "rebel") -> List[Dict[str, Any]]:
    """
    Extract triplets from a text file
    
    Args:
        file_path: Path to the text file
        extractor_type: Type of extractor to use ("rebel", "triplex")
    
    Returns:
        List of extracted triplets
    """
    
    # Read the file
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            text = f.read()
    except FileNotFoundError:
        print(f"File {file_path} not found")
        return []
    
    # Initialize extractor
    if extractor_type.lower() == "rebel":
        extractor = REBELExtractor()
    elif extractor_type.lower() == "triplex":
        extractor = TriplexExtractor()
    else:
        print(f"Unknown extractor type: {extractor_type}")
        return []
    
    # Split text into chunks if it's too long
    max_chunk_size = 500  # characters
    chunks = [text[i:i+max_chunk_size] for i in range(0, len(text), max_chunk_size)]
    
    all_triplets = []
    for i, chunk in enumerate(chunks):
        print(f"Processing chunk {i+1}/{len(chunks)}...")
        triplets = extractor.extract(chunk)
        all_triplets.extend(triplets)
    
    return all_triplets

if __name__ == "__main__":
    print("SPO Triplet Extraction Demo")
    print("This script demonstrates knowledge graph triplet extraction from raw text.")
    print("\nInstallation requirements:")
    print("pip install transformers torch")
    print("# For GPU support:")
    print("pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu118")
    
    # Run the demo
    demo_extraction()
    
    # Example of processing a file (uncomment to use)
    # triplets = extract_from_file("sample_text.txt", "rebel")
    # print(f"Extracted {len(triplets)} triplets from file")
