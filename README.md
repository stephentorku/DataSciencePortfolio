# Data Science Portfolio

## Spotify Listening Trends Dashboard (RShiny)
Technologies: R, Shiny, Spotify API, spotifyr, ggplot2, dplyr, leaflet, plotly

Developed an interactive RShiny dashboard to analyze and visualize Spotify listening habits. The app integrates with the Spotify API to fetch user-specific data, including top tracks, artists, and listening trends. Key features:

- Personalized Insights: Displays top tracks, artists, and listening patterns over time.
- Interactive Visualizations: Uses plotly for dynamic charts and leaflet for geographic artist distribution.
- Reference Data: Compares user data against a public dataset of top 2023 tracks.
- User-Friendly UI: Dark-themed, responsive design with Spotify branding.

Highlights:
- Created a reactive dashboard with real-time updates (e.g., currently playing track).

## Insurance Product Prediction (Classification)
Technologies: Python, scikit-learn, pandas, Seaborn, SMOTEENN, Random Forest, Logistic Regression

Built a binary classifier to predict whether customers would purchase an insurance product, optimizing for revenue. Key steps:

- Data Preprocessing: Cleaned 100K+ records, handled outliers, and standardized features.

- Feature Engineering: Consolidated vehicle make categories using regex and domain knowledge.

- Modeling: Compared Random Forest (AUC: 0.89) and Logistic Regression (AUC: 0.85), tuning thresholds to balance false positives/negatives.

Business Impact: Achieved a leaderboard score of 17 (top 10%) by maximizing revenue 

Highlights:
- Used SMOTEENN to address class imbalance and hyperparameter tuning (GridSearchCV) for model optimization.


## Suicidal Ideation Detection (NLP Binary Classification)

https://github.com/Ph1lipXu/Machine-Learning-on-Suicide-and-Depression-Detection

Technologies: Python, TensorFlow, BERT, Word2Vec, FastText, spaCy, NLTK, scikit-learn

Built a deep learning system to classify text as suicidal or non-suicidal using Reddit posts from SuicideWatch, depression, and teenagers forums. Key components:

### Data Processing & NLP Pipeline
- Cleaned 12K posts (balanced classes) by:

- Converting emojis to text (e.g., 😞 → "disappointed") to preserve emotional context.

- Expanding contractions (e.g., "can't" → "cannot") and removing URLs/emails.

- Lemmatizing tokens (spaCy) while retaining critical stopwords (e.g., "not", "no").

- Generated embeddings using Word2Vec and FastText to capture semantic meaning.

### Modeling & Evaluation
Deep Learning Architectures:

- Bi-LSTM (Best: 92% accuracy) with Word2Vec embeddings for sequential context.

- CNN (88% accuracy) for local pattern detection.

- BERT (Fine-tuned) for state-of-the-art contextual understanding.

### Optimizations:

- Stratified train-test splits to handle class imbalance.

- Custom F1-score callback for real-time metric tracking during training.

### Key Insights
- Preserving emotional cues (emojis, punctuation like "!") improved model sensitivity.
- Bi-LSTM outperformed CNN due to better handling of long-range dependencies in text.

Impact: Potential application in mental health chatbots to flag high-risk posts for human intervention.



## Credit Card Fraud Detection
**End-to-end ML system for real-time fraud detection with class imbalance handling**

### Overview
Developed a fraud detection system that handles extreme class imbalance (0.17% fraud rate) using SMOTE and compared multiple ML models to identify the best performer for fraud detection.

### Key Highlights
- **Class Imbalance Handling**: Implemented SMOTE + Random Undersampling to address 1:577 imbalance ratio
- **Model Comparison**: Evaluated Logistic Regression, Random Forest, and XGBoost
- **Feature Engineering**: Created time-based, amount-based, and statistical aggregation features from PCA components
- **Comprehensive EDA**: Analyzed temporal patterns, transaction amounts, and feature correlations

### Technical Stack
- **ML/DS**: scikit-learn, XGBoost, pandas, numpy
- **Imbalanced Learning**: SMOTE (imbalanced-learn)
- **Visualization**: matplotlib, seaborn


### Key Learnings
1. **Accuracy is misleading** for imbalanced datasets - a model predicting all transactions as "normal" achieves 99.83% accuracy but catches 0% fraud
2. **SMOTE effectively handles** severe class imbalance when combined with undersampling
3. **Feature engineering** on anonymized PCA features still yields predictive power through statistical aggregations and interactions
4. **Proper evaluation metrics** (ROC-AUC, Precision, Recall) are essential for imbalanced classification

### Dataset
- **Source**: Kaggle Credit Card Fraud Detection Dataset
- **Size**: 284,807 transactions
- **Features**: 28 PCA-transformed features + Time + Amount
- **Challenge**: Severe class imbalance (492 frauds out of 284,807 transactions)

