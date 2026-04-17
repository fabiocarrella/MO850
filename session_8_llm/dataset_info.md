# Dataset Information: Telegram Messages

## Overview

This dataset contains a sample of Telegram messages collected from Brazilian antivaccine channels and groups, and aggregated into time bins. The data include message metadata, engagement metrics (e.g., views and forwards), media information, and topic classification variables.

Each row in the dataset corresponds to **one Telegram message**.

Students will use this dataset to explore patterns in communication, information diffusion, and engagement on Telegram.

---

## Dataset Size

- **Rows:** 64,000 messages  
- **Columns:** 24 variables  

---

## Unit of Analysis

Each row represents **one Telegram message** posted in a public channel.

---

## Variables

### Identifiers

| Variable | Description |
|----------|-------------|
| `channel_id` | Anonymized identifier for the Telegram channel |
| `message_id` | Unique identifier for each message |
| `user_id` | Anonymized identifier of the message author |

---

### Time Variables

| Variable | Description |
|----------|-------------|
| `collected_date` | Timestamp when the message was collected |
| `date` | Original timestamp (Unix format) |
| `edit_date` | Timestamp of last edit (if applicable) |
| `date_parsed` | Human-readable timestamp |
| `time_bin` | Time bin used for temporal aggregation |

---

### Message Content

| Variable | Description |
|----------|-------------|
| `text_content` | Text of the message |
| `language` | Detected language |
| `is_vaccine_related` | Indicator for vaccine-related content (1 = yes, 0 = no, NA = unknown) |

---

### Media Variables

| Variable | Description |
|----------|-------------|
| `media_type` | Type of media attached (image, video, webpage, etc.) |
| `media_title` | Title of media content |
| `media_description` | Media description |
| `media_url` | Link to media source |
| `media_path` | Local path to stored media file |

---

### Forwarding Variables

| Variable | Description |
|----------|-------------|
| `forward_from` | Source of forwarded message |
| `forward_from_n_forwards` | Number of forwards of original message |
| `forward_from_reactions` | Reactions to original message |
| `forward_from_views` | Views of original message |
| `n_forwards` | Number of times message was forwarded |

---

### Engagement Variables

| Variable | Description |
|----------|-------------|
| `views` | Number of views |
| `reactions` | Number of reactions |
| `reply_to` | ID of message being replied to |


---

## Important

This dataset is provided **for course use only**.  
Please do not redistribute the dataset outside the course.
