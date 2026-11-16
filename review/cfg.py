"""Where a review round lives. REVIEW_DATE (e.g. 2026-11-16) names the round.

STATE (small, persistent): units.json, dims.json, prompts/, args.json,
results.json, crs.json. WORK (bulk, tmpfs): bin/ and the reviewers' scratch."""
import os, sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATE = os.environ.get('REVIEW_DATE') or sys.exit('set REVIEW_DATE=YYYY-MM-DD')
STATE = os.path.expanduser(f'~/ptmp/reviews/review-{DATE}')
WORK = os.path.expanduser(f'~/tmp/review-{DATE}')
BIN = WORK + '/bin'
REPRO = f'design/review-{DATE}/repro'
os.makedirs(STATE + '/prompts', exist_ok=True)
os.makedirs(WORK + '/work', exist_ok=True)
