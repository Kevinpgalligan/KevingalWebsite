const DEBUG_DRAW_COLLISION_CIRCLES = false;

const TIMESTEPS_PER_SECOND = 30;
const MILLIS_BETWEEN_TIMESTEPS = 1000/TIMESTEPS_PER_SECOND;
const MAX_RENDER_SKIPS = 5;
const DT = MILLIS_BETWEEN_TIMESTEPS/1000.0;

const MAX_WORLD_SIZE_PIXELS = 600;
const SQUARE_WIDTH = 100;
const PIECE_WIDTH = 90;
const BOARD_SQUARES = 8;
const BOUNDARY_WIDTH = 100
const BOARD_WIDTH = BOARD_SQUARES*SQUARE_WIDTH;
const BOUNDARY_AND_BOARD_WIDTH = 2*BOUNDARY_WIDTH + BOARD_WIDTH;
const BOARD_OFFSET = BOUNDARY_WIDTH;
const WORLD_WIDTH = BOUNDARY_AND_BOARD_WIDTH

const BACKGROUND_COLOUR = "#ADD8E6";
const WHITE_COLOUR = "#F0D9B5";
const BLACK_COLOUR = "#B58863";

const FRICTION_COEFFICIENT = 0.2;
// The so-called coefficient of restitution, between 0 and 1.
// When it's 1, it's an elastic collision (all energy conserved). When
// it's 0, it's a perfectly inelastic collision.
// See: https://en.wikipedia.org/wiki/Inelastic_collision
const COLLISION_COEFFICIENT = 0.5;
const VELOCITY_FLOOR = 15;

const MAX_DRAG_DISTANCE = 200;
const MIN_DRAG_DISTANCE = 20;
const DRAG_CIRCLE_RADIUS = 15;
const DRAG_CIRCLE_COLOUR = "#7e7e7eAA";
const NUM_DRAG_CIRCLES = 3;

const TURN_INDICATOR_WIDTH = 275;
const TURN_INDICATOR_HEIGHT = 40;
const TURN_INDICATOR_TEXT_OFFSET = 10;
const TURN_INDICATOR_FONT_SIZE = 25;

// Lichess cburnett pieces encoded in base64.
// Available under the GNU Affero General Public License.
const pieceTypeToSrc = {
	"wN": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik0yMiAxMGMxMC41IDEgMTYuNSA4IDE2IDI5SDE1YzAtOSAxMC02LjUgOC0yMSIgZmlsbD0iI2ZmZiIvPjxwYXRoIGQ9Ik0yNCAxOGMuMzggMi45MS01LjU1IDcuMzctOCA5LTMgMi0yLjgyIDQuMzQtNSA0LTEuMDQyLS45NCAxLjQxLTMuMDQgMC0zLTEgMCAuMTkgMS4yMy0xIDItMSAwLTQuMDAzIDEtNC00IDAtMiA2LTEyIDYtMTJzMS44OS0xLjkgMi0zLjVjLS43My0uOTk0LS41LTItLjUtMyAxLTEgMyAyLjUgMyAyLjVoMnMuNzgtMS45OTIgMi41LTNjMSAwIDEgMyAxIDMiIGZpbGw9IiNmZmYiLz48cGF0aCBkPSJNOS41IDI1LjVhLjUuNSAwIDEgMS0xIDAgLjUuNSAwIDEgMSAxIDB6bTUuNDMzLTkuNzVhLjUgMS41IDMwIDEgMS0uODY2LS41LjUgMS41IDMwIDEgMSAuODY2LjV6IiBmaWxsPSIjMDAwIi8+PC9nPjwvc3ZnPg==",
	"wB": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxnIGZpbGw9IiNmZmYiIHN0cm9rZS1saW5lY2FwPSJidXR0Ij48cGF0aCBkPSJNOSAzNmMzLjM5LS45NyAxMC4xMS40MyAxMy41LTIgMy4zOSAyLjQzIDEwLjExIDEuMDMgMTMuNSAyIDAgMCAxLjY1LjU0IDMgMi0uNjguOTctMS42NS45OS0zIC41LTMuMzktLjk3LTEwLjExLjQ2LTEzLjUtMS0zLjM5IDEuNDYtMTAuMTEuMDMtMTMuNSAxLTEuMzU0LjQ5LTIuMzIzLjQ3LTMtLjUgMS4zNTQtMS45NCAzLTIgMy0yeiIvPjxwYXRoIGQ9Ik0xNSAzMmMyLjUgMi41IDEyLjUgMi41IDE1IDAgLjUtMS41IDAtMiAwLTIgMC0yLjUtMi41LTQtMi41LTQgNS41LTEuNSA2LTExLjUtNS0xNS41LTExIDQtMTAuNSAxNC01IDE1LjUgMCAwLTIuNSAxLjUtMi41IDQgMCAwLS41LjUgMCAyeiIvPjxwYXRoIGQ9Ik0yNSA4YTIuNSAyLjUgMCAxIDEtNSAwIDIuNSAyLjUgMCAxIDEgNSAweiIvPjwvZz48cGF0aCBkPSJNMTcuNSAyNmgxME0xNSAzMGgxNW0tNy41LTE0LjV2NU0yMCAxOGg1IiBzdHJva2UtbGluZWpvaW49Im1pdGVyIi8+PC9nPjwvc3ZnPg==",
	"bR": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik05IDM5aDI3di0zSDl2M3ptMy41LTdsMS41LTIuNWgxN2wxLjUgMi41aC0yMHptLS41IDR2LTRoMjF2NEgxMnoiIHN0cm9rZS1saW5lY2FwPSJidXR0Ii8+PHBhdGggZD0iTTE0IDI5LjV2LTEzaDE3djEzSDE0eiIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0ibWl0ZXIiLz48cGF0aCBkPSJNMTQgMTYuNUwxMSAxNGgyM2wtMyAyLjVIMTR6TTExIDE0VjloNHYyaDVWOWg1djJoNVY5aDR2NUgxMXoiIHN0cm9rZS1saW5lY2FwPSJidXR0Ii8+PHBhdGggZD0iTTEyIDM1LjVoMjFtLTIwLTRoMTltLTE4LTJoMTdtLTE3LTEzaDE3TTExIDE0aDIzIiBmaWxsPSJub25lIiBzdHJva2U9IiNlY2VjZWMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVqb2luPSJtaXRlciIvPjwvZz48L3N2Zz4=",
	"wP": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PHBhdGggZD0iTTIyLjUgOWMtMi4yMSAwLTQgMS43OS00IDQgMCAuODkuMjkgMS43MS43OCAyLjM4QzE3LjMzIDE2LjUgMTYgMTguNTkgMTYgMjFjMCAyLjAzLjk0IDMuODQgMi40MSA1LjAzLTMgMS4wNi03LjQxIDUuNTUtNy40MSAxMy40N2gyM2MwLTcuOTItNC40MS0xMi40MS03LjQxLTEzLjQ3IDEuNDctMS4xOSAyLjQxLTMgMi40MS01LjAzIDAtMi40MS0xLjMzLTQuNS0zLjI4LTUuNjIuNDktLjY3Ljc4LTEuNDkuNzgtMi4zOCAwLTIuMjEtMS43OS00LTQtNHoiIGZpbGw9IiNmZmYiIHN0cm9rZT0iIzAwMCIgc3Ryb2tlLXdpZHRoPSIxLjUiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPjwvc3ZnPg==",
	"wQ": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0iI2ZmZiIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik04IDEyYTIgMiAwIDEgMS00IDAgMiAyIDAgMSAxIDQgMHptMTYuNS00LjVhMiAyIDAgMSAxLTQgMCAyIDIgMCAxIDEgNCAwek00MSAxMmEyIDIgMCAxIDEtNCAwIDIgMiAwIDEgMSA0IDB6TTE2IDguNWEyIDIgMCAxIDEtNCAwIDIgMiAwIDEgMSA0IDB6TTMzIDlhMiAyIDAgMSAxLTQgMCAyIDIgMCAxIDEgNCAweiIvPjxwYXRoIGQ9Ik05IDI2YzguNS0xLjUgMjEtMS41IDI3IDBsMi0xMi03IDExVjExbC01LjUgMTMuNS0zLTE1LTMgMTUtNS41LTE0VjI1TDcgMTRsMiAxMnoiIHN0cm9rZS1saW5lY2FwPSJidXR0Ii8+PHBhdGggZD0iTTkgMjZjMCAyIDEuNSAyIDIuNSA0IDEgMS41IDEgMSAuNSAzLjUtMS41IDEtMS41IDIuNS0xLjUgMi41LTEuNSAxLjUuNSAyLjUuNSAyLjUgNi41IDEgMTYuNSAxIDIzIDAgMCAwIDEuNS0xIDAtMi41IDAgMCAuNS0xLjUtMS0yLjUtLjUtMi41LS41LTIgLjUtMy41IDEtMiAyLjUtMiAyLjUtNC04LjUtMS41LTE4LjUtMS41LTI3IDB6IiBzdHJva2UtbGluZWNhcD0iYnV0dCIvPjxwYXRoIGQ9Ik0xMS41IDMwYzMuNS0xIDE4LjUtMSAyMiAwTTEyIDMzLjVjNi0xIDE1LTEgMjEgMCIgZmlsbD0ibm9uZSIvPjwvZz48L3N2Zz4=",
	"bQ": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxnIHN0cm9rZT0ibm9uZSI+PGNpcmNsZSBjeD0iNiIgY3k9IjEyIiByPSIyLjc1Ii8+PGNpcmNsZSBjeD0iMTQiIGN5PSI5IiByPSIyLjc1Ii8+PGNpcmNsZSBjeD0iMjIuNSIgY3k9IjgiIHI9IjIuNzUiLz48Y2lyY2xlIGN4PSIzMSIgY3k9IjkiIHI9IjIuNzUiLz48Y2lyY2xlIGN4PSIzOSIgY3k9IjEyIiByPSIyLjc1Ii8+PC9nPjxwYXRoIGQ9Ik05IDI2YzguNS0xLjUgMjEtMS41IDI3IDBsMi41LTEyLjVMMzEgMjVsLS4zLTE0LjEtNS4yIDEzLjYtMy0xNC41LTMgMTQuNS01LjItMTMuNkwxNCAyNSA2LjUgMTMuNSA5IDI2eiIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiLz48cGF0aCBkPSJNOSAyNmMwIDIgMS41IDIgMi41IDQgMSAxLjUgMSAxIC41IDMuNS0xLjUgMS0xLjUgMi41LTEuNSAyLjUtMS41IDEuNS41IDIuNS41IDIuNSA2LjUgMSAxNi41IDEgMjMgMCAwIDAgMS41LTEgMC0yLjUgMCAwIC41LTEuNS0xLTIuNS0uNS0yLjUtLjUtMiAuNS0zLjUgMS0yIDIuNS0yIDIuNS00LTguNS0xLjUtMTguNS0xLjUtMjcgMHoiIHN0cm9rZS1saW5lY2FwPSJidXR0Ii8+PHBhdGggZD0iTTExIDM4LjVhMzUgMzUgMSAwIDAgMjMgMCIgZmlsbD0ibm9uZSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiLz48cGF0aCBkPSJNMTEgMjlhMzUgMzUgMSAwIDEgMjMgMG0tMjEuNSAyLjVoMjBtLTIxIDNhMzUgMzUgMSAwIDAgMjIgMG0tMjMgM2EzNSAzNSAxIDAgMCAyNCAwIiBmaWxsPSJub25lIiBzdHJva2U9IiNlY2VjZWMiLz48L2c+PC9zdmc+",
	"bB": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxnIGZpbGw9IiMwMDAiIHN0cm9rZS1saW5lY2FwPSJidXR0Ij48cGF0aCBkPSJNOSAzNmMzLjM5LS45NyAxMC4xMS40MyAxMy41LTIgMy4zOSAyLjQzIDEwLjExIDEuMDMgMTMuNSAyIDAgMCAxLjY1LjU0IDMgMi0uNjguOTctMS42NS45OS0zIC41LTMuMzktLjk3LTEwLjExLjQ2LTEzLjUtMS0zLjM5IDEuNDYtMTAuMTEuMDMtMTMuNSAxLTEuMzU0LjQ5LTIuMzIzLjQ3LTMtLjUgMS4zNTQtMS45NCAzLTIgMy0yeiIvPjxwYXRoIGQ9Ik0xNSAzMmMyLjUgMi41IDEyLjUgMi41IDE1IDAgLjUtMS41IDAtMiAwLTIgMC0yLjUtMi41LTQtMi41LTQgNS41LTEuNSA2LTExLjUtNS0xNS41LTExIDQtMTAuNSAxNC01IDE1LjUgMCAwLTIuNSAxLjUtMi41IDQgMCAwLS41LjUgMCAyeiIvPjxwYXRoIGQ9Ik0yNSA4YTIuNSAyLjUgMCAxIDEtNSAwIDIuNSAyLjUgMCAxIDEgNSAweiIvPjwvZz48cGF0aCBkPSJNMTcuNSAyNmgxME0xNSAzMGgxNW0tNy41LTE0LjV2NU0yMCAxOGg1IiBzdHJva2U9IiNlY2VjZWMiIHN0cm9rZS1saW5lam9pbj0ibWl0ZXIiLz48L2c+PC9zdmc+",
	"wK": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik0yMi41IDExLjYzVjZNMjAgOGg1IiBzdHJva2UtbGluZWpvaW49Im1pdGVyIi8+PHBhdGggZD0iTTIyLjUgMjVzNC41LTcuNSAzLTEwLjVjMCAwLTEtMi41LTMtMi41cy0zIDIuNS0zIDIuNWMtMS41IDMgMyAxMC41IDMgMTAuNSIgZmlsbD0iI2ZmZiIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0ibWl0ZXIiLz48cGF0aCBkPSJNMTEuNSAzN2M1LjUgMy41IDE1LjUgMy41IDIxIDB2LTdzOS00LjUgNi0xMC41Yy00LTYuNS0xMy41LTMuNS0xNiA0VjI3di0zLjVjLTMuNS03LjUtMTMtMTAuNS0xNi00LTMgNiA1IDEwIDUgMTBWMzd6IiBmaWxsPSIjZmZmIi8+PHBhdGggZD0iTTExLjUgMzBjNS41LTMgMTUuNS0zIDIxIDBtLTIxIDMuNWM1LjUtMyAxNS41LTMgMjEgMG0tMjEgMy41YzUuNS0zIDE1LjUtMyAyMSAwIi8+PC9nPjwvc3ZnPg==",
	"wR": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0iI2ZmZiIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik05IDM5aDI3di0zSDl2M3ptMy0zdi00aDIxdjRIMTJ6bS0xLTIyVjloNHYyaDVWOWg1djJoNVY5aDR2NSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiLz48cGF0aCBkPSJNMzQgMTRsLTMgM0gxNGwtMy0zIi8+PHBhdGggZD0iTTMxIDE3djEyLjVIMTRWMTciIHN0cm9rZS1saW5lY2FwPSJidXR0IiBzdHJva2UtbGluZWpvaW49Im1pdGVyIi8+PHBhdGggZD0iTTMxIDI5LjVsMS41IDIuNWgtMjBsMS41LTIuNSIvPjxwYXRoIGQ9Ik0xMSAxNGgyMyIgZmlsbD0ibm9uZSIgc3Ryb2tlLWxpbmVqb2luPSJtaXRlciIvPjwvZz48L3N2Zz4=",
	"bP": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PHBhdGggZD0iTTIyLjUgOWMtMi4yMSAwLTQgMS43OS00IDQgMCAuODkuMjkgMS43MS43OCAyLjM4QzE3LjMzIDE2LjUgMTYgMTguNTkgMTYgMjFjMCAyLjAzLjk0IDMuODQgMi40MSA1LjAzLTMgMS4wNi03LjQxIDUuNTUtNy40MSAxMy40N2gyM2MwLTcuOTItNC40MS0xMi40MS03LjQxLTEzLjQ3IDEuNDctMS4xOSAyLjQxLTMgMi40MS01LjAzIDAtMi40MS0xLjMzLTQuNS0zLjI4LTUuNjIuNDktLjY3Ljc4LTEuNDkuNzgtMi4zOCAwLTIuMjEtMS43OS00LTQtNHoiIHN0cm9rZT0iIzAwMCIgc3Ryb2tlLXdpZHRoPSIxLjUiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPjwvc3ZnPg==",
	"bK": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik0yMi41IDExLjYzVjYiIHN0cm9rZS1saW5lam9pbj0ibWl0ZXIiLz48cGF0aCBkPSJNMjIuNSAyNXM0LjUtNy41IDMtMTAuNWMwIDAtMS0yLjUtMy0yLjVzLTMgMi41LTMgMi41Yy0xLjUgMyAzIDEwLjUgMyAxMC41IiBmaWxsPSIjMDAwIiBzdHJva2UtbGluZWNhcD0iYnV0dCIgc3Ryb2tlLWxpbmVqb2luPSJtaXRlciIvPjxwYXRoIGQ9Ik0xMS41IDM3YzUuNSAzLjUgMTUuNSAzLjUgMjEgMHYtN3M5LTQuNSA2LTEwLjVjLTQtNi41LTEzLjUtMy41LTE2IDRWMjd2LTMuNWMtMy41LTcuNS0xMy0xMC41LTE2LTQtMyA2IDUgMTAgNSAxMFYzN3oiIGZpbGw9IiMwMDAiLz48cGF0aCBkPSJNMjAgOGg1IiBzdHJva2UtbGluZWpvaW49Im1pdGVyIi8+PHBhdGggZD0iTTMyIDI5LjVzOC41LTQgNi4wMy05LjY1QzM0LjE1IDE0IDI1IDE4IDIyLjUgMjQuNWwuMDEgMi4xLS4wMS0yLjFDMjAgMTggOS45MDYgMTQgNi45OTcgMTkuODVjLTIuNDk3IDUuNjUgNC44NTMgOSA0Ljg1MyA5IiBzdHJva2U9IiNlY2VjZWMiLz48cGF0aCBkPSJNMTEuNSAzMGM1LjUtMyAxNS41LTMgMjEgMG0tMjEgMy41YzUuNS0zIDE1LjUtMyAyMSAwbS0yMSAzLjVjNS41LTMgMTUuNS0zIDIxIDAiIHN0cm9rZT0iI2VjZWNlYyIvPjwvZz48L3N2Zz4=",
	"bN": "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0NSIgaGVpZ2h0PSI0NSI+PGcgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2U9IiMwMDAiIHN0cm9rZS13aWR0aD0iMS41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik0yMiAxMGMxMC41IDEgMTYuNSA4IDE2IDI5SDE1YzAtOSAxMC02LjUgOC0yMSIgZmlsbD0iIzAwMCIvPjxwYXRoIGQ9Ik0yNCAxOGMuMzggMi45MS01LjU1IDcuMzctOCA5LTMgMi0yLjgyIDQuMzQtNSA0LTEuMDQyLS45NCAxLjQxLTMuMDQgMC0zLTEgMCAuMTkgMS4yMy0xIDItMSAwLTQuMDAzIDEtNC00IDAtMiA2LTEyIDYtMTJzMS44OS0xLjkgMi0zLjVjLS43My0uOTk0LS41LTItLjUtMyAxLTEgMyAyLjUgMyAyLjVoMnMuNzgtMS45OTIgMi41LTNjMSAwIDEgMyAxIDMiIGZpbGw9IiMwMDAiLz48cGF0aCBkPSJNOS41IDI1LjVhLjUuNSAwIDEgMS0xIDAgLjUuNSAwIDEgMSAxIDB6bTUuNDMzLTkuNzVhLjUgMS41IDMwIDEgMS0uODY2LS41LjUgMS41IDMwIDEgMSAuODY2LjV6IiBmaWxsPSIjZWNlY2VjIiBzdHJva2U9IiNlY2VjZWMiLz48cGF0aCBkPSJNMjQuNTUgMTAuNGwtLjQ1IDEuNDUuNS4xNWMzLjE1IDEgNS42NSAyLjQ5IDcuOSA2Ljc1UzM1Ljc1IDI5LjA2IDM1LjI1IDM5bC0uMDUuNWgyLjI1bC4wNS0uNWMuNS0xMC4wNi0uODgtMTYuODUtMy4yNS0yMS4zNC0yLjM3LTQuNDktNS43OS02LjY0LTkuMTktNy4xNmwtLjUxLS4xeiIgZmlsbD0iI2VjZWNlYyIgc3Ryb2tlPSJub25lIi8+PC9nPjwvc3ZnPg==",
}

const pieceTypeToImage = {};

// Initialise the image for each type of piece.
Object.entries(pieceTypeToSrc).forEach(([key, value]) => {
    var img = new Image();
    img.src = value;
    pieceTypeToImage[key] = img;
});

async function waitForImage(img) {
    await getPromise(img);
}

function getPromise(img) {
    return new Promise(resolve => {
        const listener = () => {
            img.removeEventListener("load", listener);
            resolve();
        }
        img.addEventListener("load", listener);
    });
}

// Wait for the images to load so that we don't
// try to render them before they're ready.
Object.values(pieceTypeToImage).forEach(waitForImage);

const PieceColour = {Black: "b", White: "w"};
const PieceType = {Pawn: "P", Rook: "R", Bishop: "B", Knight: "N", King: "K", Queen: "Q"};

const PIECE_COLLISION_RADII = {};
PIECE_COLLISION_RADII[PieceType.Pawn] = 25;
PIECE_COLLISION_RADII[PieceType.Rook] = 25;
PIECE_COLLISION_RADII[PieceType.Bishop] = 25;
PIECE_COLLISION_RADII[PieceType.Knight] = 25;
PIECE_COLLISION_RADII[PieceType.King] = 25;
PIECE_COLLISION_RADII[PieceType.Queen] = 25;

const PIECE_MASS = {};
PIECE_MASS[PieceType.Pawn] = 2;
PIECE_MASS[PieceType.Rook] = 2.7;
PIECE_MASS[PieceType.Bishop] = 2.3;
PIECE_MASS[PieceType.Knight] = 2.3;
PIECE_MASS[PieceType.King] = 2.1;
PIECE_MASS[PieceType.Queen] = 3;

const PIECE_MAX_LAUNCH_VELOCITY = {};
PIECE_MAX_LAUNCH_VELOCITY[PieceType.Pawn] = 180;
PIECE_MAX_LAUNCH_VELOCITY[PieceType.Rook] = 300;
PIECE_MAX_LAUNCH_VELOCITY[PieceType.Bishop] = 300;
PIECE_MAX_LAUNCH_VELOCITY[PieceType.Knight] = 250;
PIECE_MAX_LAUNCH_VELOCITY[PieceType.King] = 180;
PIECE_MAX_LAUNCH_VELOCITY[PieceType.Queen] = 300;

var teamToMove = PieceColour.White;
var winningTeam = null;
var gameOver = false;
var turnInProgress = false;

var kingDead = {};
kingDead[PieceColour.White] = false;
kingDead[PieceColour.Black] = false;

class PieceInfo {
	constructor(colour, type) {
		this.colour = colour;
		this.type = type;
	}
}

class Piece {
    constructor(x, y, info) {
        this.coords = vec2d(x, y);
        this.velocity = vec2d(0.0, 0.0);
        this.info = info;
    }

	getImage() {
		return pieceTypeToImage[this.info.colour + this.info.type];
	}

	radius() {
		return PIECE_COLLISION_RADII[this.info.type];
	}

	mass() {
		return PIECE_MASS[this.info.type];
	}

    maxVelocity() {
        return PIECE_MAX_LAUNCH_VELOCITY[this.info.type];
    }
}

function piecesIntersect(b1, b2) {
    return euclideanDistance(b1.coords, b2.coords) < b1.radius() + b2.radius();
}

function pieceContains(piece, coords) {
    return euclideanDistance(piece.coords, coords) < piece.radius();
}

function pushPiecesAlongRow(pieces, yIndex, colour) {
	pieces.push(new Piece(boardIndexToCoord(0), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Rook)));
	pieces.push(new Piece(boardIndexToCoord(1), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Knight)));
	pieces.push(new Piece(boardIndexToCoord(2), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Bishop)));
	pieces.push(new Piece(boardIndexToCoord(3), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Queen)));
	pieces.push(new Piece(boardIndexToCoord(4), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.King)));
	pieces.push(new Piece(boardIndexToCoord(5), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Bishop)));
	pieces.push(new Piece(boardIndexToCoord(6), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Knight)));
	pieces.push(new Piece(boardIndexToCoord(7), boardIndexToCoord(yIndex),
				new PieceInfo(colour, PieceType.Rook)));
}

function pushPawnsAlongRow(pieces, yIndex, colour) {
	for (var xIndex = 0; xIndex < 8; xIndex += 1) {
		pieces.push(
			new Piece(boardIndexToCoord(xIndex), boardIndexToCoord(yIndex),
					  new PieceInfo(colour, PieceType.Pawn)));
	}
}

// Create pieces in their starting positions!
var pieces = [];
pushPiecesAlongRow(pieces, 7, PieceColour.Black);
pushPawnsAlongRow(pieces, 6, PieceColour.Black);
pushPawnsAlongRow(pieces, 1, PieceColour.White);
pushPiecesAlongRow(pieces, 0, PieceColour.White);

const canvas = document.getElementById("canvas");
const ctx = canvas.getContext('2d');

var pixelsPerUnitLength = 0;
var unitLengthsPerPixel = 0;

var clickPosition = null;
var targetPiece = null;

var currentMousePosition = null;

function init() {
    window.addEventListener('mousedown', function(e) {
        storeClickPosition(e);
    });
    window.addEventListener('mouseup', function(e) {
        releaseClick(e);
    });
    window.addEventListener('mousemove', function(e) {
        storeCurrentMousePosition(e);
    });
}

async function runMainLoop() {
    // Main game loop!
    // https://dewitters.com/dewitters-gameloop/
    var nextTimestep = Date.now();
    while (true) {
        var loops = 0;
        while (Date.now() > nextTimestep && loops < MAX_RENDER_SKIPS) {
            updateGameState();
            nextTimestep += MILLIS_BETWEEN_TIMESTEPS;
            loops += 1;
        }
        render();
        window.requestAnimationFrame(time => {});
        await sleep(Math.max(0, nextTimestep - Date.now()));
    }
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function getClickPosition(event) {
    const rect = canvas.getBoundingClientRect()
    return vec2d(toUnitLengths(event.clientX - rect.left),
                 yToUnitLengths(event.clientY - rect.top));
}

function storeClickPosition(event) {
    if (launchAllowed()) {
        var potentialClickPosition = getClickPosition(event);
        for (piece of pieces) {
            if (piece.info.colour == teamToMove && pieceContains(piece, potentialClickPosition)) {
                targetPiece = piece;
                clickPosition = potentialClickPosition;
                break;
            }
        }
    }
}

function launchAllowed() {
    return !gameOver && !turnInProgress;
}

function storeCurrentMousePosition(event) {
    currentMousePosition = getClickPosition(event);
}

function releaseClick(event) {
    if (targetPiece !== null && clickPosition !== null && launchAllowed()) {
        var drag = getDragVec(getClickPosition(event));
        var dragLength = vecLength(drag);
        if (dragLength > MIN_DRAG_DISTANCE) {
            // Direction.
            var d = toUnitVec(drag);
            var dragDistance = Math.min(MAX_DRAG_DISTANCE, dragLength);
            var speed = (dragDistance / MAX_DRAG_DISTANCE) * piece.maxVelocity();
            var velocityChange = scaleVec(speed, d);
            targetPiece.velocity = addVec(targetPiece.velocity, velocityChange);
            changeTeamToMove();
            turnInProgress = true;
        }
    }
    targetPiece = null;
    clickPosition = null;
}

function changeTeamToMove() {
    if (teamToMove == PieceColour.White) {
        teamToMove = PieceColour.Black;
    } else {
        teamToMove = PieceColour.White;
    }
}

function getDragVec(newPosition) {
    return subVec(clickPosition, newPosition);
}

function updateGameState() {
    var stationaryPieces = 0;
    var newCoords = [];
    pieces.forEach(piece => {
        if (isZeroVec(piece.velocity)) {
            newCoords.push(piece.coords);
            stationaryPieces += 1;
        } else {
            newCoords.push(
                addVec(
                    piece.coords,
                    scaleVec(DT, piece.velocity)));
            piece.velocity = applyFriction(piece.velocity, piece.mass());
        }
    });
    if (stationaryPieces === pieces.length) {
        turnInProgress = false;
    }

    // Check for collisions between the pieces at their
    // new positions. If a piece is in a collision, freeze
    // it in its current position.
    // Warning: doesn't handle multi-piece collisions.
    var shouldFreeze = Array(newCoords.length).fill(false);
    for (var i = 0; i < newCoords.length; i += 1) {
        for (var j = i+1; j < newCoords.length; j += 1) {
            // Pieces collide! Freeze 'em. And update their
            // velocities.
            if (euclideanDistance(newCoords[i], newCoords[j]) < pieces[i].radius() + pieces[j].radius()) {
                shouldFreeze[i] = true;
                shouldFreeze[j] = true;
                // The component of piece i's velocity in the direction
                // of piece j is transferred to j. And vice versa.
                exchangeVelocity(pieces[i], pieces[j]);
            }
        }
    }

    for (var i = 0; i < newCoords.length; i += 1) {
        if (!shouldFreeze[i]) {
            pieces[i].coords = newCoords[i];
        }
    }

    var i = 0;
    while (i < pieces.length) {
        if (outsideBoardArea(pieces[i])) {
            if (pieces[i].info.type === PieceType.King) {
                kingDead[pieces[i].info.colour] = true;
                gameOver = true;
            }
            pieces.splice(i, 1);
        } else {
            i += 1;
        }
    }
    
    if (gameOver) {
        if (kingDead[PieceColour.White]) {
            winningTeam = PieceColour.Black;
        } else {
            winningTeam = PieceColour.White;
        }
    }
}

function applyFriction(velocity, mass) {
    // Not incorporating mass just yet.
    var newVelocity  = subVec(velocity, scaleVec(DT*(1-FRICTION_COEFFICIENT), velocity));
    // Just so pieces aren't stuck at a very very small velocity that
    // will never go to zero.
    if (vecLength(newVelocity) < VELOCITY_FLOOR) {
        return vec2d(0, 0);
    }
    return newVelocity;
}

function exchangeVelocity(piece, otherPiece) {
    var c1 = piece.coords;
    var c2 = otherPiece.coords;
    var m1 = piece.mass();
    var m2 = otherPiece.mass();
    // Velocity along the line from piece to otherPiece.
    var d = direction(c1, c2);
    // Projections onto the direction vector.
    var p1 = projectOnto(piece.velocity, d);
    var p2 = projectOnto(otherPiece.velocity, d);
    // Finally, the velocity along that direction vector. Positive direction
    // is wherever the direction vector is pointing.
    var u1 = Math.sign(dotProduct(p1, d)) * vecLength(p1);
    var u2 = Math.sign(dotProduct(p2, d)) * vecLength(p2);

    // New velocities along the direction vector!
    var output = computeOutputVelocities(u1, u2, m1, m2);

    // First remove the old velocity along the direction vector, then
    // add the new velocity along that direction.
    piece.velocity = addVec(subVec(piece.velocity, p1), scaleVec(output.v1, d));
    otherPiece.velocity = addVec(subVec(otherPiece.velocity, p2), scaleVec(output.v2, d));
}

function computeOutputVelocities(u1, u2, m1, m2) {
    // See: https://en.wikipedia.org/wiki/Elastic_collision
    //      https://en.wikipedia.org/wiki/Inelastic_collision
    const C = COLLISION_COEFFICIENT;
    return {v1: (C*m2*(u2-u1) + m1*u1 + m2*u2)/(m1+m2),
            v2: (C*m1*(u1-u2) + m1*u1 + m2*u2)/(m1+m2)};
}

function outsideBoardArea(piece) {
    return piece.coords.x < BOARD_OFFSET
        || piece.coords.x > BOARD_OFFSET + BOARD_WIDTH
        || piece.coords.y < BOARD_OFFSET
        || piece.coords.y > BOARD_OFFSET + BOARD_WIDTH;
}

function render() {
    // This can change as the user resizes the window.
    canvas.width = Math.min(MAX_WORLD_SIZE_PIXELS, window.innerHeight, window.innerWidth);
    canvas.height = canvas.width;
    pixelsPerUnitLength = canvas.width/WORLD_WIDTH;
    unitLengthsPerPixel = WORLD_WIDTH/canvas.width;
    drawBackground();
    drawBoard();
    pieces.forEach(drawPiece);
    drawAimingCircles();
    drawOverlay();
}

function drawOverlay() {
    var bgColour, textColour, text;
    if (gameOver) {
        if (winningTeam === PieceColour.White) {
            bgColour = "#FFFFFF";
            textColour = "#000000";
            text = "white wins!";
        } else {
            bgColour = "#000000";
            textColour = "#FFFFFF";
            text = "black wins!";
        }
    } else if (turnInProgress) {
        bgColour = "#999999";
        textColour = "#FFFFFF";
        text = "turn in progress";
    } else if (teamToMove === PieceColour.White) {
        bgColour = "#FFFFFF";
        textColour = "#000000";
        text = "white to play";
    } else {
        bgColour = "#000000";
        textColour = "#FFFFFF";
        text = "black to play";
    }
    drawRectangle(WORLD_WIDTH-TURN_INDICATOR_WIDTH,
                  TURN_INDICATOR_HEIGHT,
                  TURN_INDICATOR_WIDTH,
                  TURN_INDICATOR_HEIGHT,
                  bgColour);
    drawText(text,
             WORLD_WIDTH-TURN_INDICATOR_WIDTH+TURN_INDICATOR_TEXT_OFFSET,
             TURN_INDICATOR_TEXT_OFFSET,
             TURN_INDICATOR_FONT_SIZE,
             textColour);
}

function drawText(text, x, y, fontSize, colour) {
    ctx.fillStyle = colour;
    ctx.font = toPixels(fontSize) + "px monospace";
    ctx.fillText(text, toPixels(x), yToPixels(y));
}

function toPixels(length) {
    return Math.ceil(length * pixelsPerUnitLength);
}

function yToPixels(y) {
    /* There needs to be a separate function for converting a y coordinate
       to the matching pixel coordinate, since the positive y direction of
       the canvas points down, while the "world" y axis points up. */
    return canvas.height - 1 - toPixels(y)
}

function toUnitLengths(pixels) {
    return unitLengthsPerPixel * pixels;
}

function yToUnitLengths(y) {
    return toUnitLengths(canvas.height-1-y);
}

function drawBackground() {
    ctx.fillStyle = BACKGROUND_COLOUR;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
}

function drawBoard() {
    fillSquares(WHITE_COLOUR, 1);
    fillSquares(BLACK_COLOUR, 0);
}

function drawPiece(piece) {
    const xPixels = toPixels(piece.coords.x);
    const yPixels = yToPixels(piece.coords.y)
	const topLeftCornerX = toPixels(piece.coords.x - PIECE_WIDTH/2);
	const topLeftCornerY = yToPixels(piece.coords.y + PIECE_WIDTH/2);
    ctx.drawImage(piece.getImage(), topLeftCornerX, topLeftCornerY,
				  toPixels(PIECE_WIDTH), toPixels(PIECE_WIDTH));
    if (DEBUG_DRAW_COLLISION_CIRCLES) {
        drawCircle(piece.coords.x, piece.coords.y, piece.radius(), "#FF0000", false);
    } else if (targetPiece !== null) {
        var colour;
        if (targetPiece === piece) {
            colour = "#00e0007e";
        } else {
            colour = "#7e00007e";
        }
        drawCircle(piece.coords.x, piece.coords.y, piece.radius(), colour, false);
    }
}

function drawCircle(x, y, radius, colour, fill) {
    ctx.beginPath();
    ctx.arc(toPixels(x), yToPixels(y), toPixels(radius), 0, 2*Math.PI, false);
    if (fill) {
        ctx.fillStyle = colour;
        ctx.fill();
    } else {
        ctx.lineWidth = 2;
        ctx.strokeStyle = colour;
        ctx.stroke();
    }
}

function fillSquares(colour, initialX) {
    // Considering bottom left square to have board coordinates (0, 0).
    // Need to convert this to "world" coordinates.
    var xIndex = initialX;
    var yIndex = 0;
    while (yIndex < BOARD_SQUARES) {
        drawRectangle(boardIndexToCoord(xIndex) - SQUARE_WIDTH/2,
                      boardIndexToCoord(yIndex+1) - SQUARE_WIDTH/2,
                      SQUARE_WIDTH,
                      SQUARE_WIDTH,
                      colour);
        xIndex += 2;
        if (xIndex >= BOARD_SQUARES) {
            if (xIndex == BOARD_SQUARES) {
                xIndex = 1;
            }
            else {
                xIndex = 0;
            }
            yIndex += 1;
        }
    }
}

function boardIndexToCoord(i) {
	return BOARD_OFFSET + i*SQUARE_WIDTH + SQUARE_WIDTH/2;
}

function drawRectangle(x, y, width, height, colour) {
    ctx.fillStyle = colour;
    ctx.fillRect(toPixels(x), yToPixels(y), toPixels(width), toPixels(height));
}

function drawAimingCircles() {
    if (targetPiece !== null && clickPosition !== null) {
        var drag = getDragVec(currentMousePosition);
        var dragLength = vecLength(drag);
        if (dragLength > 0) {
            var dragDir = toUnitVec(drag);
            for (var i = 0; i < NUM_DRAG_CIRCLES; i += 1) {
                var circleCoords = addVec(
                    targetPiece.coords,
                    scaleVec((i/(NUM_DRAG_CIRCLES-1))
                              * Math.min(MAX_DRAG_DISTANCE, dragLength),
                             dragDir));
                drawCircle(circleCoords.x, circleCoords.y,
                           DRAG_CIRCLE_RADIUS, DRAG_CIRCLE_COLOUR, true);
            }
        }
    }
}
