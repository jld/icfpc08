#include <stdint.h>

enum msg_type {
	MSG_INIT = 0,
	MSG_WHERE = 1,
	MSG_SEEN = 2,
	MSG_MARTIANS = 3,
	MSG_CAST = 4,
	MSG_HIT = 5,
	MSG_RSET = 6,
	MSG_BOULDER = 7
};

struct msg {
	uint8_t msg_type;
};

struct msg_init {
	uint8_t msg_type, pad[7];
	double x_lim, y_lim, min_sense, max_sense,
		max_speed, max_turn, max_hard_turn;
};

struct msg_where {
	uint8_t msg_type;
	int8_t turning, pad[6];
	double time, x, y, dir, speed;
};

struct msg_seen {
	uint8_t msg_type, obj_type, pad[6];
	double x, y, r;
};

struct martian {
	double x, y, dir, speed;
};

struct msg_martians {
	uint8_t msg_type, pad[3];
	uint32_t nmartians;
	struct martian martians[]; /* C99 */
};

struct msg_cast {
	uint8_t msg_type, pad[7];
	double dir, latency;
};

struct msg_hit {
	uint8_t msg_type, obj_type;
	int8_t first_turn, pad[5];
	double odometer, unsafety;
};

struct msg_boulder {
	uint8_t msg_type, pad[7];
	double time;
};
