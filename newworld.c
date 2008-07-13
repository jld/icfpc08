#include <inttypes.h>
#include <math.h>
#ifndef M_PI
# define M_PI           3.14159265358979323846  /* pi */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* for ntohl/htonl: */
#include <arpa/inet.h> 

#include "newworld.h"

/* Constants!  Conveniently at the top! */
#define SIM_STEP 0.01
#define SIM_TELE 0.1
#define ARC_LIMIT (6 * param.max_sense)
#define FUDGE_B 0.6
#define FUDGE_C 0.1
#define FUDGE_CX 0.00
#define MRA_EPS 1e-3
#define MRA_IGN 3e-3
#define STEER_MAGIC 1.2

#define NaN (0.0/0.0)

/* Utilities go here. */
static double
dirdiff(double x, double y) 
{
	return remainder(x - y, 2*M_PI); /* C99 */
}


/* Fixed (I-message) parameters.  I'm lazy, so I'll reuse the message. */
static struct msg_init param;


/* Vehicle state, explicit and inferred. */
static double vtime, here_x, here_y, dir, speed;
static double rot_speed, max_rot_accel, min_rot_jerk = 1e6;
static double tbuf[4], dbuf[4];
static int samples, cur_turn;

static void
reset_vstate(void)
{
	speed = rot_speed = 0;
	samples = 0;
}

static void
solve_poly(int n, double x[], double y[], double c[])
{
	c[0] = y[0];

	if (n > 1) {
		double xp[n-1], yp[n-1], cp[n-1]; /* C99 */
		int i;

		c[0] = y[0];
		for (i = 1; i < n; ++i) {
			xp[i-1] = x[i];
			yp[i-1] = (y[i] - y[0]) / (x[i] - x[0]);
		}
		solve_poly(n-1, xp, yp, cp);
		for (i = 1; i < n; ++i)
			c[i] = cp[i-1];
		for (i = 0; i < n - 1; ++i)
			c[i] -= cp[i] * x[0];
	}
}

static void
update_vstate(double new_time, double new_x, double new_y,
    double new_dir /* RADIANS */, double new_speed)
{
	double dt, dco[4];

	dt = new_time - vtime;
	if (dt == 0) {
		fprintf(stderr, "Duplicate telemetry (time %g)\r\n", new_time);
		return;
	} else if (dt < 0) {
		fprintf(stderr, "Negative dt %g at time %g; resetting!\r\n",
		    dt, new_time);
		reset_vstate();
		dt = new_time - vtime;
	}

	vtime = new_time;
	here_x = new_x;
	here_y = new_y;
	speed = new_speed;
	dir = dirdiff(new_dir, dir) + dir;
	
	dbuf[3] = dbuf[2];
	dbuf[2] = dbuf[1];
	dbuf[1] = dbuf[0];
	dbuf[0] = dir;
	tbuf[3] = tbuf[2] - dt;
	tbuf[2] = tbuf[1] - dt;
	tbuf[1] = tbuf[0] - dt;
	tbuf[0] = 0; /* Yes, I know. */
	++samples;

	if (samples >= 4) {
		double rot_accel, rot_jerk;

		solve_poly(4, tbuf, dbuf, dco);
		rot_speed = dco[1];
		rot_accel = fabs(2 * dco[2]);
		rot_jerk = fabs(6 * dco[3]);
		
		if (rot_accel > MRA_EPS /* && rot_jerk > MRA_EPS */
		    && (rot_jerk < min_rot_jerk - MRA_EPS
			|| (rot_accel < min_rot_jerk + MRA_EPS
			    && rot_accel > max_rot_accel - MRA_EPS))) {
			max_rot_accel = rot_accel;
			min_rot_jerk = rot_jerk;
		}

		fprintf(stderr, "mra %g   mrj %g     ra %g   rj %g\r\n",
		    max_rot_accel, min_rot_jerk, rot_accel, rot_jerk);
	}
}


/* Steerage revised. */
static int
steer(double dir, double rs, double dtarg) 
{
	double stoptime;
	double dhalt;
	double ddiff;
	
	stoptime = fabs(rot_speed) / max_rot_accel;
	dhalt = dir + stoptime * rs / 2;
	ddiff = dirdiff(dtarg, dhalt);

	if (fabs(ddiff) < STEER_MAGIC * max_rot_accel * SIM_TELE * SIM_TELE)
		return 0;

	return ddiff < 0 ? -2 : 2;
}

static double
turn_to_rot_speed(int turn)
{
	switch (turn) {
	case -2: return -param.max_hard_turn;
	case -1: return -param.max_turn;
	case 0: return 0;
	case 1: return param.max_turn;
	case 2: return param.max_hard_turn;
	default:
		fprintf(stderr, "Bad turn: %d\r\n", turn);
		return 0;
	}
}



/* The object database (static binning, but could have been quadtrees). */
struct object {
	double x, y, r;
	struct object *cdr;
	char type;
};

#define MAP_BINS 32
static struct object *objects[MAP_BINS][MAP_BINS], *allobjects;

#define X_TO_BIN(x) (((x) + param.x_lim) * MAP_BINS / (2 * param.x_lim))
#define Y_TO_BIN(y) (((y) + param.y_lim) * MAP_BINS / (2 * param.y_lim))
#define BIN_TO_XL(x) (((x) * (2 * param.x_lim) / MAP_BINS) - param.x_lim)
#define BIN_TO_YL(y) (((y) * (2 * param.y_lim) / MAP_BINS) - param.y_lim)

static int
add_object_to(double x, double y, double r, char type, struct object **chain)
{
	struct object *i;

	for (i = *chain; i != NULL; i = i->cdr)
		if (i->x == x && i->y == y &&
		    /* No, I don't need epsilons, actually.  I hope. */
		    i->r == r && i->type == type)
			return 0;

	i = malloc(sizeof(struct object));
	i->x = x;
	i->y = y;
	i->r = r;
	i->type = type;
	i->cdr = *chain;
	*chain = i;
	return 1;
}

static void
add_object(double x, double y, double rr, char type)
{
	double r = rr;

	switch (type) {
	case 'b': r += FUDGE_B; break;
	case 'c': r *= FUDGE_CX + 1;
		  r += FUDGE_C; break;
	}

	if (add_object_to(x, y, r, type, &allobjects)) {
		int xmin, xmax, ymin, ymax, ix, iy;

		xmin = X_TO_BIN(x - r);
		if (xmin < 0)
			xmin = 0;
		xmax = X_TO_BIN(x + r);
		if (xmax >= MAP_BINS)
			xmax = MAP_BINS - 1;
		ymin = Y_TO_BIN(y - r);
		if (ymin < 0)
			ymin = 0;
		ymax = Y_TO_BIN(y + r);
		if (ymax >= MAP_BINS)
			ymax = MAP_BINS - 1;
		
		fprintf(stderr, "%d-%d, %d-%d\r\n", xmin, xmax, ymin, ymax);
		for (ix = xmin; ix <= xmax; ++ix)
			for (iy = ymin; iy <= ymax; ++iy)
				add_object_to(x, y, r, type, &objects[ix][iy]);
	}			
}


struct obj_cursor {
	struct object **chain;
	double xl, xh, yl, yh;
};

static int
obj_cursor_oob(struct obj_cursor *cu)
{
	return cu->chain == NULL;
}

static void
obj_cursor_move(struct obj_cursor *cu, double x, double y)
{
	int ix, iy;

	if (cu->chain &&
	    x >= cu->xl && x < cu->xh && y >= cu->yl && y < cu->yh)
		return;

	ix = X_TO_BIN(x);
	iy = Y_TO_BIN(y);

	if (ix < 0 || ix >= MAP_BINS || iy < 0 || iy >= MAP_BINS) {
		cu->chain = NULL;
	} else {
		cu->chain = &objects[ix][iy];
		cu->xl = BIN_TO_XL(ix);
		cu->xh = BIN_TO_XL(ix + 1);
		cu->yl = BIN_TO_XL(iy);
		cu->yh = BIN_TO_XL(iy + 1);
	}
}

static void
obj_cursor_init(struct obj_cursor* cu, double x, double y)
{
	cu->chain = NULL;
	cu->xl = cu->xh = cu->yl = cu->yh = NaN;
	obj_cursor_move(cu, x, y);
}

static struct object*
obj_cursor_test(struct obj_cursor* cu, double x, double y)
{
	struct object *i;

	if (!cu->chain)
		return NULL;

	for (i = *(cu->chain); i != NULL; i = i->cdr) {
		double dx = x - i->x, dy = y - i->y;

		if (dx*dx + dy*dy <= i->r*i->r)
			return i;
	}

	return NULL;
}


/* Simulation. */
struct sim_state {
	struct obj_cursor curs;
	double x, y, dir, rs, odometer;
};

static void
sim_start(struct sim_state *ss)
{
	ss->x = here_x;
	ss->y = here_y;
	ss->dir = dir;
	ss->rs = rot_speed;
	ss->odometer = 0;
	obj_cursor_init(&ss->curs, ss->x, ss->y);
}

static struct object*
sim_run(struct sim_state *ss, double dt, double trs)
{
	struct object *hit;
	double nrs;

	for (; dt >= SIM_STEP/2; dt -= SIM_STEP) {
		hit = obj_cursor_test(&ss->curs, ss->x, ss->y);
		if (hit)
			return hit;
		
		/* 
		 * As the internals of the simulator are shrouded in
		 * mystery, I'll make something up.
		 */
		ss->x += SIM_STEP * speed * cos(ss->dir);
		ss->y += SIM_STEP * speed * sin(ss->dir);
		obj_cursor_move(&ss->curs, ss->x, ss->y);
		if (obj_cursor_oob(&ss->curs))
			return NULL; /* XXX should be explicit wall thing */

		ss->odometer += SIM_STEP * speed;
		ss->dir += SIM_STEP * ss->rs;
		
		if (trs > ss->rs) {
			nrs = ss->rs + max_rot_accel * SIM_STEP;
			if (nrs > trs)
				nrs = trs;
		} else {
			nrs = ss->rs - max_rot_accel * SIM_STEP;
			if (nrs < trs)
				nrs = trs;
		}
		ss->rs = nrs;
	}
	return NULL;
}



/* Finally, put it all together and cast. */
static struct object*
cast_arc(double tdir, double latency, int8_t *pfirst_turn, double *podometer)
{
	struct object *hit = NULL;
	struct sim_state ss;
	int i, l, turn;
	int turnedp = 0;

	sim_start(&ss);
	l = ARC_LIMIT / param.max_speed / SIM_TELE;

	if (latency > 0) {
		hit = sim_run(&ss, latency, turn_to_rot_speed(cur_turn));
		if (hit)
			goto hit;
	}

	for (i = 0; i < l; ++i) {
		turn = steer(ss.dir, ss.rs, tdir);
		if (!turnedp) {
			turnedp = 1;
			*pfirst_turn = turn;
		}
		hit = sim_run(&ss, SIM_TELE, turn_to_rot_speed(turn));
		if (hit)
			goto hit;
	}
hit:   *podometer = ss.odometer;

	return hit;
}


/* More finally, glue in the I/O. */
static uint64_t cast_ctr;


static void signoff(const char* why)
{
	fprintf(stderr, "newworld.c halting on %s"
	    " after %"PRIu64" casts and %gs CPU.\r\n",
	    why, cast_ctr, ((double)clock())/CLOCKS_PER_SEC);
}

int main(int argc, char** argv)
{
	uint32_t msglen;
	struct msg* gmsg;

	reset_vstate();
	for (;;) {
		msglen = 0xDEADBEEF;
		if (fread(&msglen, 4, 1, stdin) != 1) {
			signoff("end of input");
			return 0;
		}
		
		msglen = ntohl(msglen);
		gmsg = malloc(msglen);

		if (fread(gmsg, msglen, 1, stdin) != 1) {
			signoff("end of input");
			return 0;
		}

		switch(gmsg->msg_type) {
		case MSG_INIT: {
			struct msg_init *msg = (void*)gmsg;
			param = *msg;
		} break;
		case MSG_WHERE: {
			struct msg_where *msg = (void*)gmsg;
			update_vstate(msg->time / 1000, msg->x, msg->y,
			    msg->dir * M_PI / 180, msg->speed);
			cur_turn = msg->turning;
		} break;
		case MSG_SEEN: {
			struct msg_seen *msg = (void*)gmsg;
			add_object(msg->x, msg->y, msg->r, msg->obj_type);
		} break;
		case MSG_MARTIANS: {
			/* ignoring. */
		} break;
		case MSG_CAST: {
			struct msg_cast *msg = (void*)gmsg;
			struct msg_hit resp;
			struct object* hit;

			++cast_ctr;
			hit = cast_arc(msg->dir * M_PI / 180, 
			    msg->latency / 1000,
			    &resp.first_turn, &resp.odometer);
			if (isnan(resp.odometer))
				resp.odometer = 0.0;
			resp.obj_type = hit ? hit->type : 0;
			resp.msg_type = MSG_HIT;
			memset(&resp.pad, 0, sizeof(resp.pad));
			
			msglen = htonl(sizeof(resp));
			fwrite(&msglen, 4, 1, stdout);
			fwrite(&resp, sizeof(resp), 1, stdout);
			if (0 != fflush(stdout)) {
				signoff("output error");
				return 1;
			}
		} break;
		case MSG_RSET: {
			reset_vstate();
		} break;
		case MSG_BOULDER: {
			struct msg_boulder *msg = (void*)gmsg;

			samples = 0;
			if ((msg->time / 1000) < vtime) 
				/* do nothing */;
		} break;			

		default: fprintf(stderr, "newworld.c: bad message type %d\r\n", 
		    gmsg->msg_type);
		}
		free(gmsg);
	}
}
