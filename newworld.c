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
#define STEER_MAGIC 1.2 /* Grrr. */

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
static double rot_speed, rot_accel, max_rot_accel;
static int ra_run, max_ra_run;

#define NaN (0.0/0.0)

static void
reset_vstate(void)
{
	vtime = here_x = here_y = dir = NaN;
	speed = rot_speed = 0;
	ra_run = 0;
	if (max_rot_accel > 10)
		max_rot_accel = 1e-3; 
}

static void
update_vstate(double new_time, double new_x, double new_y,
    double new_dir /* RADIANS */, double new_speed)
{
	double dt = NaN, new_rot_speed = NaN, new_rot_accel = NaN;

	dt = new_time - vtime;
	if (dt <= 0 && !isnan(dt)) {
		fprintf(stderr, "Nonpositive dt: %g (at %g)\r\n",
		    dt, new_time);
		return;
	}

	vtime = new_time;
	here_x = new_x;
	here_y = new_y;
	speed = new_speed;

	new_rot_speed = dirdiff(new_dir, dir) / dt;
	if (isnan(new_rot_speed))
		new_rot_speed = 0;
	new_rot_speed = 2 * new_rot_speed - rot_speed;
	new_rot_accel = fabs((new_rot_speed - rot_speed) / dt);

	if (new_rot_accel > MRA_IGN
	    && fabs(new_rot_accel - rot_accel) < MRA_EPS) {
		++ra_run;
		if (ra_run > max_ra_run) {
			max_ra_run = ra_run;
			max_rot_accel = new_rot_accel;
		}
	} else {
		ra_run = 0;
	}

	rot_accel = new_rot_accel;
	rot_speed = new_rot_speed;
	dir = new_dir;

	fprintf(stderr, "dir = %g   rs = %g   nra = %g   mra = %g\r\n",
	    dir, rot_speed, new_rot_accel, max_rot_accel);
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



/* The object database (trivial for now). */
struct object {
	double x, y, r;
	struct object *cdr;
	char type;
};

static struct object *objects;

static void
add_object(double x, double y, double rr, char type)
{
	struct object *i;
	double r = rr;

	switch (type) {
	case 'b': r += FUDGE_B; break;
	case 'c': r *= FUDGE_CX + 1;
		  r += FUDGE_C; break;
	}

	for (i = objects; i != NULL; i = i->cdr)
		if (i->x == x && i->y == y &&
		    /* No, I don't need epsilons, actually.  I hope. */
		    i->r == r && i->type == type)
			return;

	i = malloc(sizeof(struct object));
	i->x = x;
	i->y = y;
	i->r = r;
	i->type = type;
	i->cdr = objects;
	objects = i;
}


struct obj_cursor {
	int nothing;
};

static void
obj_cursor_init(struct obj_cursor* cu, double x, double y)
{
	cu = cu; x = x; y = y;
}

static void
obj_cursor_move(struct obj_cursor *cu, double x, double y)
{
	cu = cu; x = x; y = y;
}

static struct object*
obj_cursor_test(struct obj_cursor* cu, double x, double y)
{
	struct object *i;

	for (i = objects; i != NULL; i = i->cdr) {
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
cast_arc(double tdir, int8_t *pfirst_turn, double *podometer)
{
	struct object *hit = NULL;
	struct sim_state ss;
	int i, l, turn;
	int turnedp = 0;

	sim_start(&ss);
	l = ARC_LIMIT / param.max_speed / SIM_TELE;
	
	for (i = 0; i < l; ++i) {
		turn = steer(ss.dir, ss.rs, tdir);
		if (!turnedp) {
			turnedp = 1;
			*pfirst_turn = turn;
		}
		hit = sim_run(&ss, SIM_TELE, turn_to_rot_speed(turn));
		if (hit)
			break;
	}
	*podometer = ss.odometer;

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

			dir = NaN;
			if ((msg->time / 1000) < vtime) {
				rot_speed = max_rot_accel = NaN;
			}
		} break;			

		default: fprintf(stderr, "newworld.c: bad message type %d\r\n", 
		    gmsg->msg_type);
		}
		free(gmsg);
	}
}
