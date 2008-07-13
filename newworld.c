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
#define ARC_LIMIT (3 * param.max_sense)
#define FUDGE_B 0.6
#define FUDGE_C 0.1

/* Utilities go here. */
static double
dirdiff(double x, double y) 
{
	return remainder(x - y, 2*M_PI); /* C99 */
}


/* Fixed (I-message) parameters.  I'm lazy, so I'll reuse the message. */
static struct msg_init param;


/* Vehicle state, explicit and inferred. */
static int goingp;
static double vtime, here_x, here_y, dir, speed;
static double rot_speed, max_rot_accel = 1e-3; /* nonzero-but-low dummy */

static void
update_vstate(double new_time, double new_x, double new_y,
    double new_dir /* RADIANS */, double new_speed)
{
	double dt, new_rot_speed, new_rot_accel;
	
	if (goingp) {
		dt = new_time - vtime;
		if (dt <= 0) {
			fprintf(stderr, "Nonpositive dt: %g (at %g)\n",
			    dt, new_time);
			return;
		}
	}

	vtime = new_time;
	here_x = new_x;
	here_y = new_y;
	speed = new_speed;

	if (goingp) {
		new_rot_speed = dirdiff(new_dir, dir) / dt;
		new_rot_accel = fabs((new_rot_speed - rot_speed) / dt);
	
		rot_speed = new_rot_speed;
		if (new_rot_accel > max_rot_accel)
			max_rot_accel = new_rot_accel;
	}
	goingp = 1;
}



/* Steerage revised. */
static int
steer(double dir, double tdir) 
{
	double spillage;
	double ddiff;
	
	ddiff = dirdiff(tdir, dir);
	spillage = (rot_speed * rot_speed) / (2 * max_rot_accel);
	if (fabs(ddiff) < spillage)
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
		fprintf(stderr, "Bad turn: %d\n", turn);
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
	case 'c': r += FUDGE_C; break;
	}

	for (i = objects; i != NULL; i = i->cdr)
		if (i->x == x && i->y == y &&
		    /* No, I don't need epsilons, actually.  I hope. */
		    i->r == r && i->type == type)
			return;
	
	i = malloc(sizeof(struct object));
	i->x = x;
	i->y = x;
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
	l = ARC_LIMIT / SIM_TELE;
	
	for (i = 0; i < l; ++i) {
		turn = steer(ss.dir, tdir);
		if (!turnedp) {
			turnedp = 1;
			*pfirst_turn = turn;
		}
		hit = sim_run(&ss, SIM_TELE, turn_to_rot_speed(turn));
	}
	*podometer = ss.odometer;

	return hit;
}


/* More finally, glue in the I/O. */
static uint64_t cast_ctr;


static void signoff(const char* why)
{
	fprintf(stderr, "newworld.c halting on %s"
	    " after %"PRIu64" casts and %gs CPU.\n",
	    why, cast_ctr, ((double)clock())/CLOCKS_PER_SEC);
}

int main(int argc, char** argv)
{
	uint32_t msglen;
	struct msg* gmsg;

	for (;;) {
		if (fread(&msglen, 4, 1, stdin) != 4) {
			signoff("end of input");
			return 0;
		}
		gmsg = malloc(ntohl(msglen));
		
		switch(gmsg->msg_type) {
		case MSG_INIT: {
			struct msg_init *msg = (void*)gmsg;
			param = *msg;
		} break;
		case MSG_WHERE: {
			struct msg_where *msg = (void*)gmsg;
			update_vstate(msg->time, msg->x, msg->y,
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
			hit = cast_arc(msg->dir,
			    &resp.first_turn, &resp.odometer);
			resp.obj_type = hit->type;
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
		default: fprintf(stderr, "newworld.c: bad message type %d\n", 
		    gmsg->msg_type);
		}
		free(gmsg);
	}
}
