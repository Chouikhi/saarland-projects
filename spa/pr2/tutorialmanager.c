#include <stdio.h>
#include <stdlib.h>

struct list_el {
        struct list_el * next;
        int studentID;
};

struct bintut {
	/* Assume that for a valid bintut structure holds:
	 *	- the lengths of lists tut_A and tut_B differ by at most 1
	 *	- all stored studentIDs are pairwise different
	 *  - there are no cycles nor heap shared objects
	 */
	// tut_A: students assigned to tutorial group A, managed in a singly-linked list
	struct list_el * tut_A;
	// tut_B: students assigned to tutorial group B, managed in a singly-linked list
	struct list_el * tut_B;
};



/*
 * Creates and returns a pointer to an empty bintut structure.
 *
 * Show that:
 *	- a pointer to a valid, although empty bintut structure is returned.
 *
 */
struct bintut * createEmptyGrouping() {
	struct bintut * result	= malloc(sizeof(struct bintut));
	result->tut_A		= NULL;
	result->tut_B		= NULL;
	return result;
}

/*
 * Returns a pointer to the start of the tutorial list in which the student id studentID is present.
 * NULL is returned if no such id is found.
 *
 * Show that when grouping points to a valid bintut structure:
 * - iff a pointer to tut_A is returned, studentID is contained in that list
 * - iff a pointer to tut_B is returned, studentID is contained in that list
 *
 * What happens if a null pointer is passed to the method as its second argument? 
 * Does your analysis correctly detect this? 
 */
struct list_el * getTutorialGroupFor( int studentID, struct bintut * grouping ) {
	struct list_el * a = grouping->tut_A;
	struct list_el * b = grouping->tut_B;

	while(a != NULL && b != NULL) {
		if(a->studentID == studentID)
			return grouping->tut_A;
		if(b->studentID == studentID)
			return grouping->tut_B;
		a = a->next;
		b = b->next;
	}
	if(a && a->studentID == studentID)
		return grouping->tut_A;
	if(b && b->studentID == studentID)
		return grouping->tut_B;
	return NULL;
}

/*
 * Adds a student id to one of the tutorials managed within grouping.
 *
 * Show that when grouping points to a valid bintut structure
 * and when addStudentID terminates:
 *	- grouping is still/again a valid bintut structure
 *		* tut_A and tut_B are two segregated singly-linked lists
 *		* lengths of tut_A and tut_B differ at most by 1 (optional)
 *	- exactly one student id equal to studentID is contained in/reachable from grouping
 *
 * What happens if a null pointer is passed to the method as its second argument? 
 * Does your analysis correctly detect this?  
 */
void addStudentID( int studentID, struct bintut * grouping ) {
	struct list_el * a = grouping->tut_A;
	struct list_el * b = grouping->tut_B;

	struct list_el * aprev = NULL;
	struct list_el * bprev = NULL;

	while(a != NULL && b != NULL) {
		if(a->studentID == studentID || b->studentID == studentID)
			return;
		aprev = a;
		bprev = b;
		a = a->next;
		b = b->next;
	}
	if( a && a->studentID == studentID )
		return;
	if( b && b->studentID == studentID )
		return;

	struct list_el * sid = malloc(sizeof(struct list_el));
	sid->next = NULL;
	sid->studentID = studentID;

	if( a == NULL ) {
		if(aprev == NULL)
			grouping->tut_A = sid;
		else
			aprev->next = sid;
	}
	else if( b == NULL ) {
		if(bprev == NULL)
			grouping->tut_B = sid;
		else
			bprev->next = sid;
	}
}


