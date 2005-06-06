/* femfact.h */
/* This module includes the subroutines for calculting the view factors 
   for a closure where the elements see one another. The view factors 
   form a N*N matrix, if there are N side elements all together. This 
   module is largely based on the work of Juha Katajamaki (Piikiteen 
   kasvatuista simuloivan ohjelman mikrotietokonesovellus, diplomityo, 
   TKK 1993). */

int InitialInterval(Real *c1, Real *c2);
Real ViewIntegral (struct FemType *data,struct BoundaryType *bound,
		   Real c1, Real c2, int k);
int IntervalIsect(Real x1, Real x2, Real y1, Real y2, Real *z1, Real *z2);
void ExaminePoint (Real x, Real *mi, Real *ma);
Real Integrate(Real c1, Real c2);
void ViewFactors(struct FemType *data,struct BoundaryType *bound,
		 int norm);
