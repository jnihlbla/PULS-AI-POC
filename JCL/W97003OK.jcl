//W97003OK JOB (540W0000100W97003OK,W100),'RTN W970V1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*ROUTE XEQ NJERS                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> NORMALLY    <== FILEMON-TRANSFER. V1 -------> W           *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN (KOPIERINGEN) HAR GÅTT BRA!               *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W97003FI                                         
