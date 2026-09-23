//W97003ER JOB (540W0000100W97003ER,W100),'RTN W970V1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*ROUTE XEQ NJERS                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMALLY  <== FILEMON-TRANSFER. V1 --------> W          *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN (KOPIERINGEN) HAR EJ GÅTT BRA!            *          
//*                                                                  *          
//*   SE ORSAKEN I USER-LOGGEN F1XFV1.PROD.ERRLOG(+0) MED "FIMBRLOG" *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W97003FI                                                                  
