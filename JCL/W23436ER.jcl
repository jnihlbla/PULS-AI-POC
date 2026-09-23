//W23436ER JOB (640W2340100W23436ER,W100),'RTN W234V1',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*   ==> ABNORMALLY  <== FILEMON-TRANSFER. VCAS --> TRUCK PARTS     *          
//*                                                                  *          
//*  FILEMON-ÖVERFÖRINGEN FRÅN VCAS TILL TRUCK PARTS HAR EJ GÅTT BRA *          
//*                                                                  *          
//*  SE ORSAKEN I USER-LOGGEN F1XFVC.PROD.ERRLOG(+0) MED "FMBLOG"    *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W23436FI                                                                  
