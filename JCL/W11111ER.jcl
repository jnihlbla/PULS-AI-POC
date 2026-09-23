//W11111ER JOB (640W1110100W11111ER,W100),'RTN W111V1',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMALLY  <== FILEMON-TRANSFER. VCAS ------> VTP        *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN FRÅN VCAS TILL VTP HAR EJ GÅTT BRA *                 
//*                                                                  *          
//*   SE ORSAKEN I USER-LOGGEN F1XFVC.PROD.ERRLOG(+0) MED "FMBLOG"   *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W11111FI                                                                  
