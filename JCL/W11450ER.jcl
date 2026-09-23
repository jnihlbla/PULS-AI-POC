//W11450ER JOB (650W1140100W11450ER,W100),'RTN W114S2',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNOWMALLY  <== FILEMON-TRANSFER. RS --------> PV TIKO    *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN FRÅN RS TILL PV HAR EJ GÅTT BRA!          *          
//*                                                                  *          
//*   SE ORSAKEN I USER-LOGGEN F1XFVC.PROD.ERRLOG(+0) MED "FMBLOG"   *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W11450FI                                                                  
