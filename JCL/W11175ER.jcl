//W11175ER JOB (540W9100100W11175ER,W100),'RTN W111V5',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*    LOOK FOR REASON IN FILEMON-LOGG: F1XFVC.PROD.LOG(+0) OR (-?)  *          
//*                                                                  *          
//*   EXEMPEL:                                                       *          
//*   JOB W11175ER:  STEG W11175 HAR EJ GÅTT TILL NORMALT SLUT.      *          
//*       ÅTGÄRD: KÖR OM MOTSVARANDE STEG I JOB  W11175FI            *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W11175FI                                                                  
