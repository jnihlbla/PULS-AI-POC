//W91050ER JOB (540W9100100W91050ER,W100),'RTN W91050',                         
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
//*   JOB W91050ER:  STEG W91050 HAR EJ GÅTT TILL NORMALT SLUT.      *          
//*       ÅTGÄRD: KÖR OM MOTSVARANDE STEG I JOB  W91050FI            *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W91050FI                                                                  
