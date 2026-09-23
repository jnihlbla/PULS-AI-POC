//W91052ER JOB (540W9100100W91052ER,W100),'RTN W91052',                         
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
//*   JOB W91052ER:  STEG W91052 HAR EJ GÅTT TILL NORMALT SLUT.      *          
//*       ÅTGÄRD: KÖR OM MOTSVARANDE STEG I JOB  W91052FI            *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP                                                             
ABEND W91052FI                                                                  
