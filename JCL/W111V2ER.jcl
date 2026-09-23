//W111V2ER JOB (640W1110100W111V2ER,W100),'RTN W111V2',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE XEQ NJERS                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E RNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*    LOOK FOR REASON IN FILEMON-LOGG: F1XFVC.PROD.LOG(+0) OR (-?)  *          
//*                                                                  *          
//*   EXEMPEL:              FILE                                     *          
//*   JOB W111V2ER:  STEG R..... HAR EJ GÅTT TILL NORMALT SLUT.      *          
//*       ÅTGÄRD: KÖR OM MOTSVARANDE STEG I JOB  R.....FI            *          
//*                                                                  *          
//********************************************************************          
//SOP     EXEC WSOP,COMMAND='ACTIVATE W111V2ER'                                 
/*                                                                              
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W111V2ER                                         
/*                                                                              
