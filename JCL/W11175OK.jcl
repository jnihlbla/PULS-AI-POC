//W11175OK JOB (540W9100100W11175OK,W100),'RTN W111V5',                         
//   CLASS=K                                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> SUCCESSFULL <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFOK RECEIVED FROM FILEMON TRANSFER)                 *          
//*                                                                  *          
//*    JOB W11175OK: STEG W11175 I JOB W11175FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//SOP    EXEC WSOPEND,PROCESS=W11175FI                                          
