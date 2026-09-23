//W91059OK JOB (540W9100100W91059OK,W100),'RTN W91059',                         
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
//*    EXEMPEL:                                                      *          
//*    JOB W91059OK: STEG W91059 I JOB W91059FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//SOP    EXEC WSOPEND,PROCESS=W91059FI                                          
