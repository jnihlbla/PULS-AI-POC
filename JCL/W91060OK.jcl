//W91060OK JOB (540W9100100W91060OK,W100),'RTN W910V2',                         
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
//*    JOB W91060OK: STEG W91060 I JOB W91060FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//SOP    EXEC WSOPEND,PROCESS=W91060FI                                          
