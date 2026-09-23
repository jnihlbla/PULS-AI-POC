//W91052OK JOB (540W9100100W91052OK,W100),'RTN W91052',                         
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
//*    JOB W91052OK: STEG W91052 I JOB W91052FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//SOP    EXEC WSOPEND,PROCESS=W91052FI                                          
