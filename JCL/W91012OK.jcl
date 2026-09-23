//W91012OK JOB (540W9100100W91012OK,W100),'RTN W910D1',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> SUCCESSFULL <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFOK RECEIVED FROM FILEMON TRANSFER)                 *          
//*                                                                  *          
//*    EXEMPEL:                                                      *          
//*    JOB W91012OK: STEG W91012 I JOB W91012FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W91012FI                                         
/*                                                                              
