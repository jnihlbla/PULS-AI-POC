//W23355OK JOB (640W2330100W23355OK,W100),'RTN W233D1',                         
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
//*    JOB W23355OK: STEG W23355 I JOB W23355FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W23355FI                                         
/*                                                                              
