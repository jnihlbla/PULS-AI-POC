//W91050OK JOB (540W9100100W91050OK,W100),'RTN W91050',                         
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
//*    JOB W91050OK: STEG W91050 I JOB W91050FI  OK.                 *          
//*                                                                  *          
//********************************************************************          
//SOP    EXEC WSOPEND,PROCESS=W91050FI                                          
