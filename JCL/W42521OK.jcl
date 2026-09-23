//W42521OK JOB (540W4250100W42521OK,W100),'RTN W425D1',                         
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
//*    ÖVERFÖRING AV FIL W42521 TILL DEST(D886) VR HAR GÅTT BRA      *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W42521FI                                         
/*                                                                              
