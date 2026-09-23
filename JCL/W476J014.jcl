//W476J014 JOB (640W4760100W476J014,W100),'RTN W476S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P014                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W476.W476S7.W4766C(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W476.W476S7.W4766C(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.CODINVOICEINFO                                         
/*                                                                              
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W476.W476S7.W4766C(+1),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J014                                         
