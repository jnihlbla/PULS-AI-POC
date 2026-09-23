//W418Z1MQ JOB (640W4180100W418Z1MQ,W100),'RTN W418D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W418.W418D2.W418MQ(+0)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W418.W418D2.W418MQ(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.DISCREPANCYFEEDBACK                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418Z1MQ                                         
