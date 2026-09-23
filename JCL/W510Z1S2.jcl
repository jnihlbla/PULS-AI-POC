//W510Z1S2 JOB (650W5100100W510Z1S2,W100),'RTN W510D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=W510.W510D5.W5106B(+0)                              
//*                                                                             
//    IF (EMPTY.T.RC NE 4) THEN                                                 
//VCOM     EXEC W016P022,VCOM=W510Z1SE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W510.W510D5.W5106B(+0),DISP=SHR                        
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510Z1S2                                         
