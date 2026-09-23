//WB01ACD1 JOB (640WB010100WB01D1RS,W100),'RTN W012V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=WB01ACD1,                                                    
//             DSOUT=WG02.DUMP.SB1ACCE(+1)                                      
COPY TABLESPACE DWB01.SB1ACCE                                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WB01ACD1                                         
//*                                                                             
