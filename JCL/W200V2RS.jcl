//W200V2RS JOB (650W0010300W200V2RS,W100),'RTN W200V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W200V2                                                
//SOP     EXEC WSOPEND,PROCESS=W200V2RS                                         
