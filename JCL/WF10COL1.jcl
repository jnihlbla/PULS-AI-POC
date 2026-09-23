//WF10COL1 JOB (640W5100100WF10COL1,W100),'RTN WF10B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W.QASE.COUNTRY(+0),                               
//             TTLOAD=T01COCLO,UID=WF10COL1,JOBNAME=WF10COL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10COL1                                         
