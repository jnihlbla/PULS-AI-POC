//WF10CCL1 JOB (640WF100100WF10CCL1,W100),'RTN WF10D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=WF10.WF10D1.WF1008(+0),                           
//             TTLOAD=T01CCTLO,UID=WF10CCL1,JOBNAME=WF10CCL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10CCL1                                         
