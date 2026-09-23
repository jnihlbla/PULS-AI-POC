//WF10CUL1 JOB (640WF100100WF10CUL1,W100),'RTN WF10R2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=WF10.WF10R2.WF1013(+0),                           
//             TTLOAD=T01CURLO,UID=WF10CUL1,JOBNAME=WF10CUL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10CUL1                                         
