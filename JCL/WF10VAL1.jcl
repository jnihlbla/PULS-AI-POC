//WF10VAL1 JOB (640WF100100WF10VAL1,W100),'RTN WF10D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=WF10.WF10D1.WF1010(+0),                           
//             TTLOAD=T01VATLO,UID=WF10VAL1,JOBNAME=WF10VAL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10VAL1                                         
