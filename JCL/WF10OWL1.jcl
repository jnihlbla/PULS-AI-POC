//WF10OWL1 JOB (640WF100100WF10OWL1,W100),'RTN WF10D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=WF10.WF10D1.WF1004(+0),                           
//             TTLOAD=T01ONWLO,UID=WF10OWL1,JOBNAME=WF10OWL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10OWL1                                         
