//WF10J021 JOB (670WF100100WF10J021,W100),'RTN WF10R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WF10    EXEC WF10P021                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J021                                         
