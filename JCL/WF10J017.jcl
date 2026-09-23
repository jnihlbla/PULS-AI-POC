//WF10J017 JOB (640WF100100WF10J017,W100),'RTN WF10B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF10    EXEC WF10P017                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J017                                         
