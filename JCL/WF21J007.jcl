//WF21J007 JOB (670WF210100WF21J007,W100),'RTN WF21S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//WF21    EXEC WF21P007                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF21J007                                         
