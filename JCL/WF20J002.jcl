//WF20J002 JOB (670WF200100WF20J002,W100),'RTN WF20S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WF20    EXEC WF20P002,DB2GRP=PROD                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF20J002                                         
