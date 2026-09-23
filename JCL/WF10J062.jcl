//WF10J062 JOB (640WF100100WF10J062,W100),'RTN WF10M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WF10P062 EXEC WF10P062                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=WF10J062                                            
//*                                                                             
