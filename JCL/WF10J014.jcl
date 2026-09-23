//WF10J014 JOB (670WF100100WF10J014,W100),'RTN WF10D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//WF10    EXEC WF10P014                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J014                                         
