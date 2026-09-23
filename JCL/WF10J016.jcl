//WF10J016 JOB (670WF100100WF10J016,W100),'RTN WF10R2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//WF10    EXEC WF10P016                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF10.WF10R2.WF1023(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//      EXEC WSOP,COMMAND='ACTIVATE W930S3'                                     
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=WF10.WF10R2.WF1023A(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
//      EXEC WSOP,COMMAND='ACTIVATE W930S2'                                     
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J016                                         
