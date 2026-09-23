//W432J034 JOB (650W2110100W211J102,W100),'RTN W432V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY2  EXEC WEMPTST,DSIN=W432.W432V1.W43231(+0)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W432.W432V1.W43231(+0)                                    
//SYSIN           DD *                                                          
W43231-001                                                                      
W43231                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND EXEC WSOPEND,PROCESS=W432J034                                          
