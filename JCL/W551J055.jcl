//W551J055 JOB (650W5510100W551J055,W100),'RTN W551B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W551    EXEC W551P055                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W551.W551B1.W55155F(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W551.W551B1.W55155F(+1)                                   
//SYSIN           DD *                                                          
W55155-001                                                                      
W55155                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J055                                         
