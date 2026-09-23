//W216J032 JOB (670W2160100W216J032,W100),'RTN W216D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W216    EXEC W216P032                                                         
//*                                                                             
//EMPTY2  EXEC WEMPTST,DSIN=W216.W216D2.W21631(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W216.W216D2.W21631(+1)                                    
//SYSIN           DD *                                                          
W21631-001                                                                      
W21631                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W216J032                                         
