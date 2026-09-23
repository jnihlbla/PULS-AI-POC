//W428J036 JOB (640W4280100W428J036,W100),'RTN W428V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W428    EXEC W428P036                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W428.W428V1.W42836PR(+1)                                  
//SYSIN           DD *                                                          
W42836-001                                                                      
W42836                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J036                                         
