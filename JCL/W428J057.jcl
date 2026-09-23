//W428J057 JOB (640W4280100W428J057,W100),'RTN W428V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W428    EXEC W428P057                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J057                                         
