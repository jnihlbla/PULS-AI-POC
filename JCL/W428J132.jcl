//W428J132 JOB (640W4280100W428J132,W100),'RTN W428D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W428    EXEC W428P032,                                                        
//             INDUT=W428.W428D3                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J132                                         
