//W440J070 JOB (640W4400100W440J070,W100),'RTN W440V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P070                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J070                                         
