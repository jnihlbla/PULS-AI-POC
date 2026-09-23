//W440J05C JOB (640W4400100W440J05C,W100),'RTN W440V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P05C                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J05C                                         
