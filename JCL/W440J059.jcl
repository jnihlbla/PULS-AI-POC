//W440J059 JOB (650W4400100W440J059,W100),'RTN W440V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYST2                                                     
//*                                                                             
//W440    EXEC W440P059                                                         
//SOP     EXEC WSOPEND,PROCESS=W440J059                                         
