//WXTRJ089 JOB (650W0001000WXTRJ089,W100),'RTN WXTRD2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP089                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WXTRJ089                                         
