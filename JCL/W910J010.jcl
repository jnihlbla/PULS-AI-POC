//W910J010 JOB (650W9100100W910J010,W100),'RTN W100V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W910    EXEC W910P010                                                         
//SOP     EXEC WSOPEND,PROCESS=W910J010                                         
