//W513J077 JOB (650W5130100W513J077,W100),'RTN W513D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W513    EXEC W513P077                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513J077                                         
