//W513J097 JOB (650W5130100W513J097,W100),'RTN W513D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W513    EXEC W513P097                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513J097                                         
