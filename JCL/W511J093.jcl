//W511J093 JOB (650W5110100W511J093,W100),'RTN W500V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=999,FORMS=1800,LINECT=0                                         
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W511    EXEC W511P093                                                         
//SOP     EXEC WSOPEND,PROCESS=W511J093                                         
