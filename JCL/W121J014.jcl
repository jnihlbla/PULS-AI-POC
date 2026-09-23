//W121J014 JOB (670W1210100W121J014,W100),'RTN W121Q1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W121    EXEC W121P014                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W121J014                                         
