//W112J010 JOB (670W1120100W112J010,W100),'RTN W200D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W112    EXEC W112P010                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W112J010                                         
