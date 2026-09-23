//W121J010 JOB (650W1210100W121J010,W100),'RTN W121PV',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W121    EXEC W121P010                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W121J010                                         
