//W111J014 JOB (670W1110100W111J014,W100),'RTN W111V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W111    EXEC W111P014                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J014                                         
