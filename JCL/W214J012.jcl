//W214J012 JOB (670W2140100W214J012,W100),'RTN W214D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W214    EXEC W214P012                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W214J012                                         
