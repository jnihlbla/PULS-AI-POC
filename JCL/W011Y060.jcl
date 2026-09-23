//W011Y060 JOB (640W0110100W011Y060,W100),'RTN WYR001',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W011    EXEC W011P060                                                         
//*                                                                             
//W01160.W01160D6 DD DUMMY                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W011Y060                                         
