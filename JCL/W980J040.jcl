//W980J040   JOB (540W0090100W980J040,W100),'RTN W980D2',                       
//             USER=?,PASSWORD=?,                                               
//           CLASS=K                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W980    EXEC W980P040                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980J040                                         
