//WYRSTOP1 JOB (640W0010300WYRSTOP1,W100),'RTN WYR001',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WYRST   EXEC WYRSTOP1                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WYRSTOP1                                         
