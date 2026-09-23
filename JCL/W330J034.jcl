//W330J034 JOB (640W3300100W330J034,W100),'RTN W330E1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MARKET=&MARKET                                                              
//*                                                                             
//W330     EXEC W330P034,                                                       
//             INDIN=W330.W330X1&MARKET.,                                       
//             INDUT=W330.W330X1&MARKET.                                        
//*                                                                             
//SORT1.SORTIN DD *                                                             
W330X1&MARKET                                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W330J034                                         
