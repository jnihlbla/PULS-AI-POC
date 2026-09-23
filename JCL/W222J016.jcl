//W222J016 JOB (670W2220100W222J016,W100),'RTN W222D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W222    EXEC W222P016,                                                        
//             INDIN=W222.W222D1                                                
//*                                                                             
//W22216.W22216D1 DD DSN=&INDIN..W22214                                         
//                DD DSN=&INDIN..W22202                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J016                                         
