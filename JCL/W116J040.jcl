//W116J040 JOB (640W1160100W116J040,W100),'RTN W116D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P040                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J040                                         
