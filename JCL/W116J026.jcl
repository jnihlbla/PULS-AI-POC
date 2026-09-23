//W116J026 JOB (640W1160100W116J026,W100),'RTN W116S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* VCOM     =&VCOM                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//W116    EXEC W116P026                                                         
//*                                                                             
//W11626.SYSINPUT DD *                                                          
&COUNTRYX2                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J026                                         
