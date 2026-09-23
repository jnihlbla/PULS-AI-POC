//W612J032 JOB (640W6120100W612J032,W100),'RTN W612S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P032                                                         
//*                                                                             
//W61232.W61232D1 DD *                                                          
&URVAL.                                                                         
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J032                                         
