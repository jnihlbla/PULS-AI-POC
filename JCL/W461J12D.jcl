//W461J12D JOB (640W4610100W461J12D,W100),'RTN W461V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P02D,                                                        
//             INDIN=W461.W461V1,                                               
//             INDIN2=W476.W461V1,                                              
//             INDUT=W461.W461V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461J12D                                         
