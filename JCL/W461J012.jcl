//W461J012 JOB (640W4610100W461J012,W100),'RTN W461D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P012,                                                        
//             INDIN=W461.W461D1,                                               
//             INDUT=W461.W461D1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461J012                                         
