//W011J468 JOB (640W0110100W011J468,W100),'RTN W011S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P068,                                                        
//             INDUT=W011.W011S1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J468                                         
