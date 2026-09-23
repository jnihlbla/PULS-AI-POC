//W020J104 JOB (640W0510100W020J104,W100),'RTN W020D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W020    EXEC W020P004,                                                        
//        INDUT=W020.W020D1                                                     
//W02004.W02004D2 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W020J104                                         
