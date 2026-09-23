//W020J107 JOB (640W0510100W020J107,W100),'RTN W020D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W020    EXEC W020P007,                                                        
//            INDIN=W020.W020D1,                                                
//            INDUT=W020.W020D1                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W020J107                                         
