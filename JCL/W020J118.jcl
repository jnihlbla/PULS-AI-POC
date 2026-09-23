//W020J118 JOB (640W0510100W020J118,W100),'RTN W020D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W020    EXEC W020P018,                                                        
//            INDUT=W020.W020D1                                                 
//SORT18.SORTIN DD DSN=W020.W020D1.W22508(0)                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W020J118                                         
