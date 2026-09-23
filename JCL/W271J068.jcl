//W271J068 JOB (640W2710100W271J068,W100),'RTN W271V7',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P068                                                         
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W271.W271V7.W27168                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPRECLINES"                                                   
//*    RCD 2: " ¤DAPRECLINES" WHERE 41 IS IDDC                                  
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W271.W271V7.W27168(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W271.W271V7.W27168(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J068                                         
