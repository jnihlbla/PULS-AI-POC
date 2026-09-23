//W231J096 JOB (640W2310100W231J096,W100),'RTN W231V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W231    EXEC W231P096                                                         
//*                                                                             
//***************************************************************               
//*  WZ14DAP3 PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                  
//*  INPUT FILE W231.W231V3.W2319A                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX " ¤DAP" AND FOLLOWED BY:                                            
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPW23196-XXX"                                                 
//*    RCD 2: " ¤DAPW23196"                                                     
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W231.W231V3.W2319A(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W231.W231V3.W2319A(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J096                                         
