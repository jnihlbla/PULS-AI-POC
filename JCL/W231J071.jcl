//W231J071 JOB (650W2310100W231J071,W100),'RTN W231V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P071                                                         
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W231.W231V2.W2317A                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX " ¤DAP" AND FOLLOWED BY:                                            
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPW23171-01A", WHERE 01A IS IDDC                              
//*    RCD 2: " ¤DAPW23171"                                                     
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W231.W231V2.W2317A(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W231.W231V2.W2317A(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J071                                         
